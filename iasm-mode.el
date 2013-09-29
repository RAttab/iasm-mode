;; -----------------------------------------------------------------------------
;; iasm-mode.el
;; Rémi Attab (remi.attab@gmail.com), 07 Sep 2013
;; FreeBSD-style copyright and disclaimer apply
;;
;; The idea is grab the output of objdump, format it and make it interactive.
;; Note that this file probably contains some horrible abuse and misuse of emacs
;; features that would make an elisp veteran's blood boil. Go forth at your own
;; peril.
;;
;; Todos:
;; - Shorten the edit-compile-disasm loop
;;   - Introduce compilation into the loop somehow
;;
;; - Static analyses
;;   - basic-block detection (highlight and loop detection would be nice).
;;   - Show jump edges (basic-block highlighting should work).
;;
;; - ldd
;;   - Custom syntax table plus font-lock.
;;
;; -----------------------------------------------------------------------------

(require 'cl)
(require 'avl-tree)


;; -----------------------------------------------------------------------------
;; Customization
;; -----------------------------------------------------------------------------

(defgroup iasm nil
  "Interactive assembly mode"
  :prefix "iasm-"
  :group 'tools)

(defcustom iasm-disasm-cmd "objdump"
  "Executable used to retrieve the assembly of an object file"
  :group 'iasm
  :type 'string)

(defcustom iasm-disasm-syms-args "-tCwj .text"
  "Arguments fed to the executable to retrieve symbol information"
  :group 'iasm
  :type 'string)

(defcustom iasm-disasm-insts-args "-dlCw --no-show-raw-insn"
  "Arguments fed to the executable to retrieve assembly information"
  :group 'iasm
  :type 'string)


;; -----------------------------------------------------------------------------
;; process
;; -----------------------------------------------------------------------------

(defun iasm-process-parse-buffer (fn)
  (when iasm-process-buffer
    (save-match-data
      (let ((split (split-string iasm-process-buffer "\n")))
        (setq iasm-process-buffer (car (last split)))
        (dolist (line (butlast split))
          (funcall fn line))))))


(defun iasm-process-filter (proc string filter)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (setq iasm-process-buffer (concat iasm-process-buffer string))
      (save-excursion
        (let ((inhibit-read-only t))
          (iasm-process-parse-buffer filter))))))


(defun iasm-process-sentinel (proc state filter sentinel)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (save-excursion
        (let ((inhibit-read-only t))
          (iasm-process-parse-buffer filter)
          (when sentinel (funcall sentinel))))))
  (setq iasm-loading nil)
  (iasm-buffer-do-queued-jump))


(defun iasm-process-run (file exec args filter sentinel)
  (unless iasm-loading
    (setq iasm-loading t)
    (make-variable-buffer-local 'iasm-process-buffer)
    (setq iasm-process-buffer "")
    (let ((proc (apply 'start-process "iasm-process" (current-buffer) exec args)))
      (set-process-filter proc filter)
      (set-process-sentinel proc sentinel))))


;; -----------------------------------------------------------------------------
;; index
;; -----------------------------------------------------------------------------

(defun avl-tree-lower-bound (tree data)
  "Returns the greatest element that is smaller or equal to data.
Extension to the standard avl-tree library provided by iasm-mode."
  (let ((node (avl-tree--root tree))
        (compare-function (avl-tree--cmpfun tree))
        bound found)
    (while (and node (not found))
      (assert node)
      (let ((node-data (avl-tree--node-data node)))
        (assert node-data)
        (cond
         ((funcall compare-function data node-data)
          (setq node (avl-tree--node-left node)))
         ((funcall compare-function node-data data)
          (when (or (null bound) (funcall compare-function bound node-data))
            (setq bound node-data))
          (setq node (avl-tree--node-right node)))
         (t
          (setq found t)
          (setq bound node-data)))))
    bound))

(defun avl-tree-upper-bound (tree data)
  "Returns the greatest element that is smaller or equal to data.
Extension to the standard avl-tree library provided by iasm-mode."
  (let ((node (avl-tree--root tree))
        (compare-function (avl-tree--cmpfun tree))
        bound found)
    (while (and node (not found))
      (assert node)
      (let ((node-data (avl-tree--node-data node)))
        (assert node-data)
        (cond
         ((funcall compare-function data node-data)
          (when (or (null bound) (funcall compare-function node-data bound))
            (setq bound node-data))
          (setq node (avl-tree--node-left node)))
         ((funcall compare-function node-data data)
          (setq node (avl-tree--node-right node)))
         (t
          (setq found t)
          (setq bound node-data)))))
    bound))


(defstruct iasm-index syms ctxs)

(defun iasm-index-create ()
  (make-iasm-index
   :syms (avl-tree-create 'iasm-sym-less)
   :ctxs (avl-tree-create 'iasm-ctx-less)))

(defun iasm-index-shift (index min-pos delta)
  (assert index)
  (avl-tree-map
   (lambda (sym)
     (assert sym)
     (when (< min-pos (iasm-sym-pos sym))
       (setf (iasm-sym-pos sym) (+ delta (iasm-sym-pos sym))))
     sym)
   (iasm-index-syms index)))

(defun iasm-index-empty (index)
  (or (null index) (avl-tree-empty (iasm-index-syms index))))


;; -----------------------------------------------------------------------------
;; index - sym
;; -----------------------------------------------------------------------------

(defstruct iasm-sym addr addr-size pos pos-size head-size insts name)
(defun iasm-sym-less (lhs rhs)
  (assert (iasm-sym-p lhs))
  (assert (iasm-sym-p rhs))
  (< (iasm-sym-addr lhs) (iasm-sym-addr rhs)))

(defun iasm-index-add-sym (index sym)
  (assert (and index (iasm-index-p index)))
  (setf (iasm-sym-insts sym) (avl-tree-create 'iasm-inst-less-addr))
  (avl-tree-enter (iasm-index-syms index) sym))

(defun iasm-index-find-sym (index addr)
  (assert (and index (iasm-index-p index)))
  (avl-tree-lower-bound (iasm-index-syms index) (make-iasm-sym :addr addr)))

(defun iasm-index-find-next-sym (index addr)
  (assert (and index (iasm-index-p index)))
  (avl-tree-upper-bound (iasm-index-syms index) (make-iasm-sym :addr addr)))

(defun iasm-index-test-sym (index addr)
  (assert (and index (iasm-index-p index)))
  (avl-tree-member (iasm-index-syms index) (make-iasm-sym :addr addr)))

(defun iasm-index-sym-empty (index addr)
  (assert (and index (iasm-index-p index)))
  (let ((sym (iasm-index-find-sym index addr)))
    (assert sym)
    (avl-tree-empty (iasm-sym-insts sym))))

(defun iasm-index-sym-map (index fn)
  (assert (and index (iasm-index-p index)))
  (avl-tree-map fn (iasm-index-syms index)))


;; -----------------------------------------------------------------------------
;; index - inst
;; -----------------------------------------------------------------------------

(defstruct iasm-inst addr pos target file line fn)
(defun iasm-inst-less-addr (lhs rhs)
  (assert (iasm-inst-p lhs))
  (assert (iasm-inst-p rhs))
  (< (iasm-inst-addr lhs) (iasm-inst-addr rhs)))

(defun iasm-index-add-inst (index inst)
  (assert (and index (iasm-index-p index)))
  (let ((sym (iasm-index-find-sym index (iasm-inst-addr inst))))
    (assert sym)
    ;; Relative positions means that we don't need to update it when we shift.
    (setf (iasm-inst-pos inst) (- (iasm-inst-pos inst) (iasm-sym-pos sym)))
    (avl-tree-enter (iasm-sym-insts sym) inst)))

(defun iasm-index-find-inst (index addr)
  (assert (and index (iasm-index-p index)))
  (let ((sym (iasm-index-find-sym index addr)))
    (assert sym)
    (let* ((inst-src (avl-tree-lower-bound (iasm-sym-insts sym)
                                           (make-iasm-inst :addr addr)))
           (inst (copy-iasm-inst inst-src)))
      ;; Convert relative positions into absolutes
      (setf (iasm-inst-pos inst) (+ (iasm-sym-pos sym)
                                    (iasm-inst-pos inst)))
      inst)))


;; -----------------------------------------------------------------------------
;; index - ctx
;; -----------------------------------------------------------------------------

(defstruct iasm-ctx file-line addrs)
(defun iasm-ctx-less (lhs rhs)
  (let ((lhs-file (car (iasm-ctx-file-line lhs)))
        (lhs-line (cdr (iasm-ctx-file-line lhs)))
        (rhs-file (car (iasm-ctx-file-line lhs)))
        (rhs-line (cdr (iasm-ctx-file-line lhs))))
    (if (string< lhs-file rhs-file) t
      (if (not (string= lhs-file rhs-file)) nil
        (when (< lhs-line rhs-line) t)))))

(defun iasm-index-add-ctx (index file line addr)
  (assert (and index (iasm-index-p index)))
  (let* ((key (make-iasm-ctx :file-line `(,file . ,line)))
         (index-ctx (avl-tree-member (iasm-index-ctxs index) key)))
    (if index-ctx
        (avl-tree-enter (iasm-ctx-addrs index-ctx) addr)
      (progn
        (setf (iasm-ctx-addrs key) (avl-tree-create '<))
        (avl-tree-enter (iasm-ctx-addrs key) addr)
        (avl-tree-enter (iasm-index-ctxs index) key)))))


(defun iasm-find-ctx (index file line)
  (assert (and index (iasm-index-p index)))
  (avl-tree-lower-bound (iasm-index-ctxs index)
                        (make-iasm-ctx :file-line `(,file . ,line))))

(defun iasm-find-next-ctx (index file line)
  (assert (and index (iasm-index-p index)))
  (avl-tree-upper-bound (iasm-index-ctxs index)
                        (make-iasm-ctx :file-line `(,file . ,line))))


;; -----------------------------------------------------------------------------
;; syms
;; -----------------------------------------------------------------------------

(defun iasm-syms-init ()
  (make-variable-buffer-local 'iasm-index)
  (setq iasm-index (iasm-index-create)))

(defconst iasm-syms-regex
  (concat
   "^\\([0-9a-f]+\\)" ;; address
   ".*\\.text\\s-+"   ;; .text anchors our regex
   "\\([0-9a-f]+\\)"  ;; size
   "\\s-+"
   "\\(.+\\)$"))      ;; name

(defun iasm-syms-filter (line)
  (end-of-buffer)
  (save-match-data
    (when (string-match iasm-syms-regex line)
      (let ((addr (string-to-number (match-string 1 line) 16))
            (size (string-to-number (match-string 2 line) 16))
            (name (match-string 3 line)))
        ;; objdump reports duplicate symbols which we have to dedup.
        (when (and (> size 0) (null (iasm-index-test-sym iasm-index addr)))
          (when (and iasm-queued-sym-jump (string= name iasm-queued-sym-jump))
            (setq iasm-queued-sym-jump nil)
            (setq iasm-queued-jump addr))
          (iasm-index-add-sym iasm-index (make-iasm-sym
                                          :name      name
                                          :addr      addr
                                          :addr-size size)))))))

(defun iasm-syms-insert (sym)
  (let ((pos-start (point)))
    (insert (format "%016x <%s>: \n"
                    (iasm-sym-addr sym)
                    (iasm-sym-name sym)))
    (let* ((pos-stop (point))
           (size (- pos-stop pos-start)))
      (setf (iasm-sym-pos       sym) pos-start)
      (setf (iasm-sym-pos-size  sym) size)
      (setf (iasm-sym-head-size sym) size)
      (add-text-properties pos-start pos-stop '(iasm-sym t))
      (add-text-properties pos-start pos-stop
                           `(iasm-addr ,(iasm-sym-addr sym)))))
  sym)

(defun iasm-syms-sentinel ()
  (end-of-buffer)
  (if (iasm-index-empty iasm-index) (insert "No Symbols")
    (iasm-index-sym-map iasm-index 'iasm-syms-insert)))


(defun iasm-disasm-syms-args-cons (file)
  (append (split-string iasm-disasm-syms-args " ") `(,file)))

(defun iasm-syms-run (file)
  (iasm-syms-init)
  (let ((args (iasm-disasm-syms-args-cons file)))
    (iasm-process-run
     file iasm-disasm-cmd args
     (lambda (proc string)
       (iasm-process-filter proc string 'iasm-syms-filter))
     (lambda (proc state)
       (iasm-process-sentinel
        proc state 'iasm-syms-filter 'iasm-syms-sentinel)))))


;; -----------------------------------------------------------------------------
;; insts
;; -----------------------------------------------------------------------------

(defun iasm-insts-init (addr-start addr-stop)
  (make-variable-buffer-local 'iasm-insts-sym)
  (setq iasm-insts-sym (iasm-index-find-sym iasm-index addr-start))

  (make-variable-buffer-local 'iasm-insts-ctx-file)
  (setq iasm-insts-ctx-file nil)

  (make-variable-buffer-local 'iasm-insts-ctx-line)
  (setq iasm-insts-ctx-line nil)

  (make-variable-buffer-local 'iasm-insts-ctx-fn)
  (setq iasm-insts-ctx-fn nil))


(defun iasm-insts-update-ctx (line)
  (setq iasm-insts-ctx-file (match-string 1 line))
  (setq iasm-insts-ctx-line (string-to-number (match-string 2 line))))

(defun iasm-insts-update-ctx-fn (line)
  (setq iasm-current-ctx-fn (match-string 1 line)))

(defun iasm-insts-annotate-inst (start-pos stop-pos addr target)
  (assert iasm-insts-sym)
  (iasm-index-add-inst iasm-index (make-iasm-inst
                                   :addr   addr
                                   :pos    start-pos
                                   :target target
                                   :file   iasm-insts-ctx-file
                                   :line   iasm-insts-ctx-line
                                   :fn     iasm-insts-ctx-fn))
  (when (and iasm-insts-ctx-file iasm-insts-ctx-line)
    (iasm-index-add-ctx iasm-index iasm-insts-ctx-file iasm-insts-ctx-line addr))
  (setf (iasm-sym-pos-size iasm-insts-sym)
        (+ (iasm-sym-pos-size iasm-insts-sym) (- stop-pos start-pos)))
  (add-text-properties start-pos stop-pos '(iasm-inst t))
  (add-text-properties start-pos stop-pos `(iasm-addr ,addr)))


(defconst iasm-insts-regex-inst   "^ *\\([0-9a-f]+\\):")
(defconst iasm-insts-regex-jump   "\\([0-9a-f]+\\) <.*>$")
(defconst iasm-insts-regex-ctx    "^\\(/.+\\):\\([0-9]+\\)")
(defconst iasm-insts-regex-ctx-fn "^\\(.+\\):$")


(defun iasm-insts-jump-target (line)
  (save-match-data
    (when (string-match iasm-insts-regex-jump line)
      (string-to-number (match-string 1 line) 16))))

(defun iasm-insts-insert-inst (line)
  (assert iasm-insts-sym)
  (let ((addr   (string-to-number (match-string 1 line) 16))
        (target (iasm-insts-jump-target line))
        (pos    (+ (iasm-sym-pos iasm-insts-sym)
                   (iasm-sym-pos-size iasm-insts-sym))))
    (goto-char pos)
    (insert line "\n")
    (iasm-insts-annotate-inst pos (point) addr target)))


(defun iasm-insts-filter (line)
  (save-match-data
    (if (string-match iasm-insts-regex-inst line)
        (iasm-insts-insert-inst line)
      (if (string-match iasm-insts-regex-ctx line)
          (iasm-insts-update-ctx line)
        (if (string-match iasm-insts-regex-ctx-fn line)
            (iasm-insts-update-ctx-fn line))))))

(defun iasm-insts-sentinel ()
  (assert iasm-insts-sym)
  (let ((pos (iasm-sym-pos iasm-insts-sym))
        (delta (- (iasm-sym-pos-size iasm-insts-sym)
                  (iasm-sym-head-size iasm-insts-sym))))
    (iasm-index-shift iasm-index (+ pos 1) delta))

  (makunbound 'iasm-insts-sym)
  (makunbound 'iasm-insts-ctx-file)
  (makunbound 'iasm-insts-ctx-line)
  (makunbound 'iasm-insts-ctx-fn))


(defun iasm-disasm-insts-args-cons (file start stop)
  (append
   (split-string iasm-disasm-insts-args " ")
   `(,(format "--start-address=0x%x" start))
   `(,(format "--stop-address=0x%x" stop))
   `(,file)))

(defun iasm-insts-run (file start stop)
  (assert (< start stop))
  (iasm-insts-init start stop)
  (let ((args (iasm-disasm-insts-args-cons file start stop)))
    (iasm-process-run
     file iasm-disasm-cmd args
     (lambda (proc string)
       (iasm-process-filter proc string 'iasm-insts-filter))
     (lambda (proc state)
       (iasm-process-sentinel
        proc state 'iasm-insts-filter 'iasm-insts-sentinel)))))


;; -----------------------------------------------------------------------------
;; buffer
;; -----------------------------------------------------------------------------

(defun iasm-buffer-name (file)
  (concat "*iasm " (file-name-nondirectory file) "*"))


(defun iasm-buffer-init (file)
  (make-variable-buffer-local 'iasm-file)
  (setq iasm-file file)

  (make-variable-buffer-local 'iasm-file-last-modified)
  (setq iasm-file-last-modified (nth 5 (file-attributes file)))

  (make-variable-buffer-local 'iasm-loading)
  (setq iasm-loading nil)

  (make-variable-buffer-local 'iasm-queued-jump)
  (setq iasm-queued-jump nil)

  (make-variable-buffer-local 'iasm-queued-sym-jump)
  (setq iasm-queued-sym-jump nil)

  (toggle-truncate-lines t)
  (setq buffer-read-only t)

  (erase-buffer)
  (insert (format "file:  %s\n" file))
  (insert (format
           "syms:  %s %s\n" iasm-disasm-cmd
           (mapconcat 'identity (iasm-disasm-syms-args-cons file) " ")))
  (insert (format
           "insts: %s %s\n" iasm-disasm-cmd
           (mapconcat 'identity (iasm-disasm-insts-args-cons file 0 0) " ")))
  (insert "\n"))


(defun iasm-buffer-inst-p (pos) (get-text-property pos 'iasm-inst))
(defun iasm-buffer-sym-p (pos)  (get-text-property pos 'iasm-sym))
(defun iasm-buffer-addr (pos)   (get-text-property pos 'iasm-addr))

(defun iasm-buffer-sym (pos)
  (when (iasm-buffer-addr pos)
    (iasm-index-find-sym iasm-index (iasm-buffer-addr pos))))

(defun iasm-buffer-inst (pos)
  (when (iasm-buffer-inst-p pos)
    (iasm-index-find-inst iasm-index (iasm-buffer-addr pos))))

(defun iasm-buffer-sym-loaded-p (pos)
  (not (iasm-index-sym-empty iasm-index (iasm-buffer-addr pos))))

(defun iasm-buffer-sym-pos (pos)
  (if (iasm-buffer-sym-p pos) pos
    (when (iasm-buffer-inst-p pos)
      (let ((sym (iasm-buffer-sym pos)))
        (iasm-sym-pos sym)))))

(defun iasm-buffer-inst-pos (pos)
  (if (iasm-buffer-inst-p pos) pos
    (when (and (iasm-buffer-sym-p pos) (iasm-buffer-sym-loaded-p pos))
      (let ((sym (iasm-buffer-sym pos)))
        (+ (iasm-sym-pos sym) (iasm-sym-head-size sym))))))

(defun iasm-buffer-invisibility-p (pos)
  (let ((inst-pos (iasm-buffer-inst-pos pos)))
    (when inst-pos (get-text-property inst-pos 'invisible))))

(defun iasm-buffer-set-invisibility (pos value)
  (let ((sym-pos (iasm-buffer-sym-pos pos)))
    (when sym-pos
      (let* ((sym (iasm-buffer-sym sym-pos))
             (start (+ (iasm-sym-pos sym) (iasm-sym-head-size sym)))
             (stop (+ (iasm-sym-pos sym) (iasm-sym-pos-size sym))))
        (set-text-properties start stop `(invisible ,value))))))

(defun iasm-buffer-collapse-sym (pos)
  (when (not (iasm-buffer-invisibility-p pos))
    (iasm-buffer-set-invisibility pos t)))

(defun iasm-buffer-sym-load (pos)
  (when (and (iasm-buffer-sym-p pos) (not (iasm-buffer-sym-loaded-p pos)))
    (let* ((sym (iasm-buffer-sym pos))
           (inst-pos (+ (iasm-sym-pos sym) (iasm-sym-head-size sym)))
           (addr-start (iasm-sym-addr sym))
           (addr-stop (+ addr-start (iasm-sym-addr-size sym))))
      (goto-char inst-pos)
      (iasm-insts-run iasm-file addr-start addr-stop))))

(defun iasm-buffer-jump-to-addr (addr)
  (let ((sym (iasm-index-find-sym iasm-index addr)))
    (when sym
      (if (avl-tree-empty (iasm-sym-insts sym))
          (progn
            (setq iasm-queued-jump addr)
            (iasm-buffer-sym-load (iasm-sym-pos sym)))
        (let ((inst (iasm-index-find-inst iasm-index addr)))
          (goto-char (iasm-inst-pos inst)))))))

(defun iasm-buffer-do-queued-jump ()
  (when (boundp 'iasm-queue-jump)
    (setq iasm-queued-sym-jump nil)
    (when iasm-queued-jump
      (iasm-buffer-jump-to-addr iasm-queued-jump)
      (setq iasm-queued-jump nil))))


(defun iasm-buffer-goto-sym (addr)
  (let ((sym (iasm-index-find-sym iasm-index addr)))
    (when sym (goto-char (iasm-sym-pos sym)))))

(defun iasm-buffer-goto-next-sym (addr)
  (let ((sym (iasm-index-find-next-sym iasm-index addr)))
    (when sym (goto-char (iasm-sym-pos sym)))))


;; -----------------------------------------------------------------------------
;; interactive - in-buffer
;; -----------------------------------------------------------------------------

(define-derived-mode iasm-mode asm-mode
  "iasm"
  "Interactive disassembly mode.

Provides an efficient frontend for objdump as well as useful
tools to shorten the edit-compile-disassemble loop.

\\{iasm-mode-map}"
  :group 'iasm

  (define-key iasm-mode-map (kbd "q")   'iasm-quit)
  (define-key iasm-mode-map (kbd "g")   'iasm-refresh)
  (define-key iasm-mode-map (kbd "TAB") 'iasm-toggle-sym-at-point)
  (define-key iasm-mode-map (kbd "s")   'iasm-show-ctx-at-point)
  (define-key iasm-mode-map (kbd "n")   'iasm-next-line)
  (define-key iasm-mode-map (kbd "p")   'iasm-previous-line)
  (define-key iasm-mode-map (kbd "M-n") 'iasm-next-sym)
  (define-key iasm-mode-map (kbd "M-p") 'iasm-previous-sym)
  (define-key iasm-mode-map (kbd "j")   'iasm-jump)
  (define-key iasm-mode-map (kbd "c")   'iasm-collapse-all-syms)
  (define-key iasm-mode-map (kbd "l")   'iasm-goto-ldd))


(defun iasm-toggle-sym-at-point ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (when (and (null iasm-loading) (iasm-buffer-sym-p (point)))
        (if (not (iasm-buffer-sym-loaded-p (point)))
            (iasm-buffer-sym-load (point))
          (let ((value (not (iasm-buffer-invisibility-p (point)))))
            (iasm-buffer-set-invisibility (point) value)))))))

(defun iasm-collapse-all-syms ()
  (interactive)
  (let ((inhibit-read-only t))
    (iasm-index-sym-map
     iasm-index
     (lambda (sym) (iasm-buffer-collapse-sym (iasm-sym-pos sym)) sym))))

(defun iasm-show-ctx-at-point ()
  (interactive)
  (when (iasm-buffer-inst-p (point))
    (let* ((iasm-buf (current-buffer))
           (inst (iasm-buffer-inst (point)))
           (file (iasm-inst-file inst))
           (line (iasm-inst-line inst)))
      (when (and file line)
        (find-file-other-window file)
        (goto-line line)
        (pop-to-buffer iasm-buf)
        (message "Showing: %s:%s" file line)))))

(defun iasm-next-line ()
  (interactive)
  (next-line)
  (iasm-show-ctx-at-point))

(defun iasm-previous-line ()
  (interactive)
  (previous-line)
  (iasm-show-ctx-at-point))

(defun iasm-next-sym ()
  (interactive)
  (let* ((sym (iasm-buffer-sym (point)))
         (addr (if sym (+ (iasm-sym-addr sym) 1) 0)))
    (iasm-buffer-goto-next-sym addr)))

(defun iasm-previous-sym ()
  (interactive)
  (if (iasm-buffer-inst-p (point))
      (iasm-buffer-goto-sym (iasm-buffer-addr (point)))
    (let ((sym (iasm-buffer-sym (point))))
      (when sym
        (iasm-buffer-goto-sym (- (iasm-sym-addr sym) 1))))))

(defun iasm-jump ()
  (interactive)
  (when (iasm-buffer-inst-p (point))
    (let* ((inhibit-read-only t)
           (inst (iasm-buffer-inst (point)))
           (target (iasm-inst-target inst)))
      (when target (iasm-buffer-jump-to-addr target)))))

(defun iasm-goto-ldd ()
  (interactive)
  (when iasm-file (iasm-ldd-file iasm-file)))

(defun iasm-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun iasm-refresh ()
  (interactive)
  (let ((inhibit-read-only t)
        (sym (iasm-buffer-sym (point))))
    (iasm-buffer-init iasm-file)
    (when sym (setq iasm-queued-sym-jump (iasm-sym-name sym)))
    (iasm-syms-run iasm-file)))

(defun iasm-refresh-if-stale ()
  (interactive)
  (assert iasm-file-last-modified)
  (when (< (time-to-seconds iasm-file-last-modified)
           (time-to-seconds (nth 5 (file-attributes iasm-file))))
    (iasm-refresh)))

;; -----------------------------------------------------------------------------
;; interactive - out-of-buffer
;; -----------------------------------------------------------------------------

(defun iasm-disasm (file)
  "Disassemble FILE into an iasm buffer."
  (interactive "fObject file: ")
  (let* ((abs-file (expand-file-name file))
         (name     (iasm-buffer-name (expand-file-name abs-file)))
         (buffer   (get-buffer name)))
    (if buffer (with-current-buffer buffer (iasm-refresh-if-stale))
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (iasm-mode)
          (iasm-buffer-init abs-file)
          (iasm-syms-run abs-file))))
    (switch-to-buffer-other-window buffer)
    buffer))

(defun iasm-goto-disasm-buffer ()
  "Switch to the linked iasm buffer and refresh it if necessary.

A buffer can be linked to an iasm buffer using the
iasm-disasm-link-buffer function. The linked buffer is stored in
the buffer-local variable 'iasm-linked-buffer'."

  (interactive)
  (when (and (boundp 'iasm-linked-buffer) (buffer-live-p iasm-linked-buffer))
    (with-current-buffer iasm-linked-buffer (iasm-refresh-if-stale))
    (switch-to-buffer-other-window iasm-linked-buffer)))

(defun iasm-disasm-link-buffer (file)
  "Disassemble FILE and links the current buffer to the iasm buffer."
  (interactive "fObject file: ")
  (let ((src-buffer (current-buffer))
        (iasm-buffer (iasm-disasm file)))
    (with-current-buffer src-buffer
      (make-variable-buffer-local 'iasm-linked-buffer)
      (setq iasm-linked-buffer iasm-buffer))))


;; -----------------------------------------------------------------------------
;; ldd
;; -----------------------------------------------------------------------------

(defcustom iasm-ldd-cmd "ldd"
  "Executable used to retrieve linked library information."
  :group 'iasm
  :type 'string)

(defcustom iasm-ldd-args ""
  "Arguments passed to the ldd executable."
  :group 'iasm
  :type 'string)


;; -----------------------------------------------------------------------------
;; ldd - process
;; -----------------------------------------------------------------------------

(defun iasm-ldd-args-cons (file)
  (append (split-string iasm-ldd-args " " t) `(,file)))

(defun iasm-ldd-proc-insert (lib path addr)
  (assert (or lib path))
  (assert (stringp addr))
  (when path (setq path (expand-file-name path)))
  (unless lib (setq lib (file-name-nondirectory path)))
  (assert lib)
  (let ((pos (point)))
    (insert (format "%s  %-32s %s \n" addr lib path))
    (set-text-properties pos (point) `(ldd-path ,path))))

(defconst iasm-ldd-proc-regex-full
  (concat
   "\\s-+\\(.*\\)"       ;; lib
   " => "                ;; anchor
   "\\(.*\\)"            ;; path
   " (0x\\([0-9a-f]+\\))" ;; address
   ))

(defconst iasm-ldd-proc-regex-short
  (concat
   "\\s-+\\(.*\\)"       ;; path
   " (0x\\([0-9a-f]+\\))" ;; address
   ))

(defconst iasm-ldd-proc-regex-error
  (concat
   "^ldd: "    ;; anchor
   ".*: "      ;; file
   "\\(.*\\)$" ;; message
   ))

(defun iasm-ldd-proc-filter (line)
  (end-of-buffer)
  (save-match-data
    (if (string-match iasm-ldd-proc-regex-full line)
        (iasm-ldd-proc-insert
         (match-string 1 line)
         (match-string 2 line)
         (match-string 3 line))
      (if (string-match iasm-ldd-proc-regex-short line)
        (iasm-ldd-proc-insert
         nil
         (match-string 1 line)
         (match-string 2 line))
        (when (string-match iasm-ldd-proc-regex-error line)
          (insert "ERROR: " (match-string 1 line)))))))

(defun iasm-ldd-proc-run (file)
  (let ((args (iasm-ldd-args-cons file)))
    (iasm-process-run
     file iasm-ldd-cmd args
     (lambda (proc string)
       (iasm-process-filter proc string 'iasm-ldd-proc-filter))
     (lambda (proc state)
       (iasm-process-sentinel proc state 'iasm-ldd-proc-filter nil)))))


;; -----------------------------------------------------------------------------
;; ldd - buffer
;; -----------------------------------------------------------------------------

(defun iasm-ldd-buffer-name (file)
  (concat "*ldd " (file-name-nondirectory file) "*"))

(defun iasm-ldd-buffer-init (file)
  (make-variable-buffer-local 'iasm-ldd-file)
  (setq iasm-ldd-file file)

  (make-variable-buffer-local 'iasm-ldd-file-last-modified)
  (setq iasm-ldd-file-last-modified (nth 5 (file-attributes file)))

  (make-variable-buffer-local 'iasm-loading)
  (setq iasm-loading nil)

  (toggle-truncate-lines t)
  (setq buffer-read-only t)

  (erase-buffer)
  (insert "file: " file "\n")

  (insert (format
           "cmd:  %s %s\n"
           iasm-ldd-cmd (mapconcat 'identity (iasm-ldd-args-cons file) " ")))
  (insert "\n"))

(defun iasm-ldd-buffer-path (pos) (get-text-property pos 'ldd-path))

;; -----------------------------------------------------------------------------
;; ldd - interactive
;; -----------------------------------------------------------------------------


(define-derived-mode iasm-ldd-mode asm-mode
  "iasm-ldd"
  "Interactive ldd mode.

Provides an interactive frontend for ldd.

\\{iasm-ldd-mode-map}"
  :group 'iasm

  (define-key iasm-ldd-mode-map (kbd "q")   'iasm-ldd-quit)
  (define-key iasm-ldd-mode-map (kbd "g")   'iasm-ldd-refresh)
  (define-key iasm-ldd-mode-map (kbd "j")   'iasm-ldd-jump)
  (define-key iasm-ldd-mode-map (kbd "RET") 'iasm-ldd-jump)
  (define-key iasm-ldd-mode-map (kbd "d")   'iasm-ldd-disasm))

(defun iasm-ldd-jump ()
  (interactive)
  (let ((path (iasm-ldd-buffer-path (point))))
    (when path (iasm-ldd-file path))))

(defun iasm-ldd-disasm ()
  (interactive)
  (let ((path (iasm-ldd-buffer-path (point))))
    (when path (iasm-disasm path))))

(defun iasm-ldd-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun iasm-ldd-refresh ()
  (interactive)
  (assert iasm-ldd-file)
  (let ((inhibit-read-only t))
    (iasm-ldd-buffer-init iasm-ldd-file)
    (iasm-ldd-proc-run iasm-ldd-file)))

(defun iasm-ldd-refresh-if-stale ()
  (interactive)
  (assert iasm-ldd-file-last-modified)
  (when (< (time-to-seconds iasm-ldd-file-last-modified)
           (time-to-seconds (nth 5 (file-attributes iasm-ldd-file))))
    (iasm-ldd-refresh)))

(defun iasm-ldd (file)
  (interactive "fObject file: ")
  (let* ((abs-file (expand-file-name file))
         (name     (iasm-ldd-buffer-name (expand-file-name abs-file)))
         (buffer   (get-buffer name)))
    (if buffer (with-current-buffer buffer (iasm-ldd-refresh-if-stale))
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (iasm-ldd-mode)
          (iasm-ldd-buffer-init abs-file)
          (iasm-ldd-proc-run abs-file))))
    (switch-to-buffer-other-window buffer)))


;; -----------------------------------------------------------------------------
;; packaging
;; -----------------------------------------------------------------------------

(provide 'iasm-mode)
