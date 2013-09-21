;; -----------------------------------------------------------------------------
;; iasm-mode-2.el
;; Rémi Attab (remi.attab@gmail.com), 07 Sep 2013
;; FreeBSD-style copyright and disclaimer apply
;;
;; Rewrite of iasm-mode
;;
;; The idea is grab the output of objdump, format it and make it interactive.
;; Note that this file probably contains some horrible abuse and misuse of emacs
;; features that would make an elisp veteran's blood boil. Go forth at your own
;; peril.
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

(defcustom iasm-objdump "objdump"
  "Executable used to retrieve the assembly of an object file"
  :group 'iasm
  :type 'string)

(defcustom iasm-syms-args "-tCwj .text"
  "Arguments fed to the executable to retrieve symbol information"
  :group 'iasm
  :type 'string)

(defcustom iasm-disasm-args "-dlCw --no-show-raw-insn"
  "Arguments fed to the executable to retrieve assembly information"
  :group 'iasm
  :type 'string)


;; -----------------------------------------------------------------------------
;; Mode
;; -----------------------------------------------------------------------------

(define-derived-mode iasm-mode asm-mode
  "iasm"
  "BLAH!
\\{iasm-mode-map}"
  :group 'iasm

  (define-key iasm-mode-map (kbd "g")   'iasm-refresh)
  (define-key iasm-mode-map (kbd "TAB") 'iasm-toggle-sym-at-point)
  (define-key iasm-mode-map (kbd "d")   'iasm-debug)
  (define-key iasm-mode-map (kbd "s")   'iasm-show-ctx-at-point)
  (define-key iasm-mode-map (kbd "n")   'iasm-next-line)
  (define-key iasm-mode-map (kbd "p")   'iasm-previous-line)
  (define-key iasm-mode-map (kbd "M-n") 'iasm-next-sym)
  (define-key iasm-mode-map (kbd "M-p") 'iasm-previous-sym)
  (define-key iasm-mode-map (kbd "j")   'iasm-jump))


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
;; syms parser
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
  (iasm-index-sym-map iasm-index 'iasm-syms-insert))


;; -----------------------------------------------------------------------------
;; disasm parser
;; -----------------------------------------------------------------------------

(defun iasm-disasm-init (addr-start addr-stop)
  (make-variable-buffer-local 'iasm-disasm-sym)
  (setq iasm-disasm-sym (iasm-index-find-sym iasm-index addr-start))

  (make-variable-buffer-local 'iasm-disasm-ctx-file)
  (setq iasm-disasm-ctx-file nil)

  (make-variable-buffer-local 'iasm-disasm-ctx-line)
  (setq iasm-disasm-ctx-line nil)

  (make-variable-buffer-local 'iasm-disasm-ctx-fn)
  (setq iasm-disasm-ctx-fn nil))


(defun iasm-disasm-update-ctx (line)
  (setq iasm-disasm-ctx-file (match-string 1 line))
  (setq iasm-disasm-ctx-line (string-to-number (match-string 2 line))))

(defun iasm-disasm-update-ctx-fn (line)
  (setq iasm-current-ctx-fn (match-string 1 line)))

(defun iasm-disasm-annotate-inst (start-pos stop-pos addr target)
  (assert iasm-disasm-sym)
  (iasm-index-add-inst iasm-index (make-iasm-inst
                                   :addr   addr
                                   :pos    start-pos
                                   :target target
                                   :file   iasm-disasm-ctx-file
                                   :line   iasm-disasm-ctx-line
                                   :fn     iasm-disasm-ctx-fn))
  (when (and iasm-disasm-ctx-file iasm-disasm-ctx-line)
    (iasm-index-add-ctx iasm-index iasm-disasm-ctx-file iasm-disasm-ctx-line addr))
  (setf (iasm-sym-pos-size iasm-disasm-sym)
        (+ (iasm-sym-pos-size iasm-disasm-sym) (- stop-pos start-pos)))
  (add-text-properties start-pos stop-pos '(iasm-inst t))
  (add-text-properties start-pos stop-pos `(iasm-addr ,addr)))


(defconst iasm-disasm-regex-inst   "^ *\\([0-9a-f]+\\):")
(defconst iasm-disasm-regex-jump   "\\([0-9a-f]+\\) <.*>$")
(defconst iasm-disasm-regex-ctx    "^\\(/.+\\):\\([0-9]+\\)")
(defconst iasm-disasm-regex-ctx-fn "^\\(.+\\):$")


(defun iasm-disasm-jump-target (line)
  (save-match-data
    (when (string-match iasm-disasm-regex-jump line)
      (string-to-number (match-string 1 line) 16))))

(defun iasm-disasm-insert-inst (line)
  (assert iasm-disasm-sym)
  (let ((addr   (string-to-number (match-string 1 line) 16))
        (target (iasm-disasm-jump-target line))
        (pos    (+ (iasm-sym-pos iasm-disasm-sym)
                   (iasm-sym-pos-size iasm-disasm-sym))))
    (goto-char pos)
    (insert line "\n")
    (iasm-disasm-annotate-inst pos (point) addr target)))


(defun iasm-disasm-filter (line)
  (save-match-data
    (if (string-match iasm-disasm-regex-inst line)
        (iasm-disasm-insert-inst line)
      (if (string-match iasm-disasm-regex-ctx line)
          (iasm-disasm-update-ctx line)
        (if (string-match iasm-disasm-regex-ctx-fn line)
            (iasm-disasm-update-ctx-fn line))))))

(defun iasm-disasm-sentinel ()
  (assert iasm-disasm-sym)
  (let ((pos (iasm-sym-pos iasm-disasm-sym))
        (delta (- (iasm-sym-pos-size iasm-disasm-sym)
                  (iasm-sym-head-size iasm-disasm-sym))))
    (iasm-index-shift iasm-index (+ pos 1) delta))

  (makunbound 'iasm-disasm-sym)
  (makunbound 'iasm-disasm-ctx-file)
  (makunbound 'iasm-disasm-ctx-line)
  (makunbound 'iasm-disasm-ctx-fn))


;; -----------------------------------------------------------------------------
;; objdump
;; -----------------------------------------------------------------------------

(defun iasm-objdump-process-buffer-impl (head tail fn)
  (if tail
      (progn
        (funcall fn head)
        (iasm-objdump-process-buffer-impl (car tail) (cdr tail) fn))
    (setq iasm-objdump-proc-buffer head)))

(defun iasm-objdump-process-buffer (fn)
  (when iasm-objdump-proc-buffer
    (let ((split (split-string iasm-objdump-proc-buffer "\n")))
      (iasm-objdump-process-buffer-impl (car split) (cdr split) fn))))


(defun iasm-objdump-filter (proc string filter)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (setq iasm-objdump-proc-buffer (concat iasm-objdump-proc-buffer string))
      (save-excursion
        (let ((inhibit-read-only t))
          (iasm-objdump-process-buffer filter))))))


(defun iasm-objdump-sentinel (proc state filter sentinel)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (save-excursion
        (let ((inhibit-read-only t))
          (iasm-objdump-process-buffer filter)
          (funcall sentinel)))))
  (setq iasm-loading nil)
  (iasm-buffer-do-queued-jump))


(defun iasm-objdump-run (file args filter sentinel)
  (unless iasm-loading
    (setq iasm-loading t)
    (make-variable-buffer-local 'iasm-objdump-proc-buffer)
    (setq iasm-objdump-proc-buffer "")

    (let ((proc (apply 'start-process
                       "iasm-objdump"
                       (current-buffer)
                       iasm-objdump
                       args)))
      (set-process-filter proc filter)
      (set-process-sentinel proc sentinel))))


(defun iasm-objdump-syms-args-cons (file)
  (append
   (split-string iasm-syms-args " ")
   `(,(expand-file-name file))))

(defun iasm-objdump-run-syms (file)
  (iasm-syms-init)
  (let ((args (iasm-objdump-syms-args-cons file)))
    (iasm-objdump-run
     file args
     (lambda (proc string)
       (iasm-objdump-filter proc string 'iasm-syms-filter))
     (lambda (proc state)
       (iasm-objdump-sentinel proc state
                              'iasm-syms-filter 'iasm-syms-sentinel)))))


(defun iasm-objdump-disasm-args-cons (file start stop)
  (append
   (split-string iasm-disasm-args " ")
   `(,(format "--start-address=0x%x" start))
   `(,(format "--stop-address=0x%x" stop))
   `(,(expand-file-name file))))

(defun iasm-objdump-run-disasm (file start stop)
  (assert (< start stop))
  (iasm-disasm-init start stop)
  (let ((args (iasm-objdump-disasm-args-cons file start stop)))
    (iasm-objdump-run
     file args
     (lambda (proc string)
       (iasm-objdump-filter proc string 'iasm-disasm-filter))
     (lambda (proc state)
       (iasm-objdump-sentinel proc state
                              'iasm-disasm-filter 'iasm-disasm-sentinel)))))


;; -----------------------------------------------------------------------------
;; buffer
;; -----------------------------------------------------------------------------

(defun iasm-buffer-name (file)
  (concat "*iasm " (file-name-nondirectory file) "*"))


(defun iasm-buffer-init (file)
  (make-variable-buffer-local 'iasm-file)
  (setq iasm-file file)

  (make-variable-buffer-local 'iasm-loading)
  (setq iasm-loading nil)

  (make-variable-buffer-local 'iasm-queued-jump)
  (setq iasm-queued-jump nil)

  (make-variable-buffer-local 'iasm-queued-sym-jump)
  (setq iasm-queued-sym-jump nil)

  (toggle-truncate-lines t)
  (setq buffer-read-only t)

  (erase-buffer)
  (insert (format "file:   %s\n" file))
  (insert (format
           "syms:   %s %s\n" iasm-objdump
           (mapconcat 'identity (iasm-objdump-syms-args-cons file) " ")))
  (insert (format
           "dasm:   %s %s\n" iasm-objdump
           (mapconcat 'identity (iasm-objdump-disasm-args-cons file 0 0) " ")))
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

(defun iasm-buffer-sym-load (pos)
  (when (and (iasm-buffer-sym-p pos) (not (iasm-buffer-sym-loaded-p pos)))
    (let* ((sym (iasm-buffer-sym pos))
           (inst-pos (+ (iasm-sym-pos sym) (iasm-sym-head-size sym)))
           (addr-start (iasm-sym-addr sym))
           (addr-stop (+ addr-start (iasm-sym-addr-size sym))))
      (goto-char inst-pos)
      (iasm-objdump-run-disasm iasm-file addr-start addr-stop))))

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
  (setq iasm-queued-sym-jump nil)
  (when iasm-queued-jump
    (iasm-buffer-jump-to-addr iasm-queued-jump)
    (setq iasm-queued-jump nil)))


(defun iasm-buffer-goto-sym (addr)
  (let ((sym (iasm-index-find-sym iasm-index addr)))
    (when sym (goto-char (iasm-sym-pos sym)))))

(defun iasm-buffer-goto-next-sym (addr)
  (let ((sym (iasm-index-find-next-sym iasm-index addr)))
    (when sym (goto-char (iasm-sym-pos sym)))))


;; -----------------------------------------------------------------------------
;; interactive - in-buffer
;; -----------------------------------------------------------------------------

(defun iasm-disasm (file)
  (interactive "fObject file: ")
  (let ((buf (get-buffer-create (iasm-buffer-name file))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (iasm-mode)
        (iasm-buffer-init file)
        (iasm-objdump-run-syms file)))
    (switch-to-buffer-other-window buf)))

(defun iasm-refresh ()
  (interactive)
  (let ((inhibit-read-only t)
        (sym (iasm-buffer-sym (point))))
    (iasm-buffer-queue-jump (point))
    (iasm-buffer-init iasm-file)
    (when sym (setq iasm-queued-sym-jump (iasm-sym-name sym)))
    (iasm-objdump-run-syms iasm-file)))

(defun iasm-toggle-sym-at-point ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (when (and (null iasm-loading) (iasm-buffer-sym-p (point)))
        (if (not (iasm-buffer-sym-loaded-p (point)))
            (iasm-buffer-sym-load (point))
          (let ((value (not (iasm-buffer-invisibility-p (point)))))
            (iasm-buffer-set-invisibility (point) value)))))))

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
        (message "Jumped to: %s:%s" file line)))))

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

(defun iasm-debug ()
  (interactive)
  (let ((inhibit-read-only t)
        (is-sym (iasm-buffer-sym-p (point)))
        (is-inst (iasm-buffer-inst-p (point))))
    (message "show: pos=%s, addr=%x, sym=%s, inst=%s, obj=%s"
             (point)
             (iasm-buffer-addr   (point))
             is-sym is-inst
             (if is-sym (iasm-buffer-sym (point))
               (when is-inst (iasm-buffer-inst (point)))))))

;; -----------------------------------------------------------------------------
;; packaging
;; -----------------------------------------------------------------------------

(provide 'iasm-mode)
