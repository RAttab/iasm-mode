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
;; \todo
;;
;;  - When jumping to a location the disasm sentinel should jump to the
;;    requested location.
;;  - Only allow one symload at a time so have a block flag (async's a bitch).
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

(defvar iasm-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "g")   'iasm-refresh)
    (define-key map (kbd "TAB") 'iasm-toggle-sym-at-point)
    map))

(define-derived-mode iasm-mode asm-mode
  "iasm"
  "BLAH!
\\{iasm-mode-map}"
  :group 'iasm
  (toggle-truncate-lines t)
  (setq buffer-read-only t)

  (local-set-key (kbd "g")   'iasm-refresh)
  (local-set-key (kbd "TAB") 'iasm-toggle-sym-at-point)
  (local-set-key (kbd "d")   'iasm-debug)
  (local-set-key (kbd "s")   'iasm-debug-show))


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

(defstruct iasm-sym addr addr-size pos pos-size head-size insts name)
(defun iasm-sym-less (lhs rhs)
  (assert (iasm-sym-p lhs))
  (assert (iasm-sym-p rhs))
  (< (iasm-sym-addr lhs) (iasm-sym-addr rhs)))


(defstruct iasm-inst addr pos target file line fn)
(defun iasm-inst-less-addr (lhs rhs)
  (assert (iasm-inst-p lhs))
  (assert (iasm-inst-p rhs))
  (< (iasm-inst-addr lhs) (iasm-inst-addr rhs)))


(defun iasm-index-create ()
  (avl-tree-create 'iasm-sym-less))


(defun iasm-index-shift (index min-pos delta)
  (avl-tree-map
   (lambda (sym)
     (assert sym)
     (when (< min-pos (iasm-sym-pos sym))
         (setf (iasm-sym-pos sym) (+ delta (iasm-sym-pos sym))))
     sym)
   index))


(defun iasm-index-add-sym (index sym)
  (assert index)
  (setf (iasm-sym-insts sym) (avl-tree-create 'iasm-inst-less-addr))
  (avl-tree-enter index sym))

(defun iasm-index-add-inst (index inst)
  (assert index)
  (let ((sym (iasm-index-find-sym index (iasm-inst-addr inst))))
    (assert sym)
    ;; Relative positions means that we don't need to update it when we shift.
    (setf (iasm-inst-pos inst) (- (iasm-inst-pos inst) (iasm-sym-pos sym)))
    (avl-tree-enter (iasm-sym-insts sym) inst)))


(defun iasm-index-find-sym (index addr)
  (assert index)
  (avl-tree-lower-bound index (make-iasm-sym :addr addr)))

(defun iasm-index-find-inst (index addr)
  (assert index)
  (let ((sym (iasm-index-find-sym index addr)))
    (assert sym)
    (let* ((inst-src (avl-tree-lower-bound (iasm-sym-insts sym)
                                           (make-iasm-inst :addr addr)))
           (inst (copy-iasm-inst inst-src)))
      ;; Convert relative positions into absolutes
      (setf (iasm-inst-pos inst) (+ (iasm-sym-pos sym)
                                    (iasm-inst-pos inst)))
      inst)))


(defun iasm-index-sym-empty (index addr)
  (assert index)
  (let ((sym (iasm-index-find-sym index addr)))
    (assert sym)
    (avl-tree-empty (iasm-sym-insts sym))))


;; -----------------------------------------------------------------------------
;; syms parser
;; -----------------------------------------------------------------------------

(defun iasm-syms-init ()
  (make-variable-buffer-local 'iasm-index)
  (setq iasm-index (iasm-index-create)))


(defun iasm-syms-annotate (pos-start pos-stop name addr addr-size)
  (assert (< pos-start pos-stop))
  (assert (stringp name))
  (assert (integerp pos-start))
  (assert (integerp pos-stop))
  (assert (integerp addr))
  (assert (integerp addr-size))
  (iasm-index-add-sym iasm-index (make-iasm-sym
                                       :name      name
                                       :addr      addr
                                       :addr-size addr-size
                                       :pos       start
                                       :pos-size  (- pos-stop pos-start)
                                       :head-size (- pos-stop pos-start)))
  (add-text-properties pos-start pos-stop '(iasm-sym t))
  (add-text-properties pos-start pos-stop `(iasm-addr ,addr)))


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
      (let ((start (point))
            (addr (match-string 1 line))
            (size (string-to-number (match-string 2 line) 16))
            (name (match-string 3 line)))
        (when (> size 0)
          (insert (format "%s <%s>: \n" addr name))
          (iasm-syms-annotate start (point) name
                              (string-to-number addr 16)
                              size))))))

(defun iasm-syms-sentinel ())


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
  (setq iasm-disasm-ctx-line (string-to-number (match-string 2 line) 16)))

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
          (funcall sentinel))))))


(defun iasm-objdump-run (file args filter sentinel)
  (make-variable-buffer-local 'iasm-objdump-proc-buffer)
  (setq iasm-objdump-proc-buffer "")
  (message "Running: %s %s" iasm-objdump args)

  (let ((proc (apply 'start-process
                     "iasm-objdump"
                     (current-buffer)
                     iasm-objdump
                     args)))
    (set-process-filter proc filter)
    (set-process-sentinel proc sentinel)))


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
  (when (iasm-buffer-sym-p pos)
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
      (message "load: pos=%d, inst-pos=%s, addr=[%s, %s], sym=%s"
               pos inst-pos addr-start addr-stop sym)
      (goto-char inst-pos)
      (iasm-objdump-run-disasm iasm-file addr-start addr-stop))))


;; -----------------------------------------------------------------------------
;; interactive
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
  (let ((inhibit-read-only t))
        (iasm-buffer-init iasm-file)
        (iasm-objdump-run-syms iasm-file)))

(defun iasm-toggle-sym-at-point ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (when (iasm-buffer-sym-p (point))
        (if (not (iasm-buffer-sym-loaded-p (point)))
            (iasm-buffer-sym-load (point))
          (let ((value (not (iasm-buffer-invisibility-p (point)))))
            (iasm-buffer-set-invisibility (point) value)))))))

(defun iasm-debug ()
  (interactive)
  (let ((inhibit-read-only t))
    (end-of-buffer)
    (insert "\n\n")
    (dolist (entry (avl-tree-flatten iasm-index))
      (insert (format "%s\n" entry)))))

(defun iasm-debug-show ()
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
