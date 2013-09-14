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
  (toggle-truncate-lines t)
  (setq buffer-read-only t))


;; -----------------------------------------------------------------------------
;; index
;; -----------------------------------------------------------------------------

(defun avl-tree-lower-bound (tree data)
  "Returns the greatest element that is smaller or equal to data.
Extension to the standard avl-tree library."
  (let ((node (avl-tree--root tree))
        (compare-function (avl-tree--cmpfun tree))
        bound found)
    (while (and node (not found))
      (cond
       ((funcall compare-function data (avl-tree--node-data node))
        (setq node (avl-tree--node-left node))
        (when node (setq bound (avl-tree--node-data node))))
       ((funcall compare-function (avl-tree--node-data node) data)
        (setq bound (avl-tree--node-data node))
        (setq node (avl-tree--node-right node)))
       (t
        (setq found t)
        (setq bound (avl-tree--node-data node)))))
    (when (or found (funcall compare-function bound data))
      bound)))

(defun avl-tree-upper-bound (tree data)
  "Returns the smallest element that is greater or equal to data.
Extension to the standard avl-tree library."
  (let ((node (avl-tree--root tree))
        (compare-function (avl-tree--cmpfun tree))
        bound found)
    (while (and node (not found))
      (cond
       ((funcall compare-function data (avl-tree--node-data node))
        (setq bound (avl-tree--node-data node))
        (setq node (avl-tree--node-left node)))
       ((funcall compare-function (avl-tree--node-data node) data)
        (setq node (avl-tree--node-right node))
        (when node (setq bound (avl-tree--node-data node))))
       (t
        (setq found t)
        (setq bound (avl-tree--node-data node)))))
    (when (or found (funcall compare-function data bound))
      bound)))


(defstruct iasm-entry name addr pos size)

(defun iasm-entry-less (lhs rhs)
  (< (iasm-entry-addr lhs) (iasm-entry-addr rhs)))

(defun iasm-index-create ()
  (avl-tree-create 'iasm-entry-less))

(defun iasm-index-shift (index min-pos delta)
  (avl-tree-map
   (lambda (entry)
     (when (< min-pos (iasm-entry-pos entry))
         (setf (iasm-entry-pos entry) (+ delta (iasm-entry-pos entry))))
     entry)
   index))

(defun iasm-index-add (index name addr pos size)
  (avl-tree-enter
   index (make-iasm-entry :name name :addr addr :pos pos :size size))
  (iasm-index-shift index (+ pos 1) size))

(defun iasm-index-find (index addr)
  (avl-tree-lower-bound index (make-iasm-entry :addr addr)))

(defun iasm-index-adjust-size (index addr new-size)
  (let ((entry (iasm-index-find index addr)))
    (when entry
      (avl-tree-delete index entry)
      (iasm-index-shift index
                        (+ (iasm-entry-pos entry) 1)
                        (- new-size (iasm-entry-size entry)))
      (setf (iasm-entry-size entry) new-size)
      (avl-tree-enter index entry))))

;; -----------------------------------------------------------------------------
;; syms parser
;; -----------------------------------------------------------------------------

(defun iasm-syms-init ()
  (make-variable-buffer-local 'iasm-syms-index)
  (setq iasm-syms-index (iasm-index-create)))

(defun iasm-syms-annotate (start stop name addr size)
  (iasm-index-add iasm-syms-index name addr start (- stop start))
  (add-text-properties start stop `(iasm-loaded nil))
  (add-text-properties start stop `(iasm-addr-start ,addr))
  (add-text-properties start stop `(iasm-addr-stop ,(+ addr size))))

(defun iasm-syms-filter (line)
  (save-match-data
    (when (string-match
           "^\\([0-9a-f]+\\).*\\.text\\s-+\\([0-9a-f]+\\)\\s-+\\(.+\\)$"
           line)
      (let ((start (point))
            (addr (match-string 1 line))
            (size (match-string 2 line))
            (name (match-string 3 line)))
        (when (> (string-to-number size 16) 0)
          (insert (format "%s <%s>: \n" addr name))
          (iasm-syms-annotate start (point) name
                              (string-to-number addr 16)
                              (string-to-number size 16)))))))

(defun iasm-syms-sentinel ())


;; -----------------------------------------------------------------------------
;; disasm parser
;; -----------------------------------------------------------------------------

(defun iasm-disasm-init (start end)
  )

(defun iasm-disasm-filter (line)
  (insert line "\n"))

(defun iasm-disasm-sentinel ()
  )


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
          (end-of-buffer)
          (iasm-objdump-process-buffer filter))))))


(defun iasm-objdump-sentinel (proc state filter sentinel)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (save-excursion
        (let ((inhibit-read-only t))
          (end-of-buffer)
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
   `(,(format "--start-address=%x" start))
   `(,(format "--stop-address=%x" stop))
   `(,(expand-file-name file))))

(defun iasm-objdump-run-disasm (file start end)
  (iasm-disasm-init start end)
  (let ((args (iasm-objdump-disasm-args-cons file start end)))
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


(defun iasm-buffer-header (file)
  (erase-buffer)
  (insert (format "file:   %s\n" file))
  (insert (format
           "syms:   %s %s\n" iasm-objdump
           (mapconcat 'identity (iasm-objdump-syms-args-cons file) " ")))
  (insert (format
           "dasm:   %s %s\n" iasm-objdump
           (mapconcat 'identity (iasm-objdump-disasm-args-cons file 0 0) " ")))
  (insert "\n"))


;; -----------------------------------------------------------------------------
;; interactive
;; -----------------------------------------------------------------------------

(defun iasm-disasm (file)
  (interactive "fObject file: ")
  (let ((buf (get-buffer-create (iasm-buffer-name file))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (iasm-buffer-header file)
        (iasm-mode)
        (iasm-objdump-run-syms file)
        (make-variable-buffer-local 'iasm-file)
        (setq iasm-file file)))
    (switch-to-buffer-other-window buf)))


(defun iasm-debug ()
  (interactive)
  (let ((inhibit-read-only t))
    (message "buf: %s" (current-buffer))
    (insert "\n\n")
    (unless iasm-syms-index (insert "NIL!\n"))
    (insert (format "%s" iasm-syms-index))))

;; -----------------------------------------------------------------------------
;; packaging
;; -----------------------------------------------------------------------------

(provide 'iasm-mode)
