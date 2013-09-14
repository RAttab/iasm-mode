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
  "Arguments few to the executable to retrieve symbol information"
  :group 'iasm
  :type 'string)

(defcustom iasm-disasm-args "-dlCw --no-show-raw-insn"
  "Arguments few to the executable to retrieve symbol information"
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
;; \todo bound checking
;; \todo balanced tree as the ds would also be nice.

(defstruct iasm-entry name addr pos size)


(defun iasm-index-shift (delta head tail)
  (when head
    (setf (iasm-entry-pos head) (+ delta (iasm-entry-pos head)))
    (iasm-index-shift delta (car tail) (cdr tail))
    (cons head tail)))


(defun iasm-index-add-impl (entry head tail)
  (let ((addr (iasm-entry-addr entry))
        (size (iasm-entry-size entry)))
    (if (and head (> addr (iasm-entry-addr head)))
        (cons head (iasm-index-add-impl entry (car tail) (cdr tail)))
      (cons entry (iasm-index-shift size head tail)))))

(defun iasm-index-add (index name addr pos size)
  (iasm-index-add-impl
   (make-iasm-entry :name name :addr addr :pos pos :size size)
   (car index) (cdr index)))


(defun iasm-index-update-size-impl (pos size head tail)
  (when head
    (if (and tail (> pos (iasm-entry-pos (car tail))))
        (cons head (iasm-index-update-size-impl pos size (car tail) (cdr tail)))
      (let ((delta (- size (iasm-entry-size head))))
        (setf (iasm-entry-size head) size)
        (cons head (iasm-index-shift delta (car tail) (cdr tail)))))))

(defun iasm-index-update-size (index pos size)
  (iasm-index-update-size-impl pos size (car index) (cdr index)))


(defun iasm-index-find-addr-impl (addr head tail)
  (when head
    (if (and tail (> addr (iasm-entry-addr (car tail))))
        (iasm-index-find-addr-impl addr (car tail) (cdr tail))
      head)))

(defun iasm-index-find-addr (index addr)
  (iasm-index-find-addr-impl addr (car index) (cdr index)))


;; -----------------------------------------------------------------------------
;; disasm parser
;; -----------------------------------------------------------------------------

(defun iasm-disasm-init (start end)
  )

(defun iasm-disasm-filter (line)
  (insert line "\n"))

(defun iasm-disasm-sentinel (line)
  (insert line "\n"))


;; -----------------------------------------------------------------------------
;; syms parser
;; -----------------------------------------------------------------------------

(defun iasm-syms-init ()
  )

(defun iasm-syms-filter (line)
  (insert line "\n"))

(defun iasm-syms-sentinel (line)
  (insert line "\n"))


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


(defun iasm-objdump-sentinel (proc state sentinel)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (save-excursion
        (let ((inhibit-read-only t))
          (end-of-buffer)
          (iasm-objdump-process-buffer sentinel))))))


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
       (iasm-objdump-sentinel proc state 'iasm-syms-sentinel)))))


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
       (iasm-objdump-sentinel proc state 'iasm-disasm-sentinel)))))


;; -----------------------------------------------------------------------------
;; buffer
;; -----------------------------------------------------------------------------

(defun iasm-buffer-name (file)
  (concat "*iasm " (file-name-nondirectory file) "*"))


(defun iasm-buffer-header (file)
  (insert (format "file:   %s\n" file))
  (insert (format
           "syms:   %s %s\n" iasm-objdump
           (mapconcat 'identity (iasm-objdump-syms-args-cons file) " ")))
  (insert (format
           "dasm:   %s %s\n" iasm-objdump
           (mapconcat 'identity (iasm-objdump-disasm-args-cons file 0 0) " "))))


(defun iasm-buffer-setup (file)
  (erase-buffer)
  (iasm-buffer-header file)
  (iasm-objdump-run-syms file))


;; -----------------------------------------------------------------------------
;; interactive
;; -----------------------------------------------------------------------------

(defun iasm-disasm (file)
  (interactive "fObject file: ")
  (let ((buf (get-buffer-create (iasm-buffer-name file))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (iasm-buffer-setup file)
        (iasm-mode)
        (make-variable-buffer-local 'iasm-file)
        (setq iasm-file file)))
    (switch-to-buffer-other-window buf)))


;; -----------------------------------------------------------------------------
;; packaging
;; -----------------------------------------------------------------------------

(provide 'iasm-mode)
