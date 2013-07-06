;; -----------------------------------------------------------------------------
;; iasm-mode.el
;; Rémi Attab (remi.attab@gmail.com), 01 Jun 2013
;; FreeBSD-style copyright and disclaimer apply
;;
;; Interactive assembly mode for almighty emacs.
;;
;; The idea is grab the output of objdump, format it and make it interactive.
;; Let's hope it all works out.
;; -----------------------------------------------------------------------------


;; -----------------------------------------------------------------------------
;; Custom
;; -----------------------------------------------------------------------------

(defgroup iasm nil
  "Interactive assembly mode"
  :prefix "iasm-"
  :group 'tools)

(defcustom iasm-objdump "objdump"
  "Executable used to retrieve the assembly of an object file"
  :group 'iasm
  :type 'string)

(defcustom iasm-disasm-args "-dlCwj .text --no-show-raw-insn"
  "Arguments fed to the executable to retrieve assembly information"
  :group 'iasm
  :type 'string)


;; -----------------------------------------------------------------------------
;; Useful stuff
;; -----------------------------------------------------------------------------


(define-derived-mode iasm-mode asm-mode
  "iasm"
  "BLAH!
\\{iasm-mode-map}"
  :group 'iasm
  (toggle-truncate-lines t)
  (beginning-of-buffer)

  (local-set-key (kbd "g") 'iasm-refresh)
  (local-set-key (kbd "n") 'iasm-next-line)
  (local-set-key (kbd "p") 'iasm-previous-line))

(defun iasm-buffer-name (file)
  (concat "*iasm " (file-name-nondirectory file) "*"))


(defun iasm-disasm-args (file)
  (cons (expand-file-name file) (split-string iasm-disasm-args " ")))

(defun iasm-set-current-ctx (line)
  (let ((split (split-string (match-string 1 line) ":")))
    (setq iasm-current-ctx-file (car split))
    (setq iasm-current-ctx-line (string-to-number (car (cdr split))))))

(defun iasm-insert-header (line)
  (newline)
  (insert line)
  (newline))

(defun iasm-insert-inst (line)
  (let ((start (point)))
    (insert line)
    (newline)
    (add-text-properties start (point) `(iasm-ctx-file ,iasm-current-ctx-file))
    (add-text-properties start (point) `(iasm-ctx-line ,iasm-current-ctx-line))))

(defconst iasm-parse-table
  '(("^\\([/a-zA-Z0-9\\._-]*:[0-9]*\\)" . iasm-set-current-ctx)
    ("^[0-9a-f]* <\\(.*\\)>:$" . iasm-insert-header)
    ("^ *[0-9a-f]*:" . iasm-insert-inst)))

(defun iasm-parse-line (line)
  (dolist (pair iasm-parse-table)
    (save-match-data
      (when (string-match (car pair) line)
	(apply (cdr pair) line '())))))

(defun iasm-exec (buf args)
  "The world's most inneficient way to process the output of a process."
  (let ((lines (apply 'process-lines iasm-objdump args)))
    (with-current-buffer buf
      (setq iasm-current-context nil)
      (make-variable-buffer-local 'iasm-current-context)
      (dolist (line lines)
	(iasm-parse-line line)))))

(defun iasm-disasm-into (file buf)
  (let ((args (iasm-disasm-args file)))
    (with-current-buffer buf
      (erase-buffer)
      (setq iasm-file file)
      (make-variable-buffer-local 'iasm-file))
    (message (format "Running: %s %s" iasm-objdump args))
    (iasm-exec buf args)
    (beginning-of-buffer)))

(defun iasm-disasm (file)
  (interactive "fObject file: ")
  (let ((buf (get-buffer-create (iasm-buffer-name file))))
    (iasm-disasm-into file buf)
    (switch-to-buffer-other-window buf)
    (with-current-buffer buf (iasm-mode))))

(defun iasm-refresh ()
  (interactive)
  (iasm-disasm-into iasm-file (current-buffer))
  (iasm-mode))

(defun iasm-next-line ()
  (interactive)
  (next-line)
  (let ((file (get-text-property (point) 'iasm-ctx-file))
	(line (get-text-property (point) 'iasm-ctx-line))
	(iasm-buf (current-buffer)))
    (when (and file line)
      (find-file-other-window file)
      (goto-line line)
      (pop-to-buffer iasm-buf))))

(defun iasm-show-ctx-at-point ()
  (let ((file (get-text-property (point) 'iasm-ctx-file))
	(line (get-text-property (point) 'iasm-ctx-line))
	(iasm-buf (current-buffer)))
    (when (and file line)
      (find-file-other-window file)
      (goto-line line)
      (pop-to-buffer iasm-buf))))

(defun iasm-next-line ()
  (interactive)
  (next-line)
  (iasm-show-ctx-at-point))

(defun iasm-previous-line ()
  (interactive)
  (previous-line)
  (iasm-show-ctx-at-point))

(provide 'iasm-mode)
