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
  ;; (setq buffer-read-only t)

  (local-set-key (kbd "g") 'iasm-refresh)
  (local-set-key (kbd "s") 'iasm-show-ctx-at-point)
  (local-set-key (kbd "n") 'iasm-next-line)
  (local-set-key (kbd "p") 'iasm-previous-line)
  (local-set-key (kbd "j") 'iasm-jump-at-point)
  (local-set-key (kbd "TAB") 'iasm-toggle-section-at-point))

(defun iasm-buffer-name (file)
  (concat "*iasm " (file-name-nondirectory file) "*"))


(defun iasm-disasm-args (file)
  (cons (expand-file-name file) (split-string iasm-disasm-args " ")))

(defun iasm-set-current-ctx (line)
  (let ((split (split-string (match-string 1 line) ":")))
    (setq iasm-current-ctx-file (car split))
    (setq iasm-current-ctx-line (string-to-number (car (cdr split))))))

(defun iasm-create-section ()
  (let ((head-start iasm-current-header-start)
	(head-end iasm-current-header-end)
	(sec-start iasm-current-section-start)
	(sec-end (point)))
    (add-text-properties sec-start sec-end '(invisible t))
    (add-text-properties sec-start sec-end `(iasm-header ,head-start))
    (add-text-properties head-start head-end `(iasm-section-start ,sec-start))
    (add-text-properties head-start head-end `(iasm-section-end ,sec-end))))

(defun iasm-insert-header (line)
  (newline)
  (when iasm-current-header-start (iasm-create-section))
  (setq iasm-current-header-start (point))
  (insert line)
  (setq iasm-current-header-end (point))
  (newline)
  (setq iasm-current-section-start (point)))

(defun iasm-insert-inst (line)
  (let ((start (point))
	(addr (match-string 1 line)))
    (insert line)
    (newline)
    (add-text-properties start (point) `(iasm-ctx-file ,iasm-current-ctx-file))
    (add-text-properties start (point) `(iasm-ctx-line ,iasm-current-ctx-line))
    (add-text-properties start (point) `(iasm-addr ,addr))
    (when (string-match "\\([0-9a-f]+\\) <.*>$" line)
      (add-text-properties start (point) `(iasm-jump ,(match-string 1 line))))))

(defconst iasm-parse-table
  '(("^\\(/.+:[0-9]+\\)" . iasm-set-current-ctx)
    ("^[0-9a-f]+ <\\(.+\\)>:$" . iasm-insert-header)
    ("^ *\\([0-9a-f]+\\):" . iasm-insert-inst)))

(defun iasm-parse-line (line)
  (dolist (pair iasm-parse-table)
    (save-match-data
      (when (string-match (car pair) line)
	(apply (cdr pair) line '())))))

(defun iasm-exec (args)
  "The world's most inneficient way to process the output of a process."
  (let ((lines (apply 'process-lines iasm-objdump args)))
    (dolist (line lines) (iasm-parse-line line))
    (iasm-create-section)))

(defun iasm-disasm-into-buffer (file)
  (let ((args (iasm-disasm-args file)))
    (setq iasm-file file)
    (setq iasm-current-context nil)
    (setq iasm-current-header-start nil)
    (setq iasm-current-header-end nil)
    (setq iasm-current-section-start nil)
    (make-variable-buffer-local 'iasm-file)
    (make-variable-buffer-local 'iasm-current-context)
    (make-variable-buffer-local 'iasm-current-header-start)
    (make-variable-buffer-local 'iasm-current-header-end)
    (make-variable-buffer-local 'iasm-current-section-start)
    (message (format "Running: %s %s" iasm-objdump args))
    (erase-buffer)
    (iasm-exec args)
    (beginning-of-buffer)))

(defun iasm-disasm (file)
  (interactive "fObject file: ")
  (let ((buf (get-buffer-create (iasm-buffer-name file))))
    (with-current-buffer buf (iasm-disasm-into-buffer file))
    (switch-to-buffer-other-window buf)
    (with-current-buffer buf (iasm-mode))))

(defun iasm-refresh ()
  (interactive)
  (setq inhibit-read-only t)
  (iasm-disasm-into-buffer iasm-file)
  (setq inhibit-read-only nil)
  (iasm-mode))

(defun iasm-show-ctx-at-point ()
  (interactive)
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

(defun iasm-jump-at-point ()
  (interactive)
  (let ((addr (get-text-property (point) 'iasm-addr))
	(jump (get-text-property (point) 'iasm-jump)))
    (when (and addr jump)
      (let ((iaddr (string-to-number addr 16))
	    (ijump (string-to-number jump 16))
	    (search-str (format "^ *%s:" jump)))
	(when (> iaddr ijump)
	  (search-backward-regexp search-str))
	(when (< iaddr ijump)
	  (search-forward-regexp search-str)))
      (when (get-text-property (point) 'invisible)
	(let ((old-pos (point)))
	  (iasm-toggle-section-at-point)
	  (goto-char old-pos)))
      (beginning-of-line))))

(defun iasm-set-section-invisibility (value)
  (let ((sec-start (get-text-property (point) 'iasm-section-start))
	 (sec-end (get-text-property (point) 'iasm-section-end)))
    (when (and sec-start sec-end)
      (add-text-properties sec-start sec-end `(invisible ,value)))))

(defun iasm-toggle-section-at-point ()
  (interactive)
  (let ((header (get-text-property (point) 'iasm-header)))
    (if header
	(let ((value (if (get-text-property (point) 'invisible) nil t)))
	  (goto-char header)
	  (iasm-set-section-invisibility value))
      (let ((pos (get-text-property (point) 'iasm-section-start)))
	(when pos
	  (iasm-set-section-invisibility
	   (if (get-text-property pos 'invisible) nil t)))))))

(provide 'iasm-mode)
