;; -----------------------------------------------------------------------------
;; iasm-mode.el
;; Rémi Attab (remi.attab@gmail.com), 01 Jun 2013
;; FreeBSD-style copyright and disclaimer apply
;;
;; Interactive assembly mode for almighty emacs.
;;
;; The idea is grab the output of objdump, format it and make it interactive.
;; Note that this file probably contains some horrible abuse and misuse of
;; emacs features. I regret nothing!
;;
;; TODO:
;; - Speed up buffer generation by making use of async process and filters.
;; - Figure out how to better show context information.
;; - Rename half the horribly named functions.
;; - iasm-collapse-all-sections: text properties ain't gonna work well for this.
;; - Spruce up the mode:
;;   - Proper key binding table thingy
;;   - Some sort of highlighting?
;; - Add support for ldd.
;; - Get real fancy.
;; - Documentation... just kiddin!
;;
;; -----------------------------------------------------------------------------


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


(defcustom iasm-disasm-args "-dlCwj .text --no-show-raw-insn"
  "Arguments fed to the executable to retrieve assembly information"
  :group 'iasm
  :type 'string)


;; -----------------------------------------------------------------------------
;; Mode setup
;; -----------------------------------------------------------------------------

(define-derived-mode iasm-mode asm-mode
  "iasm"
  "BLAH!
\\{iasm-mode-map}"
  :group 'iasm
  (toggle-truncate-lines t)
  (beginning-of-buffer)

  (local-set-key (kbd "S") 'iasm-dbg-at-point)
  (local-set-key (kbd "g") 'iasm-refresh)
  (local-set-key (kbd "s") 'iasm-show-ctx-at-point)
  (local-set-key (kbd "n") 'iasm-next-line)
  (local-set-key (kbd "p") 'iasm-previous-line)
  (local-set-key (kbd "M-n") 'iasm-next-section)
  (local-set-key (kbd "M-p") 'iasm-previous-section)
  (local-set-key (kbd "j") 'iasm-jump-at-point)
  (local-set-key (kbd "TAB") 'iasm-toggle-section-at-point))


;; -----------------------------------------------------------------------------
;; Parser
;; -----------------------------------------------------------------------------

(defun iasm-set-current-ctx (line)
  (let ((split (split-string (match-string 1 line) ":")))
    (setq iasm-current-ctx-file (car split))
    (setq iasm-current-ctx-line (string-to-number (car (cdr split))))))


(defun iasm-set-current-ctx-fun (line)
  (setq iasm-current-ctx-fun (match-string 1 line)))


(defun iasm-create-section ()
  (let ((head-start  iasm-current-header-start)
        (head-end    iasm-current-header-end)
        (sec-start   iasm-current-section-start)
        (sec-end     (point)))
    (add-text-properties sec-start sec-end '(invisible t))
    (add-text-properties sec-start sec-end `(iasm-header ,head-start))
    (add-text-properties head-start head-end `(iasm-section-start ,sec-start))
    (add-text-properties head-start head-end `(iasm-section-end ,sec-end))))


(defun iasm-insert-header (line)
  (when iasm-current-header-start (iasm-create-section))
  (setq iasm-current-header-start (point))
  (insert " \n")
  (insert line)
  (setq iasm-current-header-end (point))
  (setq iasm-current-section-start (point))
  (insert " \n"))


(defun iasm-insert-inst (line)
  (let ((start (point))
        (addr (match-string 1 line)))
    (insert line)
    (insert " \n")
    (add-text-properties start (point) `(iasm-ctx-file ,iasm-current-ctx-file))
    (add-text-properties start (point) `(iasm-ctx-line ,iasm-current-ctx-line))
    (add-text-properties start (point) `(iasm-ctx-fun ,iasm-current-ctx-fun))
    (add-text-properties start (point) `(iasm-addr ,addr))
    (when (string-match "\\([0-9a-f]+\\) <.*>$" line)
      (add-text-properties start (point) `(iasm-jump ,(match-string 1 line))))))


(defconst iasm-parse-table
  '(("^\\(/.+:[0-9]+\\)"       . iasm-set-current-ctx)
    ("^\\(.+\\):$"             . iasm-set-current-ctx-fun)
    ("^[0-9a-f]+ <\\(.+\\)>:$" . iasm-insert-header)
    ("^ *\\([0-9a-f]+\\):"     . iasm-insert-inst)))


(defun iasm-parse-line (line)
  (dolist (pair iasm-parse-table)
    (save-match-data
      (when (string-match (car pair) line)
        (apply (cdr pair) line '())))))


(defun iasm-init-parser ()
  (make-variable-buffer-local 'iasm-current-ctx-file)
  (setq iasm-current-ctx-file nil)

  (make-variable-buffer-local 'iasm-current-ctx-line)
  (setq iasm-current-ctx-line nil)

  (make-variable-buffer-local 'iasm-current-ctx-fun)
  (setq iasm-current-ctx-fun nil)

  (make-variable-buffer-local 'iasm-current-header-start)
  (setq iasm-current-header-start nil)

  (make-variable-buffer-local 'iasm-current-header-end)
  (setq iasm-current-header-end nil)

  (make-variable-buffer-local 'iasm-current-section-start)
  (setq iasm-current-section-start nil))


;; -----------------------------------------------------------------------------
;; Utils
;; -----------------------------------------------------------------------------

(defun iasm-buffer-name (file)
  (concat "*iasm " (file-name-nondirectory file) "*"))


(defun iasm-disasm-args (file)
  (append (split-string iasm-disasm-args " ") `(,(expand-file-name file))))


(defun iasm-disasm-into-buffer (file)
  (let ((args (iasm-disasm-args file)))
    (make-variable-buffer-local 'iasm-file)
    (setq iasm-file file)
    (iasm-init-parser)
    (message (format "Running: %s %s" iasm-objdump args))
    (erase-buffer)
    (insert (format "%s " iasm-objdump))
    (dolist (arg args) (insert (format "%s " arg)))
    (insert " \n")
    (let ((lines (apply 'process-lines iasm-objdump args)))
      (dolist (line lines) (iasm-parse-line line))
      (iasm-create-section))
    (beginning-of-buffer)))


(defun iasm-set-section-invisibility (value)
  (let ((sec-start (get-text-property (point) 'iasm-section-start))
        (sec-end   (get-text-property (point) 'iasm-section-end)))
    (when (and sec-start sec-end)
      (add-text-properties sec-start sec-end `(invisible ,value)))))


;; -----------------------------------------------------------------------------
;; Interface
;; -----------------------------------------------------------------------------

(defun iasm-disasm (file)
  (interactive "fObject file: ")
  (let ((buf (get-buffer-create (iasm-buffer-name file))))
    (with-current-buffer buf (iasm-disasm-into-buffer file))
    (switch-to-buffer-other-window buf)
    (with-current-buffer buf (iasm-mode))))


(defun iasm-refresh ()
  (interactive)
  (iasm-disasm-into-buffer iasm-file)
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


(defconst iasm-section-jump-regexp "^[0-9a-f]+ <.+>:")

(defun iasm-next-section ()
  (interactive)
  (next-line)
  (beginning-of-line)
  (search-forward-regexp iasm-section-jump-regexp)
  (beginning-of-line))

(defun iasm-previous-section ()
  (interactive)
  (previous-line)
  (end-of-line)
  (search-backward-regexp iasm-section-jump-regexp)
  (beginning-of-line))


(defun iasm-jump-at-point ()
  (interactive)
  (push-mark)
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


(defun iasm-dbg-at-point ()
  (interactive)
  (let ((header    (get-text-property (point) 'iasm-header))
        (sec-start (get-text-property (point) 'iasm-section-start))
        (sec-end   (get-text-property (point) 'iasm-section-end))
        (invisible (get-text-property (point) 'invisible))
        (jump      (get-text-property (point) 'iasm-jump))
        (ctx-file  (get-text-property (point) 'iasm-ctx-file))
        (ctx-line  (get-text-property (point) 'iasm-ctx-line))
        (ctx-fun   (get-text-property (point) 'iasm-ctx-fun)))
    (message "iasm-dbg: pos=%s header=%s section=[%s, %s] invisible=%s jump=%s ctx=%s:%s:%s"
             (point) header sec-start sec-end invisible jump ctx-file ctx-line ctx-fun)))


(provide 'iasm-mode)
