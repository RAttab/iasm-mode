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

(defcustom iasm-executable "objdump"
  "Executable used to retrieve the assembly of an object file"
  :group 'iasm
  :type 'string)

(defcustom iasm-disasm-args ("--section=.text" "-dlCw")
  "Arguments fed to the executable to retrieve assembly information"
  :group 'iasm
  :type 'list)


;; -----------------------------------------------------------------------------
;; Useful stuff
;; -----------------------------------------------------------------------------

(defun iasm-mode ()
  "TODO: Do an actual mode..."
  (asm-mode)
  (toggle-truncate-lines t)
  (beginning-of-buffer))

(defun iasm-buffer-name (file)
  (concat "*iasm " (file-name-nondirectory file) "*"))

(defun iasm-disasm-arguments (file)
  (cons (expand-file-name file) iasm-disasm-args))

(defun iasm-open (file)
  ""
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'call-process iasm-executable nil t t
	     (iasm-disasm-arguments file)))))

(defun iasm-open-in-buffer (file)
  ""
  (let ((buf (get-buffer-create (iasm-buffer-name file))))
    (with-current-buffer buf
      (erase-buffer)
      (insert (iasm-open file))
      (switch-to-buffer-other-window buf)
      (iasm-mode))))
