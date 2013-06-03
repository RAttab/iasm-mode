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

(defun iasm-mode ()
  "TODO: Do an actual mode..."
  (asm-mode)
  (toggle-truncate-lines t)
  (beginning-of-buffer))

(defun iasm-buffer-name (file)
  (concat "*iasm " (file-name-nondirectory file) "*"))

(defun iasm-disasm-cmd (file)
  (format "%s %s %s" iasm-objdump iasm-disasm-args (expand-file-name file)))

(defun iasm-disasm (file)
  ""
  (let ((buf (get-buffer-create (iasm-buffer-name file)))
	(cmd (iasm-disasm-cmd file)))
    (message (format "Running: %s" cmd))
    (shell-command cmd buf)
    (switch-to-buffer-other-window buf)
    (with-current-buffer buf (iasm-mode))))

