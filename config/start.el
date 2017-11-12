;; load all extra configuration scripts in the config directory

;; This should already be set in .emacs
(defvar CONFIGDIR nil)

(when (not CONFIGDIR)
  (error "You must set CONFIGDIR!"))

;; setup the paths so emacs can find all files in the config directory
(setq load-path (cons CONFIGDIR load-path))

(setq default-directory "~/")

;; update the default-directory to start in the elisp directory
(let ((old-dir default-directory))
  (unwind-protect
      (progn
        (setq default-directory CONFIGDIR)
        (normal-top-level-add-subdirs-to-load-path))
    (setq default-directory old-dir)))

;; running emacs on windows?
(setq is-win32 (memq system-type '(windows-nt ms-dos ms-windows)))

;; where is emacs
(setq source-directory (if is-win32 (getenv "emacs_dir") "/usr/share/emacs"))

;;(load "functions")
(load "packages")
(load "prefs")
(load "keys")

;; inhibit-startup-message is reset to nil right after this file is loaded resetting it
(if inhibit-startup-message
    (add-hook 'after-init-hook (lambda () (setq inhibit-startup-message t))))

'(load-path)
