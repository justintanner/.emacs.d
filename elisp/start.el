;; load all extra configuration scripts in the elisp directory
(defvar JWTELISP nil)

(when (not JWTELISP)
  (error "You must set JWTELISP!"))

;; setup the paths so emacs can find all files in the elisp directory
(setq load-path (cons JWTELISP load-path))

;; update the default-directory to start in the elisp directory
(let ((old-dir default-directory))
  (unwind-protect
      (progn
        (setq default-directory JWTELISP)
        (normal-top-level-add-subdirs-to-load-path))
    (setq default-directory old-dir)))

;; running emacs on windows?
(setq is-win32 (memq system-type '(windows-nt ms-dos ms-windows)))

;; where is emacs
(setq source-directory (if is-win32 (getenv "emacs_dir") "/usr/share/emacs"))

(load "amdelisp")
(load "modes")
(when window-system
  (load "window"))
(when (file-exists-p "company.el")
  (load "company"))
(load "prefs")
(load (format "%s/lisp/loaddefs" JWTELISP))

;; inhibit-startup-message is reset to nil right after this file is loaded resetting it
(if inhibit-startup-message
    (add-hook 'after-init-hook (lambda () (setq inhibit-startup-message t))))

(when (not is-win32)
  (load "linux"))
