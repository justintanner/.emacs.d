;; justin tanner's emacs config file
(defvar JWTELISP (format "%s/.emacs.d/custom" (getenv "HOME")))

(load (format "%s/start" JWTELISP))

;; workaround for windows bug
(require 'server)
(server-start)

;; turn off the menubar
(menu-bar-mode 0)

;; so you can see what area you have selected
(transient-mark-mode t)

;; highlight paranthesis automatically
(show-paren-mode t)

;; what are these for (?)
(setq
 abtags-keymap-prefix nil
 backward-delete-char-untabify-method 'all
 comint-input-ring-size 99
 completion-ignore-case t
 html-helper-do-write-file-hooks nil
 shell-dirtrack-verbose nil
 sort-fold-case t
 sql-oracle-program "sqlplus"
 tags-add-tables t)

;; use TAB key for completion everywhere ( dont need these? )
(global-set-key-override0 "\t" 'clever-hippie-tab)
(global-set-key-override  "\t" 'clever-nxml-tab 'nxml-mode)

;; tab completion (?)
(when is-win32
  (define-key minibuffer-local-must-match-filename-map "\t" 'minibuffer-complete)
  (define-key minibuffer-local-filename-completion-map "\t" 'minibuffer-complete)
  (setq read-file-name-completion-ignore-case t))
