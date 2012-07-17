;; justin tanner's emacs config file
(defvar JWTELISP (format "%s/emacs" (getenv "HOME")))
(load (format "%s/start" JWTELISP))

;; workaround for windows bug  
(require 'server)
(server-start)

(require 'color-theme)

(if window-system
    (color-theme-vim-colors)
  (turn-on-color-theme-amd))

(when window-system
  (when is-win32
    (setq my-font (window-build-font "Fixedsys" 9)))
  (mouse-wheel-mode t)
  (blink-cursor-mode 1))

;; menubars suck
(menu-bar-mode 0)

;; so you can see what area you have selected
(transient-mark-mode t)

;; tab complete everything
(require 'complete)
(partial-completion-mode t)

;; make spell checker skip html
(setq
 ispell-extra-args '("--mode=sgml")
 ispell-program-name "aspell"
 ispell-silently-savep t)
(set-default 'ispell-skip-html t)

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

;; special key bindings
(defalias 'qrr 'query-replace-regexp)
(global-set-key [f5]  'call-last-kbd-macro)
(global-set-key [f7]  'abtags-find-file)
(global-set-key [f8]  'grep)
(global-set-key [f12] 'next-error)
(global-set-key "\C-xb" 'switch-to-buffer-nocreate)
(global-set-key "\C-\M-q" 'backward-up-list-indent)
(global-set-key "\M-," 'tags-search-tags-table)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'copy-region-as-kill)
(global-set-key "\C-i" 'indent-region)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-cs" 'secret-commit)
(global-set-key "\C-c\C-s" 'secret-commit)
(global-set-key "\C-c\C-f" 'doxymacs-insert-file-comment)
(global-set-key "\C-c\C-g" 'doxymacs-insert-function-comment)

;; use TAB key for completion everywhere ( dont need these? )
(global-set-key-override0 "\t" 'clever-hippie-tab)
(global-set-key-override  "\t" 'clever-nxml-tab 'nxml-mode)

;; tab completion (?)
(when is-win32
  (define-key minibuffer-local-must-match-filename-map "\t" 'minibuffer-complete)
  (define-key minibuffer-local-filename-completion-map "\t" 'minibuffer-complete)
  (setq read-file-name-completion-ignore-case t))
