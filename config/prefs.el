;; turn off the menubar
(menu-bar-mode 0)

;; so you can see what area you have selected
(transient-mark-mode t)

;; highlight paranthesis automatically
(show-paren-mode t)

;; TODO: see which of these seetings I still need.
(setq PC-word-delimiters "-_.=")
(setq auto-revert-interval 2)
(setq auto-save-list-file-prefix nil)
(setq backup-by-copying t)
(setq tab-width 2)
(setq display-time-format nil)
(setq fill-column 60)
(setq gc-cons-threshold 200000)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq jit-lock-stealth-time 1)
(setq jit-lock-stealth-nice 0.5)
(setq jit-lock-defer-contextually t)
(setq line-number-display-limit 3000000)
(setq message-log-max 200)
(setq save-abbrevs nil)
(setq speedbar-track-mouse-flag nil)
(setq track-eol nil)
(setq truncate-partial-width-windows nil)
(setq w32-use-full-screen-buffer nil)
(setq backup-inhibited t)
(setq backward-delete-char-untabify-method 'hungry)
(setq column-number-mode t)
(setq confirm-before-kill-emacs nil)
(setq line-number-mode t)
(setq require-final-newline nil)

;; make spell checker skip html
(setq
 ispell-extra-args '("--mode=sgml")
 ispell-program-name "/usr/local/bin/aspell"
 ispell-silently-savep t)
(set-default 'ispell-skip-html t)

;; cleanup make output
(setenv "TERM" "emacs")

(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 2)
