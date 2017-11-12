;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting global key bindings

(defvar mode-list
  '(emacs-lisp-mode java-mode lisp-interaction-mode lisp-mode makefile-mode
                    perl-mode python-mode sgml-mode shell-mode shell-script-mode tetris-mode
                    c-mode-common text-mode fundamental-mode sql-mode sql-interactive-mode
                    generic-mode gud-mode bat-generic-mode properties-generic-mode p4-buffer-mode
                    nxml-mode)
  "List of all the modes that these key bindings should apply to.")

(defvar the-cc-modes '(c-mode c++-mode objc-mode csharp-mode java-mode idl-mode pike-mode)
  "List of the modes which are 'subclasses' of cc-mode")

(defun global-set-key-override (keys func &optional mode)
  (if (null mode)
      (global-set-key keys func))
  (if (null mode)
      (global-set-key keys func))
  (global-set-key-override0 keys func mode))

(defun global-set-key-override0 (keys func &optional mode)
  (let* ((the-mode (if (null mode) 'global-mode mode))
         (bindings (get 'global-key-overrides the-mode))
         (binding (assoc keys bindings)))
    (if (or (null bindings) (null binding))
        (setq bindings (cons (cons keys func) bindings))
      (setcdr binding func))
    (put 'global-key-overrides the-mode bindings))
  t)

(defun global-bindings-override-hook ()
  "Function that's called for the various major modes to override bindings."
  (message (format "Applying bindings for %s" major-mode))

  ;; first map global bindings
  (mapc (lambda (binding) (local-set-key (car binding) (cdr binding)))
        (get 'global-key-overrides 'global-mode))
  (mapc (lambda (binding) (local-set-key (car binding) (cdr binding)))
        (get 'global-key-overrides major-mode))

  ;; check to see if the major-mode is a subclass of the cc-modes, and
  ;; if so, invoke the binding overrides defined for c-mode-common
  (when (memq major-mode the-cc-modes)
    ;;(message "Applying common bindings for %s" major-mode)
    (mapc (lambda (binding) (local-set-key (car binding) (cdr binding)))
          (get 'global-key-overrides 'c-common-mode))))

;; Add our hook to all the defined hooks in 'mode-list'.
(mapc (lambda (mode)
        (add-hook (intern (concat (symbol-name mode) "-hook"))
                  'global-bindings-override-hook))
      mode-list)
(add-hook 'find-file-hooks 'global-bindings-override-hook)

(defun create-empty-buffer ()
  "Open a new frame with a buffer named Untitled. The buffer is not associated with a file."
  (interactive)
  (switch-to-buffer (generate-new-buffer "Untitled")))

;; personal preferences
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

;; make spell checker skip html
(setq
 ispell-extra-args '("--mode=sgml")
 ispell-program-name "/usr/local/bin/aspell"
 ispell-silently-savep t)
(set-default 'ispell-skip-html t)

;; cleanup make output
(setenv "TERM" "emacs")

;; you might want to customize these
(setq backup-inhibited t)
(setq backward-delete-char-untabify-method 'hungry)
(setq column-number-mode t)
(setq confirm-before-kill-emacs nil)
(setq line-number-mode t)
(setq require-final-newline nil)

(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 2)

(put 'eval-expression 'disabled nil)
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

(defalias 'qrr 'query-replace-regexp)

;; C-a start line
;; C-b backward character
;; C-c prefix
;; C-d delete character
;; C-e end of line
;; C-f forward character
;; C-g cancel / escape
;; C-h prefix / chord
(global-set-key "\C-h\C-k"  'describe-key)
(global-set-key "\C-i" 'indent-region)
;; C-j indent and newline
;; C-k kill line
(global-unset-key "\C-l")
;; C-m RET
;; C-n next line
;; C-o open line
;; C-p previous line
;; C-q quoted insert
;; C-r i-search backwards
(global-set-key "\C-s" 'isearch-forward-regexp)

;; Used as next windows globally
(global-unset-key "\C-t")

;; C-u universal argument
;; C-v page down
(global-set-key "\C-w" 'kill-region)
;; C-x prefix
;; C-xC-a, C-xa unbound
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-xb" 'electric-buffer-list)
(global-set-key "\C-x\C-c" 'save-buffers-kill-emacs)
(global-set-key "\C-xc" 'save-buffers-kill-emacs)
(global-set-key "\C-x\C-d" 'dired-other-window)
;; C-xd dired-other-window
;; C-xC-e eval-last-sexp
(global-set-key "\C-xe" 'eval-last-sexp)
(global-set-key "\C-xf" 'find-file)
(global-set-key "\C-x\C-f" 'find-file)
(global-set-key "\C-x\C-g" 'grep)
(global-set-key "\C-xg" 'grep)
(global-set-key "\C-x\C-h" 'mark-whole-buffer)
;; C-xh mark-whole-buffer
(global-set-key "\C-x\C-i" 'indent-region)
(global-set-key "\C-xi" 'indent-region)
;; C-xC-j, Cxj unbound
(global-set-key "\C-x\C-k" 'kill-buffer)
(global-set-key "\C-xk" 'kill-buffer)
(global-unset-key "\C-x\C-l")
(global-unset-key "\C-xl")
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-unset-key "\C-x\C-n")
(global-unset-key "\C-xn")
(global-set-key "\C-x\C-o" 'other-window)
;; C-xo other-window
(global-set-key "\C-x\C-p" 'query-replace-regexp)
(global-set-key "\C-xp" 'query-replace-regexp)
(global-unset-key "\C-x\C-q")
(global-unset-key "\C-xq")
(global-set-key "\C-x\C-r" 'comment-region)
(global-set-key "\C-xr" 'comment-region)
(global-set-key "\C-x\C-s" 'save-buffer)
(global-set-key "\C-xs" 'save-buffer)
(global-unset-key "\C-x\C-t")
(global-unset-key "\C-xt")
(global-set-key "\C-x\C-u" 'advertised-undo)
(global-set-key "\C-xu" 'advertised-undo)
(global-unset-key "\C-x\C-v")
(global-unset-key "\C-xv")
;; C-xC-w write-file
(global-set-key "\C-xw"  'write-file)
(global-unset-key "\C-x\C-x")
(global-unset-key "\C-xx")
;; C-xC-y, C-xy unset
(global-unset-key "\C-x\C-z")
(global-unset-key "\C-xz")

(global-unset-key "\M-a")
(global-unset-key "\M-b")
(global-unset-key "\M-c")
(global-unset-key "\M-d")
(global-unset-key "\M-e")
;; M-g goto-line
(global-unset-key "\M-h")
(global-unset-key "\M-i")
(global-unset-key "\M-j")
(global-unset-key "\M-k")
(global-unset-key "\M-l")
(global-unset-key "\M-m")
;; M-n new-buffer
(global-unset-key "\M-o")
(global-unset-key "\M-q")
;; M-p used in emacs console
(global-unset-key "\M-r")
(global-unset-key "\M-s")
(global-unset-key "\M-t")
(global-unset-key "\M-u")
;; M-v pageup
(global-unset-key "\M-w")
;; M-x M-x
;; M-y yank pop
(global-unset-key "\M-z")

;; C-y yank (paste)
(global-unset-key "\C-z")

(global-set-key [C-backspace]   'backward-kill-word)
(global-set-key [C-kp-right]    'indent-for-tab-command)
(global-set-key [C-right]       'indent-for-tab-command)
(global-set-key [C-tab]         'abtags-find-next-file)
(global-set-key-override "\177" 'backward-delete-char-untabify)

(global-set-key [home]          'beginning-of-line)
(global-set-key [end]           'end-of-line)
(global-set-key [C-home]        'beginning-of-line)
(global-set-key [C-end]         'end-of-line)
(global-set-key [C-left]        'backward-word)
(global-set-key [C-up]          'previous-line)
(global-set-key [C-down]        'next-line)
(global-set-key [C-kp-up]       'previous-line)
(global-set-key [C-kp-down]     'next-line)
(global-set-key [C-kp-left]     'backward-word)

(global-set-key-override "\t" 'comint-dynamic-complete 'shell-mode)

; mini-buffer
(define-key minibuffer-local-map "\t" 'hippie-expand)

; imenu
(when window-system
  (global-set-key [C-down-mouse-3] 'imenu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window specific settings.

(setq ring-bell-function (function (lambda ())))

(defvar my-font nil)

;; (when (or window-system (not is-win32))
;;   (setq font-lock-verbose 2048)
;;   (setq font-lock-maximum-decoration t)
;;   (global-font-lock-mode t))

;; settings from customzie-group
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(coffee-tab-width 2)
 '(css-indent-offset 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-enter-indents-newline t)
 '(scss-compile-at-save nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; colors and themes
(require 'color-theme)

;; shell colors
(setq ansi-color-names-vector
      ["black" "tomato" "PaleGreen2" "gold1"
       "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])

(setq ansi-color-map (ansi-color-make-color-map))

;; Don't tabify after rectangle
(setq cua-auto-tabify-rectangles nil)
;; No region when it is not highlighted
(transient-mark-mode 1)
;; Standard Windows behaviour
(setq cua-keep-region-after-copy t)
