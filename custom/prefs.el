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

;; personal preferences
(setq PC-word-delimiters "-_.=")
(setq auto-revert-interval 2)
(setq auto-save-list-file-prefix nil)
(setq backup-by-copying t)
(setq tab-width 2)
(setq display-time-format nil)
(setq file-name-buffer-file-type-alist '(("\\.cgi$" . t)))
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
;;(setq printer-name nil)
(setq require-final-newline nil)

(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 2)

(put 'eval-expression 'disabled nil)
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; editing

(when (not is-win32)
  (keyboard-translate ?\C-h ?\C-?))

(global-set-key "\C-\\"         'advertised-undo)
(global-set-key "\C-c\C-c"      'comment-region)
(global-set-key "\C-c\C-u"      'uncomment-region)
(global-set-key "\C-m"          'newline-and-indent)
(global-set-key "\C-x."         'find-tag)
(global-set-key "\C-x\C-b"      'electric-buffer-list)
(global-set-key "\M-."          'find-tag-non-interactive)
(global-set-key "\M-;"          'tags-return)
(global-set-key "\M-g"          'goto-line)
(global-set-key [C-backspace]   'backward-kill-word)
(global-set-key [C-kp-right]    'indent-for-tab-command)
(global-set-key [C-right]       'indent-for-tab-command)
(global-set-key [C-tab]         'abtags-find-next-file)
(global-set-key-override "\177" 'backward-delete-char-untabify)

;; mini-buffer
(define-key minibuffer-local-map "\t" 'hippie-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; movement

(global-set-key "\M-z"          'pager-row-up)
(global-set-key "\C-z"          'pager-row-down)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mistakes

(global-set-key "\C-xf"     'find-file)
(global-set-key "\C-x\C-f"  'find-file)
(global-set-key "\C-xs"     'save-buffer)
(global-set-key "\C-x\C-s"  'save-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compilation

(global-set-key "\M-m"      'make)
(global-set-key "\M-p"      'make-remake)
(global-set-key-override "\M-s" 'make-magic)
(global-set-key [M-up]      'previous-error)
(global-set-key [M-down]    'next-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; java/cpp

(global-set-key-override "\C-cr"    'repackage  'java-mode)
(global-set-key-override "\C-c\C-r" 'repackage  'java-mode)
(global-set-key-override "\C-cj"    'jdok-generate-javadoc-template 'java-mode)
(global-set-key-override "\C-c\C-j" 'jdok-generate-javadoc-template 'java-mode)
(global-set-key-override "\C-ct"    'java-trace-method 'java-mode)
(global-set-key-override "\C-c\C-t" 'java-trace-method 'java-mode)
(global-set-key-override "\C-cp"    'java-trace-ctor 'java-mode)
(global-set-key-override "\C-c\C-p" 'java-trace-ctor 'java-mode)
(global-set-key-override "\C-ct"    'cpp-trace-method 'c++-mode)
(global-set-key-override "\C-c\C-t" 'cpp-trace-method 'c++-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shell

(global-set-key-override "\t" 'comint-dynamic-complete 'shell-mode)
(global-set-key-override "\C-c\C-c" 'comint-interrupt-subjob 'shell-mode)

(global-set-key-override "\C-x\C-g" 'grep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; imenu
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
