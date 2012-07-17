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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; imenu
(when window-system
  (global-set-key [C-down-mouse-3] 'imenu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window specific settings.

(setq ring-bell-function (function (lambda ())))

(defvar my-font nil)

(when window-system
  (blink-cursor-mode 0)
  (set-scroll-bar-mode nil)
  (setq visible-bell t)
  (tool-bar-mode 0)
  (transient-mark-mode 0)

  (if is-win32 (setq my-font (window-build-font "Courier New" 9)))

  (window-set-frame-default 'auto-raise nil)
  (window-set-frame-default 'cursor-type 'box)
  (window-set-frame-default 'scroll-bar-width 12)

  ;; frame title
  (setq frame-title-format
        (concat "Emacs@"
                (if (string-match "^\\([^.]+\\)\..+" system-name)
                    (match-string 1 system-name)
                  system-name)
                " - %f")))

(when (or window-system (not is-win32))
  (setq font-lock-verbose 2048)
  (setq font-lock-maximum-decoration t)
  (global-font-lock-mode t))

;; settings from customzie-group
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; amd's color theme - turned off by default

(defun color-theme-amd ()
  (color-theme-install
   '(color-theme-amd
     ((background-color . "black")
      (foreground-color . "white")
      (cursor-color     . "yellow")
      (background-mode  . dark))
     
     (default      ((t (nil))))
     (fringe       ((t (                    :background "grey20"))))
     (modeline     ((t (:foreground "white" :background "darkslateblue"))))
     (region       ((t (                    :background "midnight blue"))))
     (highlight    ((t (                    :background "#13385b"))))
     
     (font-lock-builtin-face       ((t (:foreground "cornflower blue"))))
     (font-lock-comment-face       ((t (:foreground "green"))))
     (font-lock-doc-face           ((t (:foreground "green"))))
     (font-lock-constant-face      ((t (:foreground "gold"))))
     (font-lock-function-name-face ((t (:foreground "goldenrod" :bold t))))
     (font-lock-keyword-face       ((t (:foreground "DeepSkyBlue1"))))
     (font-lock-string-face        ((t (:foreground "red"))))
     (font-lock-type-face          ((t (:foreground "CadetBlue1" :bold t))))
     (font-lock-variable-name-face ((t (:foreground "SeaGreen2"))))
     (font-lock-warning-face       ((t (:foreground "Pink"))))

     )))

(defun color-theme-amd-win32 ()
  (color-theme-amd)
  (let ((color-theme-is-cumulative t))  
    (color-theme-install
     '(color-theme-amd-win32
       nil
       nil
       (font-lock-keyword-face       ((t (:foreground "cornflower blue"))))
       (font-lock-string-face        ((t (:foreground "tomato"))))
       (font-lock-warning-face       ((t (:foreground "cornflower blue"))))
       ))))

(defun color-theme-amd-linux ()
  (color-theme-amd)
  (let ((color-theme-is-cumulative t))  
    (color-theme-install
     '(color-theme-amd-win32
       ((background-color . "black"))
       nil
       (font-lock-string-face        ((t (:foreground "tomato"))))
       ))))

(defun color-theme-amd-linux-nw ()
  (color-theme-amd)
  (let ((color-theme-is-cumulative t))  
    (color-theme-install
     '(color-theme-amd-win32
       nil
       nil
       (font-lock-function-name-face ((t (:bold nil))))
       (font-lock-type-face          ((t (:foreground "cyan" :bold nil))))
       ))))

(defun turn-on-color-theme-amd ()
  "Turn on amd's colors."
  (interactive)
  (when (or window-system (not is-win32))
    (require 'color-theme)
    (cond
     (is-win32      (color-theme-amd-win32))
     (window-system (color-theme-amd-linux))
     (t             (color-theme-amd-linux-nw)))))
