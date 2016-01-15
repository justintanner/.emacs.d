;; hooks and other setup variables for many different modes

;; delete trailing whitespace before saving any file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; what does this function do?
(defun autoloads (file &rest funcs)
  "A helper function written by jp that lets you autoload many
functions from one source file."
  (let ((result))
    (while (not (null funcs))
      (let ((func-string (format "%s" (car funcs))))
        (setq result (cons `(autoload (quote ,(car funcs))
                              ,file
                              ,func-string
                              (quote ,(car funcs)))
                           result)))
      (setq funcs (cdr funcs)))
    (eval `(progn ,@result))))

;; amd's etags setup
(setq tags-revert-without-query t)
(defun my-etags-setup ()
  (setq case-fold-search nil))
(eval-after-load "etags"
  '(add-hook 'tags-table-format-hooks 'my-etags-setup))

;; ansi-color
(autoloads "ansi-color"
           'ansi-color-for-comint-mode-on
           'ansi-color-apply-on-region)

;; grep defaults
(setq grep-command "grep -n -s -i -r --exclude=\*{TAGS,.svg,.log,.png,.jpg,*.o} \"")

;; ansi colors for grep - makes --color=auto work
(eval-after-load "compile"
  '(defun grep-process-setup ()
     "Set up `compilation-exit-message-function' for `grep'."
     (set (make-local-variable 'compilation-exit-message-function)
          (lambda (status code msg)
            (require 'ansi-color)
            (ansi-color-apply-on-region (point-min) (point-max))
            (if (eq status 'exit)
                (cond ((zerop code)
                       '("finished (matches found)\n" . "matched"))
                      ((= code 1)
                       '("finished with no matches found\n" . "no match"))
                      (t
                       (cons msg code)))
              (cons msg code))))))

;; smart shift
(require 'smart-shift)
(global-smart-shift-mode 1)

;; text
(defun my-text-setup ()
  (if (eq indent-line-function 'indent-to-left-margin)
      (setq indent-line-function 'indent-relative-maybe)))
(add-hook 'text-mode-hook 'my-text-setup)

;; c
(setq c-default-style '((c-mode . "k&r")))

;; perl
(eval-when-compile (require 'perl-mode))
(defun my-perl-setup ()
  (setq tab-width 4))
(add-hook 'perl-mode-hook 'my-perl-setup)

;; tcl
(eval-when-compile (require 'tcl))
(defun my-tcl-setup ()
  (setq indent-tabs-mode nil))
(add-hook 'tcl-mode-hook 'my-tcl-setup)

;; makefile
(eval-when-compile (require 'make-mode))
(defun my-makefile-setup ()
  (define-key makefile-mode-map "\M-m" 'compile-make)
  (define-key makefile-mode-map "\M-p" 'compile-re-make))
(add-hook 'makefile-mode-hook 'my-makefile-setup)
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\(defs\\|rules\\)$" . makefile-mode))

;; imenu
(setq imenu-sort-function 'imenu--sort-by-name)

;; ascii
(eval-after-load "ascii"
  '(set-face-background 'ascii-ascii-face (face-background 'region)))

;; highlight while completing
(defun make-completion-re (re) (concat "\\<" re "\\([ \n\t]\\|\\'\\)"))
(defun make-prefix-re (prefix) (make-completion-re (concat prefix "[^ \n\t]*")))
(defun make-exact-re  (words)  (make-completion-re words))
(defun make-suffix-re (suffix) (make-completion-re (concat "[^ \n\t]+" suffix)))

(defun setup-completion-keywords ()
  (when window-system
    (let* (
           (ext-1
            (eval-when-compile
              (regexp-opt '("js" "h" "h++" "hh" "hpp" "hxx"
                            "bat" "sys" "eps" "ps" "inf" "ini" "reg"
                            "btm" "conf" "conf.in" "properties"
                            "text" "txt") t)))
           (ext-2
            (eval-when-compile
              (regexp-opt '("mak" "jam" "jspi" "el" "pl" "cgi"
                            "c" "c++" "cc" "cpp" "cs" "cxx" "log"
                            "java" "tag" "tld" "xsl" "wml" "dtd" "mk"
                            "rb") t)))
           (ext-3
            (eval-when-compile
              (regexp-opt '("css" "py" "htm" "html" "arc" "jar" "lzh"
                            "sql" "zip" "zoo" "tar" "shtml" "shtm"
                            "asp" "idl" "jsp" "xml" "rhtml") t)))
           (exact-1
            (eval-when-compile
              (regexp-opt '("makefile" "gnumakefile" "makefile.in") t)))
           )
      (setq completion-added-font-lock-keywords
            (list
             (cons "\\<[^ \n]*/" 'font-lock-builtin-face)
             (cons (make-suffix-re (concat "\\." ext-1)) 'font-lock-constant-face)
             (cons (make-suffix-re (concat "\\." ext-2)) 'font-lock-type-face)
             (cons (make-suffix-re (concat "\\." ext-3)) 'font-lock-comment-face)
             (cons (make-exact-re exact-1) 'font-lock-constant-face)
             )))))

(when window-system
  (add-hook 'after-init-hook 'setup-completion-keywords))

;; apache
(add-to-list 'auto-mode-alist '("\\my.cnf$" . apache-mode))

;; zip support
(setq archive-zip-use-pkzip nil)
(add-to-list 'auto-mode-alist '("\\.war$" . archive-mode))

;; shell-script-mode
(add-to-list
 'auto-mode-alist
 (cons
  (format "/%s$"
          (eval-when-compile
            (regexp-opt '(".bash_logout" ".bash_profile" ".bashrc" ".cshrc"
                          ".inputrc" "bashrc" "csh.cshrc" "csh.login" "profile"))))
  'shell-script-mode))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'auto-mode-alist '("\\.\\(tcsh\\|bash\\)$" . shell-script-mode))


;; abtags ( removing this ? )
(autoload 'abtags-key-map "abtags" "abtags-key-map" nil `keymap)

;; color-theme
(autoload 'color-theme-select "color-theme" "color-theme" t)

;; antlr
(autoload 'antlr-mode "antlr-mode" nil t)
(setq auto-mode-alist (cons '("\\.g\\'" . antlr-mode) auto-mode-alist))

;; turn in image modes
(auto-image-file-mode t)

;; go
(add-to-list 'load-path (format "%s/emacslib/go-mode-load.el" JWTELISP) t)
(require 'go-mode-load)

;; css
(autoload 'css-mode "css-mode" "css-mode" t)

;; python
(defun my-python-mode-hook ()
  (global-set-key "\C-c\C-k" 'copy-region-as-kill))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(auto-fill-mode 0)

;; haml mode
(eval-when-compile (require 'haml-mode))
(defun my-haml-setup ()
  (make-local-variable 'standard-indent)
  (setq standard-indent 2)
  (define-key haml-mode-map [C-left] 'my-decrease)
  (define-key haml-mode-map [C-right] 'my-increase)
  (setq haml-backspace-backdents-nesting nil)
  (modify-syntax-entry ?_ "." haml-mode-syntax-table))
(add-hook 'haml-mode-hook 'my-haml-setup)

(setq auto-mode-alist (cons '("\\.haml$" . haml-mode) auto-mode-alist))
(autoload 'haml-mode "haml-mode" "Haml editing mode." t)

;; rails
(defun my-ruby-setup ()
  (setq indent-tabs-mode nil)
  (define-key ruby-mode-map "\C-m" 'newline-and-indent))

(add-hook 'ruby-mode-hook 'my-ruby-setup)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("/Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("/Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; javascript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(require 'coffee-mode)

(require 'web-mode)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(set-face-attribute 'web-mode-html-tag-face nil :foreground "black")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "black")
(set-face-attribute 'web-mode-html-attr-value-face nil :foreground "tomato")
(set-face-attribute 'web-mode-html-attr-value-face nil :background "white")
(set-face-attribute 'web-mode-symbol-face nil :foreground "teal")

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
  '(lambda ()
  (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; require
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook
          (lambda ()
            (set-fill-column 100)))
