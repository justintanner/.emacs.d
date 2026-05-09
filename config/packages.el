(require 'package)
(setq package-install-upgrade-built-in t)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Packages
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lua-indent-level 2)
 '(package-selected-packages
   (quote
    (add-node-modules-path ahk-mode catppuccin-theme coffee-mode compat ellama exec-path-from-shell go-mode gptel inf-ruby js2-mode json-mode llm lua-mode markdown-mode prettier-js robe rspec-mode rubocop sass-mode seq typescript-mode web-mode yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; LLM
(global-set-key (kbd "C-c g") 'gptel)
(global-set-key (kbd "C-c C-g") 'gptel-send)
(with-eval-after-load 'gptel
  (setq gptel-default-mode 'markdown-mode))

;; Ruby
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-ts-mode-hook 'inf-ruby-minor-mode)
(add-hook 'ruby-ts-mode-hook 'robe-mode)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; JavaScript / TypeScript
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(defun jwt-enable-node-tools ()
  (when (fboundp 'add-node-modules-path)
    (add-node-modules-path))
  (when (and (fboundp 'prettier-js-mode)
             (executable-find "prettier"))
    (prettier-js-mode 1)))

(dolist (hook '(js2-mode-hook typescript-mode-hook web-mode-hook json-mode-hook))
  (add-hook hook 'jwt-enable-node-tools))

;; Allows emacs terminal to have the right path
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Make sure I'm not using zsh
(setq explicit-shell-file-name "/bin/bash")
(setq-default shell-file-name "/bin/bash")
