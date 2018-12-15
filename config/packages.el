(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Lua
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lua-indent-level 2)
 '(package-selected-packages
   (quote
    (lua-mode yaml-mode web-mode sass-mode markdown-mode js2-mode go-mode coffee-mode ahk-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq js-indent-level 2)

;; Set yasnippets to global
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs '("~/emacs.d/snippets"))

;; Allows emacs terminal to have the right path
(unless (require 'exec-path-from-shell nil 'noerror)
  (exec-path-from-shell-initialize))
