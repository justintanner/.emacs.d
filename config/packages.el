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


;; Allows emacs terminal to have the right path
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Make sure I'm not using zsh
(setq explicit-shell-file-name "/bin/bash")
(setq-default shell-file-name "/bin/bash")

