;;; gas-city-theme.el --- Warm-dark Gas City palette  -*- lexical-binding: t -*-

;; Mirrors the Gas City palette in ~/dotfiles/mac/.alacritty.toml.
;; Walnut background, champagne foreground, copper / honey / sage / teal
;; accents.

;;; Code:

(deftheme gas-city
  "Warm-dark Gas City palette: walnut, copper, honey, sage, teal.")

(let ((bg              "#1e150e")
      (bg-alt          "#2a1f15")
      (bg-mode         "#3a2a1c")
      (bg-bright-black "#50382a")
      (bg-region       "#4a3520")
      (fg              "#e8d4a8")
      (fg-dim          "#a89074")
      (fg-bright       "#fce8c8")
      (red             "#cc6e3a")
      (red-bright      "#e08652")
      (green           "#8a9a5b")
      (green-bright    "#a8b878")
      (yellow          "#e0a060")
      (yellow-bright   "#f0b870")
      (blue            "#5b85a8")
      (blue-bright     "#7ba5c8")
      (magenta         "#c4806c")
      (magenta-bright  "#d8a090")
      (cyan            "#7fa9a9")
      (cyan-bright     "#9fc4c4"))

  (custom-theme-set-faces
   'gas-city

   ;; Basics. In TTY mode the hex bg quantizes to 256-color and lands on a
   ;; muddy red, so let `default' inherit from the terminal — Alacritty
   ;; already paints #1e150e via colors.primary.background. In GUI Emacs we
   ;; set the hex directly.
   `(default ((((type tty))     (:background unspecified :foreground unspecified))
              (((type graphic)) (:background ,bg :foreground ,fg))))
   `(cursor           ((t (:background ,yellow))))
   `(region           ((t (:background ,bg-region))))
   `(highlight        ((t (:background ,bg-alt))))
   `(hl-line          ((t (:background ,bg-alt))))
   `(fringe ((((type tty))     (:background unspecified :foreground unspecified))
             (((type graphic)) (:background ,bg :foreground ,fg-dim))))
   `(vertical-border  ((t (:foreground ,bg-mode))))

   ;; Mode line
   `(mode-line              ((t (:background ,bg-mode :foreground ,fg :box nil))))
   `(mode-line-inactive     ((t (:background ,bg-alt  :foreground ,fg-dim :box nil))))
   `(mode-line-buffer-id    ((t (:foreground ,yellow :weight bold))))

   ;; Line numbers — let the gutter inherit terminal bg in TTY
   `(line-number
     ((((type tty))     (:background unspecified :foreground ,bg-bright-black))
      (((type graphic)) (:background ,bg :foreground ,bg-bright-black))))
   `(line-number-current-line
     ((((type tty))     (:background unspecified :foreground ,yellow :weight bold))
      (((type graphic)) (:background ,bg :foreground ,yellow :weight bold))))

   ;; Minibuffer / links
   `(minibuffer-prompt ((t (:foreground ,yellow :weight bold))))
   `(link              ((t (:foreground ,cyan :underline t))))
   `(link-visited      ((t (:foreground ,magenta :underline t))))

   ;; Search
   `(isearch        ((t (:background ,yellow :foreground ,bg))))
   `(lazy-highlight ((t (:background ,bg-region :foreground ,yellow-bright))))

   ;; Paren matching
   `(show-paren-match    ((t (:background ,bg-region :foreground ,yellow :weight bold))))
   `(show-paren-mismatch ((t (:background ,red :foreground ,fg-bright :weight bold))))

   ;; Diagnostics
   `(error   ((t (:foreground ,red :weight bold))))
   `(warning ((t (:foreground ,red-bright))))
   `(success ((t (:foreground ,green-bright))))

   ;; Font lock
   `(font-lock-comment-face              ((t (:foreground ,fg-dim :slant italic))))
   `(font-lock-comment-delimiter-face    ((t (:foreground ,fg-dim))))
   `(font-lock-doc-face                  ((t (:foreground ,fg-dim :slant italic))))
   `(font-lock-string-face               ((t (:foreground ,green))))
   `(font-lock-keyword-face              ((t (:foreground ,red))))
   `(font-lock-builtin-face              ((t (:foreground ,blue-bright))))
   `(font-lock-function-name-face        ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face        ((t (:foreground ,cyan))))
   `(font-lock-type-face                 ((t (:foreground ,magenta))))
   `(font-lock-constant-face             ((t (:foreground ,magenta-bright))))
   `(font-lock-preprocessor-face         ((t (:foreground ,blue))))
   `(font-lock-warning-face              ((t (:foreground ,red :weight bold))))
   `(font-lock-negation-char-face        ((t (:foreground ,red))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,red-bright))))))

(provide-theme 'gas-city)

;;; gas-city-theme.el ends here
