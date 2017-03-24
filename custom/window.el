;;;
;;; This file is only loaded when window-system is true.
;;;

(require 'font-lock)

(defun window-build-font (family points)
  "Given a font family and a point size, this function builds the magic
font string that emacs uses to represent that font. The mono-font-sizes
list is used to fill in the magic values for the font name."
  (let ((mono-font-sizes '((8 11 82) (9 12 90) (10 13 97)
                           (11 15 112) (12 16 120))))
    (format "-*-%s-normal-r-normal-normal-%s-%s-*-*-c-*-iso8859-15"
            family
            (nth 1 (assoc points mono-font-sizes))
            (nth 2 (assoc points mono-font-sizes)))))

;;; set or append a frame attribute in the default-frame-alist
(defun window-set-frame-default (key value)
  (let ((val (assoc key default-frame-alist)))
    (if (null val)
        (setcdr (last default-frame-alist) (list (cons key value)))
      (rplacd val value)))
  value)

;; setup the window
(defun window-setup ()
  (if is-win32 (set-frame-font my-font))
  (if display-time-format (display-time)))

(when window-system
  (blink-cursor-mode 0)
  (set-scroll-bar-mode nil)
  (setq visible-bell t)
  (tool-bar-mode 0)
  (transient-mark-mode t)
  (mouse-wheel-mode t)

  (if is-win32 (setq my-font (window-build-font "Courier New" 9)))

  (window-set-frame-default 'auto-raise nil)
  (window-set-frame-default 'cursor-type 'box)
  (window-set-frame-default 'scroll-bar-width 12)

  (setq frame-title-format
    (concat "Emacs@"
      (if (string-match "^\\([^.]+\\)\..+" (system-name))
        (match-string 1 (system-name))
          (system-name))
            " - %f")))
