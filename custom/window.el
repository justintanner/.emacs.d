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

(defun arrange-frame (w h x y)
  "Set the width, height, and x/y position of the current frame"
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (set-frame-position frame x y)
    (set-frame-size frame w h)))

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
    (cond
      ((equal (x-display-pixel-width) 1920) (arrange-frame 139 68 920 22))
      ((equal (x-display-pixel-width) 1440) (arrange-frame 150 60 365 22))
      ((equal (x-display-pixel-width) 1366) (arrange-frame 150 47 290 22))
      (t (arrange-frame 100 50 10 22)))))

(set-frame-size-according-to-resolution)
