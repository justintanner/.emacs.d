;; Borrowed from https://www.emacswiki.org/emacs/MessagesBuffer
(defun show-message-log () (interactive) (switch-to-buffer "*Messages*"))

(defun indent-or-hippie-expand (arg)
  "Indent or hippie expand"
  (interactive "*P")
  (cond
   ((and transient-mark-mode mark-active)
    (indent-region (region-beginning) (region-end) nil))
   ((and (eq (char-syntax (preceding-char)) ?w)
         (not (= (current-column) 0)))
    (hippie-expand arg))
   (t (indent-for-tab-command))))

(defun empty-frame ()
  "Open a new frame with a buffer named Untitled<N>.

The buffer is not associated with a file."
  (interactive)
  (switch-to-buffer-other-frame (generate-new-buffer "Untitled")))

(defun empty-buffer ()
  "Open a new a buffer named Untitled<N>.

The buffer is not associated with a file."
  (interactive)
  (switch-to-buffer (generate-new-buffer "Untitled")))
