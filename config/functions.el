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

