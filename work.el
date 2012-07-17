;; work stuff
(defvar AMDIR "trunk")

(defun grep-current-word ()
  "grep the current directory for the function name at the point"
  (interactive)
  (grep (concat "grep -n -s -F -i -r \"" (current-word) "(\" * ")))

(defun compile-tags ()
  "compile etags for the current project"
  (interactive)
  (compile "find . -name \"*.[chCH]\" -print | etags -"))

