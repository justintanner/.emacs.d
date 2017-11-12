(defvar CONFIGDIR (format "%s/.emacs.d/config" (getenv "HOME")))

(load (format "%s/start" CONFIGDIR))

(require 'server)
(server-start)
