(require 'package)
(setq package-enable-at-startup nil)

(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(require 'use-package)


(defun online? ()
  "Detect if it's online or not,
  ;TODO:find a better way to filter out interfaces"
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (or (equal "lo" (car iface)) (equal "docker0" (car iface)))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

(when (online?)
    (package-refresh-contents))
