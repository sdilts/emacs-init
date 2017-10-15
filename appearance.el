;; Stuart Dilts 2017


(when (not window-system)
  (menu-bar-mode -1))
;;  (disable-theme 'deeper-blue)
;;  (load-theme 'manoj-dark))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-compile)
  (spaceline-emacs-theme)
  (spaceline-toggle-version-control-on)
  (setf powerline-default-separator 'arrow))

(use-package material-theme
  :config
  (load-theme 'material t))

(use-package fancy-battery
  :ensure t
  :config
  (fancy-battery-mode))

;; ;;disables that pesky tool bar.
(tool-bar-mode -1)
