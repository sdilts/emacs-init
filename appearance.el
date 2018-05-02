;; Stuart Dilts 2017

;;disables that pesky tool bar.
(tool-bar-mode -1)


(if (and (not window-system) (not (daemonp)))
    (progn
      (menu-bar-mode -1)
      (load-theme 'manoj-dark))
  (progn
    (menu-bar-mode -1)
    ;;when in a windowed system, use the fancy themes:
    (use-package spaceline
      :ensure t
      :config
      (require 'spaceline-config)
      (spaceline-compile)
      (spaceline-emacs-theme)
      (spaceline-toggle-version-control-on)
      (setf powerline-default-separator 'arrow))
    (use-package material-theme
      :ensure t
      :config
      (load-theme 'material t))

    (use-package fancy-battery
      :config
      (fancy-battery-mode))))
