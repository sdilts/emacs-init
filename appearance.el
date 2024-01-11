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
      ;; :ensure t
      :config
      (require 'spaceline-config)
      (spaceline-compile)
      (spaceline-emacs-theme)
      (spaceline-toggle-version-control-on)
      (setf powerline-default-separator 'arrow))
    ;; (use-package material-theme
    ;;   :ensure t
    ;;   :config
    ;;   (load-theme 'material t))
    (load-theme 'misterioso t)
    (modify-all-frames-parameters (list (cons 'cursor-color "#ff6347")))
    (setq w32-use-visible-system-caret nil)

    (without-system windows-nt
      (use-package fancy-battery
	:ensure t
	:config
	(fancy-battery-mode)))))
