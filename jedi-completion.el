;; jedi completion is installed locally in ~/Programs/emacs-jedi:
;; needs to be loaded separately. In addition, company-jedi also needs
;; to be installed manually as well.

(setq python-environment--verbose t)
(setq jedi:environment-root "/home/stuart/Programs/emacs-jedi/env")
(add-to-list 'load-path "/home/stuart/Programs/emacs-jedi")

;; stuff for jedi-mode completion:
;; Jedi
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:complete-on-dot t)

;; if installing manually through my fork, these packages are required:
(use-package python-environment
  :ensure t)

(use-package auto-complete
  :ensure t)

(setq jedi:environment-virtualenv
    	  (append python-environment-virtualenv
    		  '("--python" "python3")))

(add-hook 'python-mode-hook '(lambda () (company-mode -1)))
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'inferior-python-mode '(lambda ()
				   (jedi-mode)
				   (auto-fill-mode -1)))

;; company-jedi: Doesn't seem to work as well. too bad :(
;;(add-to-list 'load-path "/home/stuart/Programs/emacs-company-jedi")

;; (defun my-config-load-python ()
;;   (use-package jedi
;;     :ensure t
;;     :config
;;     (setf jedi:complete-on-dot t)
;;     (setq jedi:environment-virtualenv
;;     	  (append python-environment-virtualenv
;;     		  '("--python" "python3")))
;;     (unless (jedi:-env-server-command)
;;       (jedi:install-server)))
;;   (company-mode -1)
;;   (jedi:setup))
;; 					;always use python3

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
