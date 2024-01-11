;; (without-system windows-nt
;;   (setq python-shell-interpreter "/usr/bin/python3"))

;;;###autoload
;; (defun init-python-mode ()
;;   (use-package company-anaconda
;;     :commands anaconda-mode
;;     :ensure t
;;     :defer t
;;     :commands python-mode
;;     :init
;;     (add-hook 'python-mode-hook 'anaconda-mode)
;;     (add-to-list 'company-backends 'company-anaconda)
;;     :config
;;     (print "Loaded anaconda mode"))
;;   (anaconda-mode)
;;   (remove-hook 'python-mode-hook 'init-python-mode))

;; (add-hook 'python-mode-hook 'init-python-mode)

;; (use-package lsp-jedi
;;   :ensure t
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)
;;     (add-to-list 'lsp-enabled-clients 'jedi)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       `(python-mode . ("jedi-language-server.exe"))))
