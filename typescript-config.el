(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (define-key tide-mode-map (kbd "M-RET") #'tide-fix)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(add-hook 'typescript-mode-hook #'setup-tide-mode)


(use-package web-mode
  :defer t
  :ensure t
  :config
  (setq web-mode-engines-alist
	'(("freemarker" . "\\.ftlh"))))
