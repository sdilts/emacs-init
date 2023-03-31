(use-package tide-mode
  :ensure t)

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


;; ng2-mode provides a mode for ts files, but it sucks, so remove it from
;; (commented out because I'm not using it right now:
;; (cl-remove-if (lambda (item)
;; 		(cl-find (car item) '("\\.module.ts\\'"
;; 				      "\\.guard.ts\\'"
;; 				      "\\.directive.ts\\'"
;; 				      "\\.pipe.ts\\'"
;; 				      "\\.service.ts\\'"
;; 				      "\\.component.ts\\'"
;; 				      "\\.component.html\\'")
;; 			 :test #'string-equal))
;; 	      auto-mode-alist)

;; (use-package web-mode
;;   :defer t
;;   :ensure t
;;   :config
;;   (setq web-mode-engines-alist
;; 	'(("freemarker" . "\\.ftlh"))))

(define-derived-mode ng-template-mode mhtml-mode "Angular Template"
  "Major mode for Angular template files")

(add-hook 'ng-template-mode-hook #'eglot-ensure)

(add-to-list 'auto-mode-alist '("\\.component.html\\'" . ng-template-mode))

(require 'eglot)
(add-to-list `eglot-server-programs
	     `(ng-template-mode "node"
				"C:/Users/cnc030/node_modules/@angular/language-server/index.js"
				"--ngProbeLocations"
				"C:/Users/cnc030/node_modules/"
				"--tsProbeLocations"
				"C:/Users/cnc030/node_modules/"
				"--stdio"))
