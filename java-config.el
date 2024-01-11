;; (use-package lsp-java
;;   :defer t)

;; (require 'lsp-java)
;; (add-hook 'java-mode-hook #'lsp)

(use-package eglot-java
  :ensure t)

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure nil
  :commands (dired-sidebar-toggle-sidebar))

(add-hook 'java-mode-hook 'eglot-java-mode)
