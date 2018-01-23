(add-hook 'python-mode-hook 'init-python-mode)

;;;###autoload
(defun init-python-mode ()
  (use-package company-anaconda
    :commands anaconda-mode
    :ensure t
    :defer t
    :commands python-mode
    :init
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-to-list 'company-backends 'company-anaconda)
    :config
    (print "Loaded anaconda mode"))
  (anaconda-mode)
  (remove-hook 'python-mode-hook 'init-python-mode))
