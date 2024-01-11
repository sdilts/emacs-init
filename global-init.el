;;Stuart Dilts 2014

(setf ring-bell-function (lambda ()))

;; ;;macros:
(defmacro with-system (type &rest body)
  "Evaluate body if `system-type' equals type."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(defmacro without-system (type &rest body)
  "Evaluate body if `system-type' equals type."
  (declare (indent defun))
  `(unless (eq system-type ',type)
     ,@body))

;;;******************************************************************
;;;******************************************************************
;;Configure globally needed packages:

;;allows to download packages
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(if (< emacs-major-version 27)
    (package-initialize))

(unless (package-installed-p 'use-package)
  (message "Installing `use-package")
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package auto-compile
  ;; :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t))

(use-package company
  ;; :ensure t
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1))

(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.1)

(use-package magit
  ;; :ensure t
  :defer t)

(use-package ivy
  ;; :ensure t
  )
(use-package swiper
  ;; :ensure t
  :config
  (setf ivy-wrap t)
  :bind (("C-s" . swiper)))

(use-package yaml-mode
  ;; :ensure t
  :defer t)

(ido-mode t)

(setq-default auto-fill-function 'do-auto-fill)

(set-fill-column 80)
(add-hook 'before-save-hook 'time-stamp)
(setq comint-prompt-read-only t)
(setq tramp-default-method "ssh")

(add-hook 'prog-mode-hook (lambda ()
			     (company-mode)
			     (auto-fill-mode -1)))

(add-hook 'shell-mode-hook (lambda ()
			       (compilation-shell-minor-mode)
			       (auto-fill-mode -1)))

(use-package vimish-fold
  ;; :ensure t
  :bind ("s-c" . vimish-fold)
  :bind ("s-a" . vimish-fold-toggle)
  :config
  (vimish-fold-global-mode t))
;;:bind ("s-d" . vimish-unfold))

;;;*********************************************************************
;: "Dynamically" loaded paackages:


(add-hook 'java-mode-hook (lambda ()
			      (make-local-variable 'company-backends)
			      (push '(company-semantic company-keywords)
			     	    company-backends)))

(setq tetris-score-file
      "~/.emacs.d/tetris-scores")
(setq snake-score-file
      "~/.emacs.d/snake-scores")
;;;********************************************************************
;;;********************************************************************
;Setup some useful functions:

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

;; newline-without-break-of-line
(defun newline-without-break-of-line ()
  "1. remove to end of the line.
  2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "M-d") 'delete-backward-char)
(global-set-key (kbd "C-o") 'newline-without-break-of-line)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
