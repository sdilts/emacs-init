;;Stuart Dilts 2014

(setf ring-bell-function (lambda ()))
;; actual pt of the font is value / 10:
(set-face-attribute 'default nil :height 110)

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

;; Set the backup file names to be hidden:
(defun make-backup-file-name-hidden (filename)
  (let* ((full-name (make-backup-file-name--default-function filename))
	 (file-part (file-name-nondirectory full-name))
	 (path-part (file-name-directory full-name)))
    (concat path-part "." file-part)))

(setf make-backup-file-name-function 'make-backup-file-name--default-function)

(setf dired-kill-when-opening-new-dired-buffer t)

;; silence the native comp warnings:
(if (> emacs-major-version 27)
    (setq native-comp-async-report-warnings-errors nil))

;;;******************************************************************
;;;******************************************************************
;;Configure globally needed packages:

;;allows to download packages
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; note: you must manually initialze the packages the first time that
;; emacs is run on a new machine
(if (< emacs-major-version 27)
    (package-initialize)
  (setf package-quickstart nil))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t))

(use-package company
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  (setq company-backends '(company-elisp
			   company-cmake
			   company-capf
			   company-files
			   (company-dabbrev-code company-gtags company-etags company-keywords)
			   company-oddmuse
			   company-dabbrev))
  :ensure t)

(use-package org
;;  :ensure org-plus-contrib
  :defer t
  :commands org-mode
  :config
  (setf org-list-allow-alphabetical t)
  ;; (add-to-list 'org-babel-load-languages '(R . t))
  (org-babel-do-load-languages 'org-babel-load-languages '((R . t) (lisp . t)))
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("dvipsnames" "xcolor"))
  (setf org-list-allow-alphabetical t)
  (setf org-latex-listings-options
	'(("keywordstyle" "\\color{RoyalBlue}")
	  ;; ("basicstyle" "\\scriptsize\\tfamily")
	  ("commentstyle" "\\color{Green}\\ttfamily")
	  ("stringstyle" "\\color{BrickRed}")
	  ("rulecolor" "\\color{black}")
	  ("upquote" "true")
	  ("numbers" "left")
	  ("numberstyle" "\\tiny\\color{gray}")
	  ("stepnumber" "1")
	  ("numbersep" "8pt")
	  ("showstringspaces" "false")
	  ("breaklines" "true")
	  ("frameround" "ftff")
	  ("xleftmargin" "\\parindent")
	  ("frame" "single")
	  ;; ("belowcaptionskip" "5em")
	  ("belowskip" "1em")))
  (setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))


(use-package magit
  :ensure t
  :defer t)

(use-package ivy
  :ensure t)

(use-package swiper
  :ensure t
  :config
  (setf ivy-wrap t)
  :bind (("C-s" . swiper)))

(ido-mode t)

(setq-default auto-fill-function 'do-auto-fill)

(set-fill-column 80)
(add-hook 'before-save-hook 'time-stamp)
(setq comint-prompt-read-only t)
(setq tramp-default-method "ssh")

(add-hook 'prog-mode-hook (lambda ()
			     (company-mode)
			     (auto-fill-mode -1)))

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
				  global-semanticdb-minor-mode
				  global-semantic-idle-summary-mode
				  global-semantic-mru-bookmark-mode))

(add-hook 'shell-mode-hook (lambda ()
			      (compilation-shell-minor-mode)
			      (auto-fill-mode -1)))

(use-package popwin
  :ensure t)
(define-global-minor-mode my-global-popwin-mode popwin-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'slime-repl-mode 'shell-mode)))
      (popwin-mode 1))))
(my-global-popwin-mode 1)

(use-package vimish-fold
  :ensure t
  :bind ("s-c" . vimish-fold)
  :bind ("s-a" . vimish-fold-toggle)
  :config
  (vimish-fold-global-mode t))
;;:bind ("s-d" . vimish-unfold))

(eval-after-load 'lisp-mode
  (use-package slime
    :defer t
    :config
    (slime-setup '(slime-repl))
    (setq slime-lisp-implementations
	  '((sbcl ("sbcl"))
	    (ccl ("ccl"))
	    (ecl ("ecl"))
            (clisp ("clisp"))))))

;;;*********************************************************************
;: "Dynamically" loaded paackages:

(eval-after-load 'cc-mode
  (progn
    (require 'cc-mode)
    (add-to-list 'c-default-style '(c++-mode . "linux"))
    (let ((tab-width-setter (lambda ()
			      (setq-default indent-tabs-mode t)
			      (setq-default tab-width 4) ; Assuming you want your tabs to be four spaces wide
			      (defvaralias 'c-basic-offset 'tab-width))))
      (add-hook 'c-mode-hook tab-width-setter)
      (add-hook 'c++-mode-hook tab-width-setter))))

(add-hook 'comint-mode (lambda ()
			  (company-mode)
			  (auto-fill-mode -1)))

(add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
(defun init-java-mode ()
  (make-local-variable 'project-vc-extra-root-markers)
  (setq project-vc-extra-root-markers (list "build.gradle" "pom.xml")))
;; Use the depth arg to add the hooks such that they are ran
;; before most others, as we set project-vc-extra-root-markers
;; in this, and that must come before we init eglot, which is
;; done elsewhere
(add-hook 'java-mode-hook #'init-java-mode -10)
(add-hook 'java-ts-mode-hook #'init-java-mode -10)

(use-package eglot
  :hook ((c-mode c++-mode rust-mode java-mode) .
	 (lambda () (eglot-ensure))))

(add-hook 'latex-mode-hook (lambda ()
			     (semantic-mode)
			     (company-mode)))

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

(global-set-key (kbd "<ESC> l") 'goto-line)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
