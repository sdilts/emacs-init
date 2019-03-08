;;Stuart Dilts 2014

(setf ring-bell-function (lambda ()))

;; ;;macros:
(defmacro with-system (type &rest body)
  "Evaluate body if `system-type' equals type."
  (declare (indent defun))
  `(when (eq system-type ',type)
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
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(if (< emacs-major-version 27)
    (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  ;(setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t))

(use-package company
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  :ensure t)

(use-package org
  :ensure org-plus-contrib
  :defer t
  :commands org-mode
  :config
  (setf org-list-allow-alphabetical t)
  ;; (add-to-list 'org-babel-load-languages '(R . t))
  (org-babel-do-load-languages 'org-babel-load-languages '((R . t)))
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

(add-hook 'prog-mode-hook '(lambda ()
			     (semantic-mode)
			     (company-mode)
			     (auto-fill-mode -1)))

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
				  global-semanticdb-minor-mode
				  global-semantic-idle-summary-mode
				  global-semantic-mru-bookmark-mode))

(add-hook 'shell-mode-hook '(lambda ()
			      (compilation-shell-minor-mode)
			      (company-mode)
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

(eval-after-load 'html-mode
  (use-package multi-web-mode
    :config
    (setq mweb-default-major-mode 'html-mode)
    (setq mweb-tags
	  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
	    (js-mode  "<script[^>]*>" "</script>")
	    (css-mode "<style[^>]*>" "</style>")))
    (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
    (multi-web-global-mode 1)))

(eval-after-load 'lisp-mode
  (use-package slime
    :ensure t
    :defer t
    :config
    (slime-setup '(slime-repl))
    (setq slime-lisp-implementations
	  '((sbcl ("sbcl"))
            (clisp ("clisp"))))))

;; (add-hook 'lisp-mode-hook '(lambda ()
;;                                   (unless (get-process "SLIME Lisp")
;;                                      (let ((oldbuff (current-buffer)))
;;                                        (slime)
;;                                        (switch-buffer oldbuff)))))

;;;*********************************************************************
;: "Dynamically" loaded paackages:


(add-hook 'comint-mode '(lambda ()
			  (company-mode)
			  (auto-fill-mode -1)))


(add-hook 'java-mode-hook '(lambda ()
			     (make-local-variable 'company-backends)
			     (push '(company-semantic company-keywords)
			     	   company-backends)))

(add-hook 'c-mode-hook 'my-config-load-c-completion)
(add-hook 'c++-mode-hook 'my-config-load-c-completion)

;;;###autoload
(defun my-config-load-c-completion ()
  (use-package irony
    :ensure t
    :config
    (unless (irony--find-server-executable)
      (call-interactively #'irony-install-server)))
  (use-package company-irony
    :ensure t)
  (use-package company-irony-c-headers
    :ensure t)
  ;; remove the current function:
  (remove-hook 'c++-mode-hook 'my-config-load-c-completion)
  (remove-hook 'c-mode-hook 'my-config-load-c-completion)
  (fmakunbound 'my-config-load-c-completion)
  ;; setup  c modes:
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-to-list 'company-backends '(company-irony-c-headers company-irony))
  (irony-mode))

(add-hook 'latex-mode-hook '(lambda ()
			      (semantic-mode)
			      (company-mode)))

(eval-after-load 'haskell-mode
  (progn
    (use-package company-cabal
      :ensure t
      :defer t)
    (use-package haskell-mode
      :ensure t
      :defer t)))
(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))



;; (let ((maxima-location "/usr/local/share/maxima/5.40.0/emacs/"))
;;   (when (file-directory-p maxima-location)
;;     (add-to-list 'load-path maxima-location)
;;     (autoload 'maxima-mode "maxima" "Maxima mode" t)
;;     (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
;;     (autoload 'maxima "maxima" "Maxima interaction" t)
;;     (autoload 'imath-mode "imath" "Imath mode for math formula input" t)
;;     (setq imaxima-use-maxima-mode-flag t)
;;     (add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))))


(desktop-save-mode 1)
(setq desktop-restore-eager 3)
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
