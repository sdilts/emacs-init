(require 'cl)
(require 'cl-lib)

(defgroup easy-load-shell nil
  "Provides convenience function to open a shell in a mostly
convenient place")

(defcustom *easy-shell-type* 'eshell
  "The shell or repl that will load when load-shell is called.
Must correspond with *easy-shell-default-buffer*, or else switching will not work properly"
  :group 'easy-load-shell)

(defcustom *easy-shell-default-buffer* "*eshell*"
  "The name of the buffer that the shell specified by *easy-shell-type* lives in.
Must correspond to *easy-shell-type*, or else switching will not work properly"
  :group 'easy-load-shell)

(defun open-shell (&optional location)
  "Provides a generic function to open the shell"
  (if location
      (let ((default-directory location))
	 (funcall *easy-shell-type* location))
    (funcall *easy-shell-type*)))


(defun window-b-right-corner-p (window)
  "Returns true if the given window is the rightmost window.
Author: ChrisDone"
  (and
   (>= (nth 2 (window-edges window))
       (frame-width (window-frame window)))
   (>= (+ 1 (nth 3 (window-edges window)))
       (frame-height (window-frame window)))))

(defun load-shell (&optional location)
  "Finds a nice place to open the shell specified by
*easy-shell-type*. Usually opens the shell in the bottom right hand
corner of the frame."
  (interactive)
  (if (not (equal (buffer-name (current-buffer)) *easy-shell-default-buffer*))
      (if (cl-member *easy-shell-default-buffer* (window-list)
		     :key #'(lambda (x) (buffer-name (window-buffer x)))
		     :test 'equalp)
	  (switch-to-buffer-other-window *easy-shell-default-buffer*)
	(cond
	 ((= (length (window-list)) 1)
	  (split-window-right)
	  (other-window 1)
	  (open-shell location))
	 ((= (length (window-list)) 2)
	  ;;could also use select-buffer:
	  (switch-to-buffer-other-window
	   (window-buffer (cl-find #'window-b-right-corner-p (window-list))))
	  (split-window)
	  (other-window 1)
	  ;; make the new buffer smaller
	  ;; (let ((delta-size (/ (window-height) -2)))
	  ;;   (if (window-resizable nil delta-size)
	  ;; 	(window-resize nil delta-size)))
	  (open-shell location))
	 (t
	  (select-window
	   (cl-find-if #'window-b-right-corner-p (window-list)))
	  (open-shell location))))))

(provide 'easy-load-shell)
