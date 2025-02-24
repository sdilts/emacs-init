(with-system windows-nt
  (let ((python-path (concat (getenv "HOMEPATH") "\\AppData\\Local\\Microsoft\\WindowsApps\\python3")))
    (setq python-shell-interpreter python-path)))
(without-system windows-nt
  (setq python-shell-interpreter "/usr/bin/python3"))
