(progn
  (require 'package)
  (push '("melpa" . "https://melpa.org/packages/") package-archives)
  (package-initialize)
  (unless (seq-find (lambda (e) (string= "melpa" (package-desc-archive (cadr e)))) package-archive-contents)
    (package-refresh-contents)))
