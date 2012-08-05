;; Batch test web package
(require 'cl)
(setq base-dir (file-name-directory
                (or (buffer-file-name)
                    load-file-name
                    default-directory)))
(setq package-user-dir (concat base-dir ".elpa"))
(setq package-archives
      `(("local" . ,(concat base-dir "packages"))))
(package-initialize)
(package-refresh-contents)
(destructuring-bind
      (dashes tar-file-name &optional requirements) command-line-args-left
  (let ((tar-package
         (concat
          (file-name-directory
           (or (buffer-file-name)
               load-file-name))
          tar-file-name)))
    (message "the package is: %s" tar-package)
    (package-install-file tar-package)))

;; End
