(setq load-path (cons "~/.emacs.d/elisp/" load-path)
      load-path (cons "~/.emacs.d/elisp/apel/" load-path)
      load-path (cons "~/.emacs.d/elisp/emu/" load-path)
      load-path (cons "~/.emacs.d/elisp/migemo/" load-path)
      load-path (cons "~/.emacs.d/elisp/ac-dict/" load-path)
      load-path (cons "~/.emacs.d/elisp/magit/share/emacs/site-lisp/" load-path)
      load-path (cons "/usr/share/emacs/site-lisp/howm/" load-path)
      )

(require 'init-loader)
;; loads hoge.elc (if exists) or hoge.el
(eval-when-compile (require 'cl))
(flet ((init-loader--re-load-files (re dir &optional sort)
  (loop for el in (directory-files dir t)
        when (and (string-match re (file-name-nondirectory el))
                  (or (string-match "elc$" el)
                      (and (string-match "el$" el)
                           (not (locate-library (concat el "c"))))))
        collect (file-name-nondirectory el) into ret
        finally return (if sort (sort ret 'string<) ret))))
  (init-loader-load "~/.emacs.d/conf"))