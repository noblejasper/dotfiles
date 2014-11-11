(require 'zencoding-mode)
(add-hook 'jinja2-mode-hook 'zencoding-mode)
(define-key zencoding-mode-keymap [s-return] 'zencoding-expand-line)
(define-key zencoding-mode-keymap [C-return] 'find-file)
