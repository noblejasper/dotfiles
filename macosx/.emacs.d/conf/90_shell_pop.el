;(require 'shell-pop)
;(shell-pop-set-internal-mode "ansi-term")
;(shell-pop-set-internal-mode-shell "/bin/bash")
;
;(defvar ansi-term-after-hook nil)
;(add-hook 'ansi-term-after-hook
;          (function
;           (lambda ()
;             (define-key term-raw-map "\C-t" 'shell-pop))))
;(defadvice ansi-term (after ansi-term-after-advice (arg))
;  "run hook as after advice"
;  (run-hooks 'ansi-term-after-hook))
;(ad-activate 'ansi-term)
;
;(global-set-key "\C-t" 'shell-pop)
