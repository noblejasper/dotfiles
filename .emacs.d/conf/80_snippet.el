(require 'smartchr)
(global-set-key (kbd "=") (smartchr '("=" " = " " == ")))
(global-set-key (kbd "'") (smartchr '("'`!!''" "'''`!!''''" "'")))
(global-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
(global-set-key (kbd "(") (smartchr '("(`!!')" "(")))
(global-set-key (kbd "{") (smartchr '("{`!!'}" "{{ `!!' }}" "{% `!!' %}" "{# `!!' #}" "{")))
(global-set-key (kbd "[") (smartchr '("[`!!']" "[")))
(global-set-key (kbd "C") (smartchr '("C" "class `!!'():")))
(global-set-key (kbd "D") (smartchr '("D" "def `!!'(self):")))
(global-set-key (kbd "A") (smartchr '("A" "<a href=\"`!!'\"></a>")))
(global-set-key (kbd "H") (smartchr '("H" "<h1>`!!'</h1>" "<h2>`!!'</h2>" "<h3>`!!'</h3>" "<h4>`!!'</h4>" "<h5>`!!'</h5>")))
(global-set-key (kbd "<") (smartchr '("<`!!'>" "<" "&lt;")))
(global-set-key (kbd ">") (smartchr '(">" "&gt;")))
(global-set-key (kbd "&") (smartchr '("&" "&amp;")))

;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
(require 'snippet)
;; snippet.el で、addrev に定型文を追加する
(setq-default abbrev-mode t)
(add-hook 'html-mode-hook
          '(lambda ()
             (setq-default abbrev-mode t) ;; abbrev-mode をon
             (snippet-with-abbrev-table 'html-mode-abbrev-table
                                        ("ahref" . "<a href=\"$${url}\">$${title}</a>")
                                        ("linkbase" . "<a href=\"{{ link(\"$${path}\") }}\">$${title}</a>")
                                        ("if" . "{% if $${True} %}\n{% endif %}")
                                        ("for" . "{% for $${scalar} in $${list} %}\n{% endfor %}")
                                        ("include" . "{% include \"$${include_filaname}\" %}")
                                        ("space" . "{{ $${1}|space|safe }}")
                                        )))

(add-hook 'python-mode-hook
          '(lambda ()
             (setq-default abbrev-mode t) ;; abbrev-mode をon
             (snippet-with-abbrev-table 'python-mode-abbrev-table
                                        ("for" . "for $${element} in $${sequence}:")
                                        ("if" . "if $${True}:")
                                        ("def" . "def $${name}($${args}):\n    ")
                                        ("wh" . "while $${True}:")
                                        ("pdb" . "import pdb; pdb.set_trace()")
                                        )))
