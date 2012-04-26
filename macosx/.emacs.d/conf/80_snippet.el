;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
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
(global-set-key (kbd "B") (smartchr '("B" "<br />" "{% if not g.is_sp %}<br />{% endif %}")))
(global-set-key (kbd "<") (smartchr '("<`!!'>" "<" "&lt;")))
(global-set-key (kbd ">") (smartchr '(">" "&gt;")))
(global-set-key (kbd "&") (smartchr '("&" "&amp;")))

(require 'snippet)
;; snippet.el で、addrev に定型文を追加する
(setq-default abbrev-mode t)
(add-hook 'html-mode-hook
          '(lambda ()
             (setq-default abbrev-mode t) ;; abbrev-mode をon
             (snippet-with-abbrev-table 'html-mode-abbrev-table
                                        ("ahref" . "<a href=\"$${url}\">$${title}</a>")
                                        ("linkbase" . "<a href=\"{{ link(\"$${path}\") }}\">$${title}</a>")
                                        ("img" . "<img src=\"{{ g.image_base }}$${path}\" width=\"{{ fit_size($${width}) }}\" height=\"{{ fit_size($${height}) }}\" alt=\"$${alt}\" />")
                                        ("if" . "{% if $${True} %}\n{% endif %}")
                                        ("for" . "{% for $${scalar} in $${list} %}\n{% endfor %}")
                                        ("include" . "{% include \"$${include_filaname}\" %}")
                                        ("emoji" . "{{ color(e(\"$${emoji_id}\"),\"$${color}\") }}")
                                        ("space" . "{{ $${1}|space|safe }}")
                                        ("blink" . "{% call blink() %}$${blink}{% endcall %}")
                                        ("hblink" . "<span style=\"text-decoration:blink\"><blink>$${body}</blink></span>")
                                        ("call" . "{% call $${func_name}() %}$${body}{% endcall %}")
                                        ("td" . "{% call td() %}$${body}{% endcall %}")
                                        ("divb" . "<div style=\"background-color:$${bg_color};color:$${color};\">\n</div>")
                                        ("divbc" . "<div style=\"background-color:$${bg_color};color:$${color};text-align:center\">\n</div>")
                                        ("divc" . "<div style=\"text-align:center\">\n</div>")
                                        ("spanc" . "<span style=\"color:$${color}\">$${body}</span>")
                                        )))
(add-hook 'html-helper-mode-hook
          '(lambda ()
             (setq-default abbrev-mode t) ;; abbrev-mode をon
             (snippet-with-abbrev-table 'html-helper-mode-abbrev-table
                                        ("ahref" . "<a href=\"$${url}\">$${title}</a>")
                                        ("linkbase" . "<a href=\"{{ link(\"$${path}\") }}\">$${title}</a>")
                                        ("img" . "<img src=\"{{ g.image_base }}$${path}\" width=\"{{ fit_size($${width}) }}\" height=\"{{ fit_size($${height}) }}\" alt=\"$${alt}\" />")
                                        ("if" . "{% if $${True} %}\n{% endif %}")
                                        ("for" . "{% for $${scalar} in $${list} %}\n{% endfor %}")
                                        ("include" . "{% include \"$${include_filaname}\" %}")
                                        ("emoji" . "{{ color(e(\"$${emoji_id}\"),\"$${color}\") }}")
                                        ("space" . "{{ $${1}|space|safe }}")
                                        ("blink" . "{% call blink() %}$${blink}{% endcall %}")
                                        ("hblink" . "<span style=\"text-decoration:blink\"><blink>$${body}</blink></span>")
                                        ("call" . "{% call $${func_name}() %}$${body}{% endcall %}")
                                        ("td" . "{% call td() %}$${body}{% endcall %}")
                                        ("divb" . "<div style=\"background-color:$${bg_color};color:$${color};\">\n</div>")
                                        ("divbc" . "<div style=\"background-color:$${bg_color};color:$${color};text-align:center\">\n</div>")
                                        ("divc" . "<div style=\"text-align:center\">\n</div>")
                                        ("spanc" . "<span style=\"color:$${color}\">$${body}</span>")
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
                                        ("hblink" . "<span style=\"text-decoration:blink\"><blink>$${body}</blink></span>")
                                        ("spanc" . "<span style=\"color:$${color}\">$${body}</span>")
                                        )))
