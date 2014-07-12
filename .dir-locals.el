;; Per-directory local variables for GNU Emacs 23 and later.

((nil             . ((fill-column . 78)
                     (tab-width   .  8)))
 (scheme-mode
  .
  ((indent-tabs-mode . nil)
   (eval . (put 'with-tox 'scheme-indent-function 1))))
 (emacs-lisp-mode . ((indent-tabs-mode . nil)))
 (texinfo-mode    . ((indent-tabs-mode . nil)
                     (fill-column . 72))))
