(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "M-n") #'flymake-goto-next-error)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)

(provide 'init-kbd)
