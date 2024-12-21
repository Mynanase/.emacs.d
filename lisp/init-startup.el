;; 中文乱码
(prefer-coding-system 'utf-8)
(unless *is-windows*
  (set-selection-coding-system 'utf-8))
;; GC
(setq gc-cons-threshold most-positive-fixnum)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; 括号自动补全
(electric-pair-mode t)
;; shut off 
(setq inhibit-startup-screen t)

(provide 'init-startup)
