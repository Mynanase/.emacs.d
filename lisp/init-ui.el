;; 使用 doom-themes 包
(use-package doom-themes
  :ensure t
  :config
  ;; 全局主题设置
  (setq doom-themes-enable-bold t    ; 如果为 nil，将禁用粗体
        doom-themes-enable-italic t)  ; 如果为 nil，将禁用斜体

  (load-theme 'doom-city-lights t)
  (doom-themes-org-config))

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t
        sml/theme 'respectful)
  (sml/setup))

;; 中英文字体, 字符串 ff  ff i dsd f 0d d sdsfassd
(set-face-attribute 'default nil
                    :font (font-spec :family "Maple Mono Normal NF CN")
		    :height 120)

;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;; 		    charset
;; 		    (font-spec :family "华文楷体" :height 140)))

;; (set-fontset-font t 'symbol "Segoe UI Emoji")

;; 设置中文字体
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset
;;                     (font-spec :family "Microsoft YaHei"
;;                               :size 20)))

;; 调整行高
(setq-default line-spacing 0.2)

;; 启动相对行号
(add-hook 'prog-mode-hook 'column-number-mode) ;在ModeLine显示列号
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;显示行号
(add-hook 'prog-mode-hook 'electric-pair-mode) ;括号的配对
(add-hook 'prog-mode-hook 'flymake-mode) ;错误的提示
(add-hook 'prog-mode-hook 'hs-minor-mode) ;代码的折叠
;; (add-hook 'prog-mode-hook 'prettify-symbols-mode) ;会将lambda等符号美化为λ
(setq display-line-numbers-type 'relative)

(provide 'init-ui)
