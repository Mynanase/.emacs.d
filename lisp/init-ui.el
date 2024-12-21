;; 使用 doom-themes 包
(use-package doom-themes
  :ensure t
  :config
  ;; 全局主题设置
  (setq doom-themes-enable-bold t    ; 如果为 nil，将禁用粗体
        doom-themes-enable-italic t)  ; 如果为 nil，将禁用斜体
  
  (load-theme 'doom-city-lights t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (doom-themes-neotree-config))

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t
        sml/theme 'respectful)
  (sml/setup))

;; 中英文字体, 字符串
(set-face-attribute 'default nil
                    :font (font-spec :family "Sarasa Mono SC Light"
                                    :size 22
                                    :weight 'normal))

;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;; 		    charset
;; 		    (font-spec :family "QiushuiShotai"
;; 			       :size 24)))

(set-fontset-font t 'symbol "Segoe UI Emoji")

;; 设置中文字体
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset
;;                     (font-spec :family "Microsoft YaHei"
;;                               :size 20)))

;; 调整行高
(setq-default line-spacing 0.2)

;; 启动相对行号
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(provide 'init-ui)

