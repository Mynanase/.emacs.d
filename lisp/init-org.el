(use-package org
  :config
    (setq org-support-shift-select t)  ;; 启用 shift-select
    (setq org-directory (file-truename "~/Org/"))
    )

(add-hook 'org-mode-hook (lambda ()
			  (org-indent-mode)
                          (visual-line-mode)
                          (setq line-spacing 0.3)))

(setq org-hide-emphasis-markers t
      org-hide-leading-stars t)

;; org-appear
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t))

;; org-superstar config
(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  ;; 优化性能
  (setq org-superstar-prettify-item-bullets t
	org-superstar-leading-bullet ?\s
        org-superstar-special-todo-items t)
  
  ;; 禁用语法检查以提高性能
  (add-hook 'org-mode-hook #'org-superstar-toggle-lightweight-lists)
)

;;; Org-roam
;; Org-roam 配置
(use-package org-roam
  :ensure t
  :after org
  :custom
  (org-roam-directory (concat org-directory "roam/")) ; 设置 org-roam 目录
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%I:%M %p>: %?"
       :if-new (file+head "%<%Y-%m-%d>.org" "#+DATE: <%<%Y-%m-%d %H:%M>>\n#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
    :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(setq org-roam-capture-templates
      `(("m" "main" plain "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+DATE: <%<%Y-%m-%d %H:%M>>\n#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new (file+head "reference/${title}.org"
                            "#+DATE: <%<%Y-%m-%d %H:%M>>\n#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new (file+head "articles/${title}.org"
                            "#+DATE: <%<%Y-%m-%d %H:%M>>\n#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)
        ("d" "daily" plain "%?"
         :if-new (file+head "daily/%<%Y-%m-%d>.org"
                            "#+DATE: <%<%Y-%m-%d %H:%M>>\n#+title: ${title}\n#+filetags: :daily:\n")
         :immediate-finish t
         :unnarrowed t)))

;; org-roam-ui
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  ;; 自定义 org-roam-ui 选项
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  :config
  ;; 启动 org-roam-ui 服务器
  (org-roam-ui-mode))



(provide 'init-org)
