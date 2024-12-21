;; Latex
(setq org-preview-latex-image-directory "~/.latex-figs/") ;; 设置缓存目录

(use-package cdlatex
  :ensure t
  :hook (org-mode . turn-on-org-cdlatex))

(setq-default org-latex-packages-alist '(("" "physics" t)
                                        ("" "amsmath" t)
                                        ("margin=1in" "geometry" t)))

;; 当光标
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode)
  :config
  )
;; Latex 相关
(use-package org
  :config
    (setq org-directory (file-truename "~/Org/"))
    (setq org-support-shift-select t)  ;; 启用 shift-select
    (setq org-hide-emphasis-markers t)
    (setq org-hide-leading-stars t)
    (setq org-startup-with-latex-preview t)
    (setq org-highlight-latex-and-related '(native latex entities)) ;; LaTeX 高亮设置
    (setq org-pretty-entities t) ;; LaTeX 代码的 prettify
    (setq org-pretty-entities-include-sub-superscripts nil) ;; 不隐藏 LaTeX 的上下标更容易编辑
    (setq my/latex-preview-scale 1.2)
     (setq org-format-latex-options
  	`(:foreground default :background default :scale ,my/latex-preview-scale :html-foreground "Black" :html-background "Transparent" :html-scale ,my/latex-preview-scale :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))) ;; 增大公式预览的图片大小
     )

(add-hook 'org-mode-hook (lambda ()
			  (org-indent-mode)
                          (visual-line-mode)
			  (org-cdlatex-mode)
                          (setq line-spacing 0.5)
			  ))

;; org-cdlatex-mode 中使用 cdlatex 的自动匹配括号, 并把 $...$ 换成 \( ... \)
(defun my/insert-inline-OCDL ()
  (interactive)
  (insert "\\(")
  (save-excursion (insert "\\)" )))
(defun my/insert-dollar-OCDL ()
  (interactive)
  (insert "$")
  (save-excursion (insert "$" )))
(defun my/insert-bra-OCDL ()
  (interactive)
  (insert "(")
  (save-excursion (insert ")" )))
(defun my/insert-sq-bra-OCDL ()
  (interactive)
  (insert "[")
  (save-excursion (insert "]" )))
(defun my/insert-curly-bra-OCDL ()
  (interactive)
  (insert "{")
  (save-excursion (insert "}" )))

(define-key org-cdlatex-mode-map (kbd "$") 'my/insert-inline-OCDL)
(define-key org-cdlatex-mode-map (kbd "(") 'my/insert-bra-OCDL)
(define-key org-cdlatex-mode-map (kbd "[") 'my/insert-sq-bra-OCDL)
(define-key org-cdlatex-mode-map (kbd "{") 'my/insert-curly-bra-OCDL)

;; org-appear
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t))

;; src prettify
(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
                                       ("#+END_SRC" . "†")
                                       ("#+begin_src" . "†")
                                       ("#+end_src" . "†")
                                       (">=" . "≥")
                                       ("=>" . "⇨")))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

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
  (cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))
  (org-roam-db-autosync-mode))

(setq org-roam-capture-templates
      `(("m" "main" plain "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+DATE: <%<%Y-%m-%d %H:%M>>\n#+filetags: :daily:\n#+title: ${title}")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new (file+head "reference/${title}.org"
                            "#+DATE: <%<%Y-%m-%d %H:%M>>\n#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new (file+head "articles/${title}.org"
                            "#+DATE: <%<%Y-%m-%d %H:%M>>\n#+filetags: :article:\n#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("d" "daily" plain "%?"
         :if-new (file+head "daily/%<%Y-%m-%d>.org"
                            "#+DATE: <%<%Y-%m-%d %H:%M>>\n#+filetags: :daily:\n#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)))

(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

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

