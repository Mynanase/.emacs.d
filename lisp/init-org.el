(use-package org
  :ensure nil
  :load-path "~/.emacs.d/dev/org-mode/lisp"
  :config
  (setq
   org-directory (file-truename "~/Org/")
   org-hide-emphasis-markers t
   org-support-shift-select t
   org-ellipsis "..")
  )

;; (custom-set-faces
;;  '(org-level-1 ((t (:family "MiSans" :weight bold))))
;;  '(org-level-2 ((t (:family "MiSans" :weight bold))))
;;  '(org-bold ((t (:family "MiSans" :weight bold))))
;;  '(bold ((t (:family "MiSans" :weight bold)))))

(add-hook 'org-mode-hook (lambda ()
			   ;; (visual-line-mode)
			   (org-cdlatex-mode)
			   (setq line-spacing 0.2)
			   ))

(add-hook 'org-mode-hook #'org-latex-preview-auto-mode)

;;; org-latex-preview
;; init
(setq org-startup-with-inline-images t
      org-startup-with-latex-preview t)

(use-package tex :ensure auctex)

;; cdlatex
(use-package cdlatex
  :ensure t
  :hook (org-mode . turn-on-org-cdlatex))

;; latex image path
(setq
 ;; org latex preview
 org-latex-preview-cache 'temp
 org-latex-preview-live '(inline block edit-special)
 org-latex-preview-live-display-type 'buffer
 )

;; (setq org-latex-preview-numbered t)
					; (plist-put org-latex-preview-options :zoom 1.25)
(let ((pos (assoc 'dvisvgm org-latex-preview-process-alist)))
  (plist-put (cdr pos) :image-converter '("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview -o %B-%%9p.svg %f")))
;; org latex package
(setq-default org-latex-packages-alist '(("" "physics" t)
                                         ("" "amsmath" t)
                                         ("margin=1in" "geometry" t)))


(setq org-highlight-latex-and-related '(native)) ; Highlight inline LaTeX code
(setq org-use-sub-superscripts '{})

;; org-cdlatex-mode 中使用 cdlatex 的自动匹配括号, 并把 $...$ 换成 \( ... \)
(defun my/insert-inline-OCDL ()
  (interactive)
  (insert "\\(")
  (save-excursion (insert "\\)" )))
(define-key org-cdlatex-mode-map (kbd "$") 'my/insert-inline-OCDL)

;; org-appear
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t))

;; org-modern setting
(use-package org-modern			;
  :ensure t
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(setq
 ;; Styling
 org-modern-star 'fold
 ; org-modern-hide-stars 'leading
 org-modern-fold-stars '(("" . "")
			 ("" . "")
			 ("" . "")
			 ("" . "")
			 ("" . ""))

 org-modern-list '((43 . "󰫢")
 		   (45 . ""))

 org-modern-block-name '(("export" "" "")
  			("example" "󰌵" "󰌵")
  			("verse" "" "")
  			("comment" "" "")
  			("src" "" "")
  			("quote" "" "")
  			(t . t))
 
 org-modern-todo t
 org-modern-keyword nil
 org-modern-checkbox nil

 ;; Editor settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 )

;; src prettify
;; (setq prettify-symbols-alist
;;       '(("lambda"  . ?λ)
;;         (":PROPERTIES:" . ?)
;;         (":ID:" . ?)
;;         (":END:" . ?)
;;         ("#+TITLE:" . ?)
;;         ("#+AUTHOR" . ?)
;;         ("#+BEGIN_QUOTE" . ?)
;;         ("#+END_QUOTE" . ?)
;;         ("#+RESULTS:" . ?)
;;         ("[ ]" . )
;;         ("[-]" . )
;;         ("[X]" . )
;;         ("[#A]" . 󰯬)
;;         ("[#B]" . 󰯯)
;;         ("[#C]" . 󰯲)))

;; (add-hook 'org-mode-hook 'prettify-symbols-mode)


;; src prettify
;; (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
;;                                        ("#+END_SRC" . "†")
;;                                        ("#+begin_src" . "†")
;;                                        ("#+end_src" . "†")
;;                                        (">=" . "≥")
;;                                        ("=>" . "⇨")))
;; (setq prettify-symbols-unprettify-at-point 'right-edge)
;; (add-hook 'org-mode-hook 'prettify-symbols-mode)

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
                            "#+DATE: <%<%Y-%m-%d %H:%M>>\n#+title: ${title}")
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
      (concat "${type:15} ${title:*} " (propertize "${tags:20}" 'face 'org-tag)))

;; org-roam-ui
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  :config
  (org-roam-ui-mode))

(provide 'init-org)
