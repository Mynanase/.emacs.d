;; 性能测试相关
(use-package benchmark-init
  :ensure t
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

;; 重启功能
(use-package restart-emacs
  :ensure t
  :defer t)

;; Vertico - 垂直补全界面
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Orderless - 补全匹配引擎
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia - 为 minibuffer 提供注释
(use-package marginalia
  :ensure t
  :after vertico
  :demand t
  :init
  ;; 使用 with-eval-after-load 确保正确的加载顺序
  (with-eval-after-load 'vertico
    (require 'marginalia nil t)
    (when (fboundp 'marginalia-mode)
      (marginalia-mode 1))))

;; Consult
(use-package consult
  :ensure t
  :after vertico
  :bind
  (("C-x b" . consult-buffer)
   ("C-x C-r" . consult-recent-file)
   ("C-c f" . consult-find)
   ("C-s" . consult-line)
   ("C-c s" . consult-ripgrep)
   ("C-c i" . consult-imenu)
   ("M-g g" . consult-goto-line)
   ("M-y" . consult-yank-pop))
  :config
  (consult-customize
   consult-theme consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any)))

;; Embark
(use-package embark
  :ensure t
  :after consult
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Company - 代码补全
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-show-quick-access t)
  :config
  (setq company-idle-delay 0.1))

;; Flycheck - 语法检查
(use-package flycheck
  :ensure t
  :hook (prog-mode . flymake-mode)
  :bind
  (("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error)))

;; Crux - 文本编辑增强
(use-package crux
  :ensure t
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-c ^" . crux-top-join-line)
   ("C-x ," . crux-find-user-init-file)
   ("C-c k" . crux-smart-kill-line)))

;; Hungry Delete
(use-package hungry-delete
  :ensure t
  :defer t
  :bind
  (("C-c DEL" . hungry-delete-backward)
   ("C-c d" . hungry-delete-forward)))

;; Drag Stuff
(use-package drag-stuff
  :ensure t
  :bind
  (("<M-up>" . drag-stuff-up)
   ("<M-down>" . drag-stuff-down)))

;; Which Key
(use-package which-key
  :ensure t
  :defer nil
  :config
  (which-key-mode))

;; Ace Window
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window))

;; 格式化
(use-package format-all :ensure t :defer t
  ;; 开启保存时自动格式化
  :hook (prog-mode . format-all-mode)
  ;; 绑定一个手动格式化的快捷键
  :bind ("C-c f" . #'format-all-region-or-buffer))

(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :bind ("C-c e f" . eglot-format))

(provide 'init-packages)
