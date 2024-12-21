;; 1. 首先确保 package.el 正确初始化
(require 'package)

;; 2. 设置软件源
(setq package-archives
      '(("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("org" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; 3. 关闭签名检验
(setq package-check-signature nil)

;; 4. 初始化包管理
(package-initialize)

;; 5. 确保 package-archive-contents 存在
(unless package-archive-contents
  (package-refresh-contents))

;; 6. 安装 use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; 7. 配置 use-package
(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer nil  ; 改为 nil，不要默认延迟加载
      use-package-enable-imenu-support t
      use-package-expand-minimally t)

(provide 'init-elpa)
