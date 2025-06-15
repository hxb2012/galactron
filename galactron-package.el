;;; galactron-package.el --- 修改Package Archive设置  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-package t "启用修改Package Archive设置" :type 'boolean :tag "Package")

(defgroup galactron-package nil
  "修改Package Archive设置"
  :prefix "galactron-package-"
  :tag "Package"
  :group 'galactron)
;; [[file:README.org::*\[\[help:galactron-package-enabled-melpa\]\[MELPA\]\]][[[help:galactron-package-enabled-melpa][MELPA]]:1]]
(defcustom galactron-package-enabled-melpa t
  "MELPA

- MELPA官方 Milkypostman's Emacs Lisp Package Archive
  https://melpa.org

- TUNA镜像 清华大学开源软件镜像站
  https://mirrors.tuna.tsinghua.edu.cn/help/elpa/

- SJTUG镜像 上海交通大学SJTUG软件源镜像服务
  https://mirrors.sjtug.sjtu.edu.cn/docs/emacs-elpa
"
  :type
  '(choice
    (const :tag "MELPA官方" t)
    (const :tag "TUNA镜像" tuna)
    (const :tag "SJTUG镜像" sjtug)
    (const :tag "禁用" nil))
  :tag "MELPA")

(when galactron-package-enabled-melpa
  (galactron-default-assoc
   package-archives
   "melpa"
   (pcase galactron-package-enabled-melpa
     ('tuna "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
     ('sjtug "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")
     (_ "https://melpa.org/packages/"))))
;; [[help:galactron-package-enabled-melpa][MELPA]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-package-enabled-hxb\]\[我的美尔巴\]\]][[[help:galactron-package-enabled-hxb][我的美尔巴]]:1]]
(defcustom galactron-package-enabled-hxb t
  "我的美尔巴 https://github.com/hxb2012/melpa/"
  :type 'boolean
  :tag "我的美尔巴")

(when galactron-package-enabled-hxb
  (galactron-default-assoc package-archives
                        "hxb" "https://hxb2012.github.io/melpa/")
  (galactron-default-assoc package-archive-priorities "hxb" 1))
;; [[help:galactron-package-enabled-hxb][我的美尔巴]]:1 ends here
(provide 'galactron-package)
;;; galactron-package.el ends here
