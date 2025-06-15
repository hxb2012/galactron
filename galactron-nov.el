;;; galactron-nov.el --- 修改Nov设置  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-nov t "启用修改Nov设置" :type 'boolean :tag "Nov")

(defgroup galactron-nov nil
  "修改Nov设置"
  :prefix "galactron-nov-"
  :tag "Nov"
  :group 'galactron)
;; [[file:README.org::*\[\[help:galactron-nov-enabled-auto-mode\]\[关联后缀\]\]][[[help:galactron-nov-enabled-auto-mode][关联后缀]]:1]]
(defcustom galactron-nov-enabled-auto-mode t
  "关联后缀"
  :type 'boolean
  :tag "Auto mode")

(when galactron-nov-enabled-auto-mode
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
;; [[help:galactron-nov-enabled-auto-mode][关联后缀]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-nov-enabled-visual-line\]\[自动折行\]\]][[[help:galactron-nov-enabled-visual-line][自动折行]]:1]]
(defcustom galactron-nov-enabled-visual-line t
  "自动折行"
  :type 'boolean
  :tag "Visual line")

(when galactron-nov-enabled-visual-line
  (add-hook 'nov-mode-hook 'visual-line-mode))
;; [[help:galactron-nov-enabled-visual-line][自动折行]]:1 ends here
(provide 'galactron-nov)
;;; galactron-nov.el ends here
