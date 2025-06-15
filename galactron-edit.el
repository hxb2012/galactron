;;; galactron-edit.el --- 修改编辑设置  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-edit t "启用修改编辑设置" :type 'boolean :tag "Edit")

(defgroup galactron-edit nil
  "修改编辑设置"
  :prefix "galactron-edit-"
  :tag "Edit"
  :group 'galactron)
;; [[file:README.org::*\[\[help:galactron-edit-enabled-no-electric-indent\]\[禁用回车自动缩进\]\]][[[help:galactron-edit-enabled-no-electric-indent][禁用回车自动缩进]]:1]]
(defcustom galactron-edit-enabled-no-electric-indent t
  "禁用回车自动缩进"
  :type 'boolean
  :tag "No electric indent")

(when galactron-edit-enabled-no-electric-indent
  (galactron-default electric-indent-mode nil))
;; [[help:galactron-edit-enabled-no-electric-indent][禁用回车自动缩进]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-edit-enabled-delete-selection\]\[输入覆盖选中区域\]\]][[[help:galactron-edit-enabled-delete-selection][输入覆盖选中区域]]:1]]
(defcustom galactron-edit-enabled-delete-selection t
  "输入覆盖选中区域"
  :type 'boolean
  :tag "Delete selection")

(when galactron-edit-enabled-delete-selection
  (galactron-default delete-selection-mode t))
;; [[help:galactron-edit-enabled-delete-selection][输入覆盖选中区域]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-edit-enabled-no-indent-tabs\]\[禁用Tab缩进\]\]][[[help:galactron-edit-enabled-no-indent-tabs][禁用Tab缩进]]:1]]
(defcustom galactron-edit-enabled-no-indent-tabs t
  "禁用Tab缩进"
  :type 'boolean
  :tag "No indent tabs")
;; [[help:galactron-edit-enabled-no-indent-tabs][禁用Tab缩进]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-edit-enabled-no-indent-tabs\]\[禁用Tab缩进\]\]][[[help:galactron-edit-enabled-no-indent-tabs][禁用Tab缩进]]:2]]
(defun galactron-edit--enable-indent-tabs-mode-h ()
  (indent-tabs-mode t))

(when galactron-edit-enabled-no-indent-tabs
  (galactron-default indent-tabs-mode nil)
  (add-hook 'makefile-mode-hook
            'galactron-edit--enable-indent-tabs-mode-h))
;; [[help:galactron-edit-enabled-no-indent-tabs][禁用Tab缩进]]:2 ends here
(provide 'galactron-edit)
;;; galactron-edit.el ends here
