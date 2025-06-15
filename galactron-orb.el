;;; galactron-orb.el --- 修改Orb设置  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-orb t "启用修改Orb设置" :type 'boolean :tag "Orb")

(defgroup galactron-orb nil
  "修改Orb设置"
  :prefix "galactron-orb-"
  :tag "Orb"
  :group 'galactron)
;; [[file:README.org::*\[\[elisp:(customize-group 'galactron-orb)\]\[Org-Mode SQLite缓存\]\] (请先\[\[elisp:(package-install 'orb)\]\[安装orb\]\])][[[elisp:(customize-group 'galactron-orb)][Org-Mode SQLite缓存]] (请先[[elisp:(package-install 'orb)][安装orb]]):2]]
(galactron-default orb-db-location
                (locate-user-emacs-file "orb.sqlite"))
;; [[elisp:(customize-group 'galactron-orb)][Org-Mode SQLite缓存]] (请先[[elisp:(package-install 'orb)][安装orb]]):2 ends here
;; [[file:README.org::*\[\[help:galactron-orb-enabled-roam-refs\]\[ROAM_REFS\]\]][[[help:galactron-orb-enabled-roam-refs][ROAM_REFS]]:1]]
(defcustom galactron-orb-enabled-roam-refs t
  "ROAM_REFS"
  :type 'boolean
  :tag "ROAM_REFS")

(when galactron-orb-enabled-roam-refs
  (galactron-with-package-installed orb
    (galactron-default orb-property-refs "ROAM_REFS")))
;; [[help:galactron-orb-enabled-roam-refs][ROAM_REFS]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-orb-enabled-auto-sync\]\[保存时自动同步\]\]][[[help:galactron-orb-enabled-auto-sync][保存时自动同步]]:1]]
(defcustom galactron-orb-enabled-auto-sync t
  "保存时自动同步"
  :type 'boolean
  :tag "Auto Sync")

(when galactron-orb-enabled-auto-sync
  (with-eval-after-load 'org
    (orb-db-autosync-mode t)))
;; [[help:galactron-orb-enabled-auto-sync][保存时自动同步]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-orb-enabled-smart-refresh\]\[尽量避免刷新\]\]][[[help:galactron-orb-enabled-smart-refresh][尽量避免刷新]]:1]]
(defcustom galactron-orb-enabled-smart-refresh t
  "尽量避免刷新"
  :type 'boolean
  :tag "Smart Refresh")

(when galactron-orb-enabled-smart-refresh
  (with-eval-after-load 'org
    (orb-db-smartrefresh-mode t)))
;; [[help:galactron-orb-enabled-smart-refresh][尽量避免刷新]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-orb-enabled-id-mode\]\[ID链接增强\]\]][[[help:galactron-orb-enabled-id-mode][ID链接增强]]:1]]
(defcustom galactron-orb-enabled-id-mode t
  "ID链接增强"
  :type 'boolean
  :tag "Orb ID mode")

(when galactron-orb-enabled-id-mode
  (with-eval-after-load 'org
    (orb-id-mode t)
    (keymap-set org-mode-map "C-c i i" 'orb-id-insert)
    (keymap-set org-mode-map "C-c i t" 'orb-tag-insert)))
;; [[help:galactron-orb-enabled-id-mode][ID链接增强]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-orb-enabled-agenda\]\[从缓存查询Agenda\]\]][[[help:galactron-orb-enabled-agenda][从缓存查询Agenda]]:1]]
(defcustom galactron-orb-enabled-agenda t
  "从缓存查询Agenda"
  :type 'boolean
  :tag "Orb Agenda Mode")

(when galactron-orb-enabled-agenda
  (with-eval-after-load 'org-agenda
    (orb-agenda-mode t)))
;; [[help:galactron-orb-enabled-agenda][从缓存查询Agenda]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-orb-enabled-sidebar\]\[侧栏增强\]\]][[[help:galactron-orb-enabled-sidebar][侧栏增强]]:1]]
(defcustom galactron-orb-enabled-sidebar t
  "侧栏增强"
  :type 'boolean
  :tag "Orb Sidebar DB mode")

(when galactron-org-enabled-sidebar
  (when galactron-orb-enabled-sidebar
    (galactron-with-package-installed orb
      (orb-sidebar-db-mode t))))
;; [[help:galactron-orb-enabled-sidebar][侧栏增强]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-orb-enabled-noter-capture\]\[Noter自动收录文件\]\]][[[help:galactron-orb-enabled-noter-capture][Noter自动收录文件]]:1]]
(defcustom galactron-orb-enabled-noter-capture t
  "Noter自动收录文件"
  :type 'boolean
  :tag "Orb Noter Capture")

(when galactron-orb-enabled-noter-capture
  (advice-add 'orb-capture-noter :override 'orb-capture-asset)
  (with-eval-after-load 'orb-noter
    (galactron-default-member
     orb-noter--find-doc-node-functions
     'orb-asset-noter--find-doc-node)
    (galactron-default-member
     orb-noter--find-doc-buffer-functions
     'orb-asset-noter--find-doc-buffer)))
;; [[help:galactron-orb-enabled-noter-capture][Noter自动收录文件]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-orb-enabled-capture\]\[Capture增强\]\]][[[help:galactron-orb-enabled-capture][Capture增强]]:1]]
(defcustom galactron-orb-enabled-capture t
  "Capture增强"
  :type 'boolean
  :tag "Orb Capture DB mode")

(when galactron-orb-enabled-capture
  (galactron-with-package-installed orb
    (orb-capture-db-mode t)))
;; [[help:galactron-orb-enabled-capture][Capture增强]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-orb-enabled-archive\]\[Archive增强\]\]][[[help:galactron-orb-enabled-archive][Archive增强]]:1]]
(defcustom galactron-orb-enabled-archive t
  "Archive增强"
  :type 'boolean
  :tag "Orb Archive")

(when galactron-orb-enabled-archive
  (galactron-with-package-installed orb
    (galactron-default org-archive-default-command 'orb-archive)))
;; [[help:galactron-orb-enabled-archive][Archive增强]]:1 ends here
(provide 'galactron-orb)
;;; galactron-orb.el ends here
