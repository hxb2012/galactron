;;; galactron-wanderlust.el --- 修改Wanderlust设置  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-wanderlust t "启用修改Wanderlust设置" :type 'boolean :tag "Wanderlust")

(defgroup galactron-wanderlust nil
  "修改Wanderlust设置"
  :prefix "galactron-wanderlust-"
  :tag "Wanderlust"
  :group 'galactron)
;; [[file:README.org::*\[\[help:galactron-wanderlust-enabled-no-prefetch-threshold\]\[禁用下载上限\]\]][[[help:galactron-wanderlust-enabled-no-prefetch-threshold][禁用下载上限]]:1]]
(defcustom galactron-wanderlust-enabled-no-prefetch-threshold t
  "禁用下载上限"
  :type 'boolean
  :tag "No prefresh threshold")

(when galactron-wanderlust-enabled-no-prefetch-threshold
  (galactron-default wl-prefetch-threshold nil))
;; [[help:galactron-wanderlust-enabled-no-prefetch-threshold][禁用下载上限]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-wanderlust-enabled-accept-quoted-encoded\]\[接受quoted编码\]\]][[[help:galactron-wanderlust-enabled-accept-quoted-encoded][接受quoted编码]]:1]]
(defcustom galactron-wanderlust-enabled-accept-quoted-encoded t
  "接受quoted编码"
  :type 'boolean
  :tag "Accept quoted encoded")

(when galactron-wanderlust-enabled-accept-quoted-encoded
  (galactron-default mime-header-accept-quoted-encoded-words t))
;; [[help:galactron-wanderlust-enabled-accept-quoted-encoded][接受quoted编码]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-wanderlust-enabled-file-location\]\[文件位置\]\]][[[help:galactron-wanderlust-enabled-file-location][文件位置]]:1]]
(defcustom galactron-wanderlust-enabled-file-location t
  "文件位置"
  :type 'boolean
  :tag "File location")

(when galactron-wanderlust-enabled-file-location
  (galactron-default wl-init-file
                  (expand-file-name "wl/init" user-emacs-directory))
  (galactron-default wl-folders-file
                (expand-file-name "wl/folders" user-emacs-directory))
  (galactron-default wl-address-file
                (expand-file-name "wl/address" user-emacs-directory))
  (galactron-default wl-alias-file
                (expand-file-name "wl/alias" user-emacs-directory))
  (galactron-default wl-temporary-file-directory
                  (expand-file-name "wl/tmp" user-emacs-directory))
  (galactron-default elmo-msgdb-directory
                (expand-file-name "elmo" user-emacs-directory))
  (galactron-default elmo-archive-folder-path
                (expand-file-name "Mail" user-emacs-directory))
  (galactron-default elmo-localdir-folder-path
                (expand-file-name "Mail" user-emacs-directory))
  (galactron-default elmo-maildir-folder-path
                  (expand-file-name "Maildir" user-emacs-directory))
  (galactron-default elmo-search-namazu-default-index-path
                  (expand-file-name "Mail" user-emacs-directory)))
;; [[help:galactron-wanderlust-enabled-file-location][文件位置]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-wanderlust-enabled-field-list\]\[显示字段\]\]][[[help:galactron-wanderlust-enabled-field-list][显示字段]]:1]]
(defcustom galactron-wanderlust-enabled-field-list t
  "显示字段"
  :type 'boolean
  :tag "Field list")

(when galactron-wanderlust-enabled-field-list
  (galactron-default wl-message-ignored-field-list '("^.*:"))
  (galactron-default wl-message-visible-field-list
                  '("^To:" "^Cc:" "^From:" "^Subject:" "^Date:"))
  (galactron-default wl-message-sort-field-list
                  '("^From:" "^Subject:" "^Date:" "^To:" "^Cc:")))
;; [[help:galactron-wanderlust-enabled-field-list][显示字段]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-wanderlust-enabled-summary-line-format\]\[Summary格式\]\]][[[help:galactron-wanderlust-enabled-summary-line-format][Summary格式]]:1]]
(defcustom galactron-wanderlust-enabled-summary-line-format t
  "Summary格式"
  :type 'boolean
  :tag "Summary line format")

(when galactron-wanderlust-enabled-summary-line-format
  (galactron-default wl-summary-line-format
                  "%n%T%P%Y-%M-%D(%W) %t%[%17(%c %f%) %] %s"))
;; [[help:galactron-wanderlust-enabled-summary-line-format][Summary格式]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-wanderlust-enabled-mail-user-agent\]\[设置为Emacs邮件User-Agent\]\]][[[help:galactron-wanderlust-enabled-mail-user-agent][设置为Emacs邮件User-Agent]]:1]]
(defcustom galactron-wanderlust-enabled-mail-user-agent t
  "设置为Emacs邮件User-Agent"
  :type 'boolean
  :tag "Mail user agent")

(when galactron-wanderlust-enabled-mail-user-agent
  (define-mail-user-agent
    'wl-user-agent
    'wl-user-agent-compose
    'wl-draft-send
    'wl-draft-kill
    'mail-send-hook)
  (galactron-default mail-user-agent 'wl-user-agent))
;; [[help:galactron-wanderlust-enabled-mail-user-agent][设置为Emacs邮件User-Agent]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-wanderlust-enabled-org-mime\]\[发送Org Mode里编辑的邮件\]\] (请先\[\[elisp:(package-install 'org-mime)\]\[安装org-mime\]\])][[[help:galactron-wanderlust-enabled-org-mime][发送Org Mode里编辑的邮件]] (请先[[elisp:(package-install 'org-mime)][安装org-mime]]):1]]
(defcustom galactron-wanderlust-enabled-org-mime t
  "发送Org Mode里编辑的邮件"
  :type 'boolean
  :tag "Org mime")

(when galactron-wanderlust-enabled-org-mime
  (galactron-with-package-installed org-mime
    (galactron-default org-mime-library 'semi)))
;; [[help:galactron-wanderlust-enabled-org-mime][发送Org Mode里编辑的邮件]] (请先[[elisp:(package-install 'org-mime)][安装org-mime]]):1 ends here
(provide 'galactron-wanderlust)
;;; galactron-wanderlust.el ends here
