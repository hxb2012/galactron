;;; galactron-pdf.el --- 修改PDF Tools设置  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-pdf t "启用修改PDF Tools设置" :type 'boolean :tag "Pdf")

(defgroup galactron-pdf nil
  "修改PDF Tools设置"
  :prefix "galactron-pdf-"
  :tag "Pdf"
  :group 'galactron)
;; [[file:README.org::*\[\[help:galactron-pdf-enabled-auto-mode\]\[关联后缀\]\]][[[help:galactron-pdf-enabled-auto-mode][关联后缀]]:1]]
(defcustom galactron-pdf-enabled-auto-mode t
  "关联后缀"
  :type 'boolean
  :tag "Auto mode")

(when galactron-pdf-enabled-auto-mode
  (pdf-loader-install))
;; [[help:galactron-pdf-enabled-auto-mode][关联后缀]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-pdf-enabled-python-server\]\[Python Server\]\]][[[help:galactron-pdf-enabled-python-server][Python Server]]:1]]
(defcustom galactron-pdf-enabled-python-server t
  "Python Server

- epdfinfo
  使用Poppler
- evpdfinfo
  使用Evince"
  :type '(choice
          (const :tag "epdfinfo" t)
          (const :tag "evpdfinfo" evpdfinfo)
          (const :tag "禁用" nil))
  :tag "Python Server")

(when galactron-pdf-enabled-python-server
  (galactron-default
   pdf-info-epdfinfo-program
   (file-name-concat
    (file-name-directory load-file-name)
    (pcase galactron-pdf-enabled-python-server
      ('evpdfinfo "evpdfinfo")
      (_ "epdfinfo")))))
;; [[help:galactron-pdf-enabled-python-server][Python Server]]:1 ends here
(provide 'galactron-pdf)
;;; galactron-pdf.el ends here
