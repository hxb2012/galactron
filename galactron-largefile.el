;;; galactron-largefile.el --- 修改largefile设置  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-largefile t "启用修改largefile设置" :type 'boolean :tag "Largefile")

(defgroup galactron-largefile nil
  "修改largefile设置"
  :prefix "galactron-largefile-"
  :tag "Largefile"
  :group 'galactron)
;; [[file:README.org::*\[\[elisp:(customize-group 'galactron-largefile)\]\[largefile\]\] (请先\[\[elisp:(package-install 'largefile)\]\[安装largefile\]\])][[[elisp:(customize-group 'galactron-largefile)][largefile]] (请先[[elisp:(package-install 'largefile)][安装largefile]]):2]]
(largefile-mode t)
;; [[elisp:(customize-group 'galactron-largefile)][largefile]] (请先[[elisp:(package-install 'largefile)][安装largefile]]):2 ends here
;; [[file:README.org::*\[\[help:galactron-largefile-enabled-docview\]\[DocView\]\]][[[help:galactron-largefile-enabled-docview][DocView]]:1]]
(defcustom galactron-largefile-enabled-docview t
  "DocView"
  :type 'boolean
  :tag "DocView")

(when galactron-largefile-enabled-docview
  (dolist (item auto-mode-alist)
    (when (eq (cdr item) 'doc-view-mode-maybe)
      (add-to-list 'largefile-mode-alist item))))
;; [[help:galactron-largefile-enabled-docview][DocView]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-largefile-enabled-pdf\]\[PDF Tools\]\]][[[help:galactron-largefile-enabled-pdf][PDF Tools]]:1]]
(defcustom galactron-largefile-enabled-pdf t
  "PDF Tools"
  :type 'boolean
  :tag "PDF-Tools")

(when galactron-largefile-enabled-pdf
  (galactron-when-package-installed pdf-tools
    (dolist (item auto-mode-alist)
      (when (equal
             (cdr item)
             (let ((args (list nil nil nil nil)))
               (lambda ()
                 (apply #'pdf-loader--load args))))
        (add-to-list 'largefile-mode-alist item)))))
;; [[help:galactron-largefile-enabled-pdf][PDF Tools]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-largefile-enabled-nov\]\[Nov\]\]][[[help:galactron-largefile-enabled-nov][Nov]]:1]]
(defcustom galactron-largefile-enabled-nov t
  "Nov"
  :type 'boolean
  :tag "Nov")

(when galactron-largefile-enabled-nov
  (galactron-when-package-installed nov
    (dolist (item auto-mode-alist)
      (when (eq (cdr item) 'nov-mode)
        (add-to-list 'largefile-mode-alist item)))))
;; [[help:galactron-largefile-enabled-nov][Nov]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-largefile-enabled-djvu\]\[Djvu\]\]][[[help:galactron-largefile-enabled-djvu][Djvu]]:1]]
(defcustom galactron-largefile-enabled-djvu t
  "Djvu"
  :type 'boolean
  :tag "Djvu")

(when galactron-largefile-enabled-djvu
  (galactron-when-package-installed djvu
    (dolist (item auto-mode-alist)
      (when (eq (cdr item) 'djvu-init-mode)
        (add-to-list 'largefile-mode-alist item)))))
;; [[help:galactron-largefile-enabled-djvu][Djvu]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-largefile-enabled-ssp\]\[SSP\]\]][[[help:galactron-largefile-enabled-ssp][SSP]]:1]]
(defcustom galactron-largefile-enabled-ssp t
  "SSP"
  :type 'boolean
  :tag "SSP")

(when galactron-largefile-enabled-ssp
  (galactron-when-package-installed ssp
    (dolist (item auto-mode-alist)
      (when (eq (cdr item) 'ssp-mode)
        (add-to-list 'largefile-mode-alist item)))))
;; [[help:galactron-largefile-enabled-ssp][SSP]]:1 ends here
(provide 'galactron-largefile)
;;; galactron-largefile.el ends here
