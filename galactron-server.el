;;; galactron-server.el --- 修改Server设置  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-server t "启用修改Server设置" :type 'boolean :tag "Server")

(defgroup galactron-server nil
  "修改Server设置"
  :prefix "galactron-server-"
  :tag "Server"
  :group 'galactron)
;; [[file:README.org::*\[\[elisp:(customize-group 'galactron-server)\]\[Server\]\]][[[elisp:(customize-group 'galactron-server)][Server]]:2]]
(require 'server)
(unless (server-running-p)
  (server-start))
;; [[elisp:(customize-group 'galactron-server)][Server]]:2 ends here
(provide 'galactron-server)
;;; galactron-server.el ends here
