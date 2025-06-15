;;; galactron-shortcut.el --- 快捷菜单  -*- coding: utf-8; lexical-binding: t; -*-
;; [[file:README.org::*\[\[help:galactron-display-enabled-shortcut-menu\]\[快捷菜单\]\]][[[help:galactron-display-enabled-shortcut-menu][快捷菜单]]:3]]
(defvar galactron-shortcut-menu
  (make-sparse-keymap "Galactron Shortcut"))

;;;###autoload
(defun galactron-shortcut-add-item (key name binding &rest plist)
  (funcall
   (if (keymap-lookup galactron-shortcut-menu key)
       'keymap-set
     'keymap-set-after)
   galactron-shortcut-menu
   key
   (apply 'list 'menu-item name binding plist)))

;;;###autoload
(defun galactron-shortcut-add-separator (key)
  (galactron-shortcut-add-item key
                            "--" 'ignore
                            :enable nil))
;; [[help:galactron-display-enabled-shortcut-menu][快捷菜单]]:3 ends here
(provide 'galactron-shortcut)
;;; galactron-shortcut.el ends here
