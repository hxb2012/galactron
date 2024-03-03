;;; galactron-accessories.el --- 花里胡哨  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-accessories t "启用花里胡哨" :type 'boolean :tag "Accessories")

(defgroup galactron-accessories nil
  "花里胡哨"
  :prefix "galactron-accessories-"
  :tag "Accessories"
  :group 'galactron)
;; [[file:README.org::*\[\[help:galactron-accessories-enabled-reminder\]\[启动退出提醒\]\]][[[help:galactron-accessories-enabled-reminder][启动退出提醒]]:1]]
(defcustom galactron-accessories-enabled-reminder t
  "启动退出提醒"
  :type 'boolean
  :tag "Reminder")

(defcustom galactron-accessories-reminder-hook nil
  "启动退出Emacs时提醒"
  :type 'hook
  :tag "Reminder hook")

(defun galactron-accessories--reminder-run-hooks ()
  (run-hooks 'galactron-accessories-reminder-hook))

(defun galactron-accessories--reminder-confirm-kill (prompt)
  (galactron-accessories--reminder-run-hooks)
  (yes-or-no-p prompt))

(when galactron-accessories-enabled-reminder
  (add-hook 'window-setup-hook
            'galactron-accessories--reminder-run-hooks)
  (galactron-default confirm-kill-emacs
                  'galactron-accessories--reminder-confirm-kill))
;; [[help:galactron-accessories-enabled-reminder][启动退出提醒]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-accessories-enabled-sticker\]\[便利贴\]\]][[[help:galactron-accessories-enabled-sticker][便利贴]]:1]]
(defcustom galactron-accessories-enabled-sticker t
  "便利贴"
  :type 'boolean
  :tag "Sticker")

(defvar galactron-accessories--sticker-windows nil)

(defcustom galactron-accessories-sticker-functions nil
  "返回Sticker Buffer"
  :type '(repeat function)
  :tag "Sticker functions")

(defun galactron-accessories-sticker-toggle (&optional arg)
  (interactive "P")
  (let ((valid-windows
         (seq-filter 'window-valid-p
                     galactron-accessories--sticker-windows)))
    (when valid-windows
      (dolist (window valid-windows)
        (quit-window t window)))
    (when (or arg
              (not valid-windows))
      (setq galactron-accessories--sticker-windows
            (seq-map-indexed
             (lambda (f i)
               (display-buffer-in-side-window
                (funcall f)
                `((side . top) (slot . ,i))))
             galactron-accessories-sticker-functions)))))

(defun galactron-accessories--sticker-show ()
  (galactron-accessories-sticker-toggle t))

(when galactron-accessories-enabled-sticker
  (add-hook 'galactron-accessories-reminder-hook
            'galactron-accessories--sticker-show)
  (keymap-global-set "C-`" 'galactron-accessories-sticker-toggle))
;; [[help:galactron-accessories-enabled-sticker][便利贴]]:1 ends here
(provide 'galactron-accessories)
;;; galactron-accessories.el ends here
