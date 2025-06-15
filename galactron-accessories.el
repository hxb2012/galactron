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

(defcustom galactron-accessories-sticker-functions nil
  "返回Sticker Buffer"
  :type '(repeat function)
  :tag "Sticker functions")

(defun galactron-accessories-sticker-toggle (&optional arg)
  (interactive "P")
  (when galactron-accessories-sticker-functions
    (let ((has-side-window (window-with-parameter 'window-side nil)))
      (when has-side-window
        (window-toggle-side-windows))
      (when (or arg (not has-side-window))
        (seq-map-indexed
         (lambda (f i)
           (with-selected-window
               (display-buffer-in-side-window
                (get-scratch-buffer-create)
                `((side . top) (slot . ,i) (dedicated . t)))
             (with-window-non-dedicated nil
               (funcall f))
             (selected-window)))
         galactron-accessories-sticker-functions)))))

(defun galactron-accessories--sticker-show ()
  (galactron-accessories-sticker-toggle t))

(when galactron-accessories-enabled-sticker
  (add-hook 'galactron-accessories-reminder-hook
            'galactron-accessories--sticker-show)
  (keymap-global-set "C-`" 'galactron-accessories-sticker-toggle))
;; [[help:galactron-accessories-enabled-sticker][便利贴]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-accessories-enabled-sticker-remember\]\[Remember\]\]][[[help:galactron-accessories-enabled-sticker-remember][Remember]]:1]]
(defcustom galactron-accessories-enabled-sticker-remember t
  "Remember便利贴"
  :type 'boolean
  :tag "Remember Sticker")

(defun galactron-accessories--sticker--remember ()
  (remember-notes t))

(when galactron-accessories-enabled-sticker-remember
  (when galactron-org-enabled-remember
    (galactron-default-member
     galactron-accessories-sticker-functions
     'galactron-accessories--sticker--remember)))
;; [[help:galactron-accessories-enabled-sticker-remember][Remember]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-accessories-enabled-sticker-diary\]\[日程表\]\]][[[help:galactron-accessories-enabled-sticker-diary][日程表]]:1]]
(defcustom galactron-accessories-enabled-sticker-diary t
  "日程表 便利贴"
  :type 'boolean
  :tag "Diary Sticker")

(defcustom galactron-accessories-sticker-diary-buffer-name
  "*Org Diary*"
  "Diary buffer name"
  :type 'string
  :tag "Diary buffer name")

(defun galactron-accessories--sticker--diary ()
  (let ((org-agenda-buffer-name
         galactron-accessories-sticker-diary-buffer-name))
    (galactron-agenda-diary))
  (goto-char (point-min))
  (when (text-property-search-forward 'face 'org-agenda-current-time)
    (recenter)))

(when galactron-accessories-enabled-sticker-diary
  (when galactron-agenda-enabled-diary
    (galactron-default-member
     galactron-accessories-sticker-functions
     'galactron-accessories--sticker--diary)))
;; [[help:galactron-accessories-enabled-sticker-diary][日程表]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-accessories-enabled-sticker-dashboard\]\[Dashboard\]\]][[[help:galactron-accessories-enabled-sticker-dashboard][Dashboard]]:1]]
(defcustom galactron-accessories-enabled-sticker-dashboard t
  "Dashboard便利贴"
  :type 'boolean
  :tag "Dashboard Sticker")

(defcustom galactron-accessories-sticker-dashboard-buffer-name
  "*Org Dashboard*"
  "Dashboard buffer name"
  :type 'string
  :tag "Dashboard buffer name")

(defun galactron-accessories--sticker--dashboard ()
  (let ((org-agenda-buffer-name
         galactron-accessories-sticker-dashboard-buffer-name))
    (galactron-agenda-dashboard))
  (when (org-clocking-p)
    (unless (eq org-agenda-show-log 'clockcheck)
      (save-excursion
        (goto-char (point-min))
        (org-agenda-update-agenda-type)
        (org-agenda-log-mode 'clockcheck)))))

(when galactron-accessories-enabled-sticker-dashboard
  (when galactron-agenda-enabled-dashboard
    (galactron-default-member
     galactron-accessories-sticker-functions
     'galactron-accessories--sticker--dashboard)))
;; [[help:galactron-accessories-enabled-sticker-dashboard][Dashboard]]:1 ends here
(provide 'galactron-accessories)
;;; galactron-accessories.el ends here
