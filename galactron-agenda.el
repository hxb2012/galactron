;;; galactron-agenda.el --- 日程  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-agenda t "启用日程" :type 'boolean :tag "Agenda")

(defgroup galactron-agenda nil
  "日程"
  :prefix "galactron-agenda-"
  :tag "Agenda"
  :group 'galactron)
;; [[file:README.org::*\[\[elisp:(customize-group 'galactron-agenda)\]\[日程\]\]][[[elisp:(customize-group 'galactron-agenda)][日程]]:2]]
(defmacro galactron-agenda-add ()
  (let ((file (file-name-with-extension load-file-name ".org")))
    `(with-eval-after-load 'org-agenda
       (galactron-default-member org-agenda-files ,file))))
;; [[elisp:(customize-group 'galactron-agenda)][日程]]:2 ends here
;; [[file:README.org::*\[\[help:galactron-agenda-enabled-todo-state\]\[TODO状态\]\]][[[help:galactron-agenda-enabled-todo-state][TODO状态]]:1]]
(defcustom galactron-agenda-enabled-todo-state t
  "TODO状态"
  :type 'boolean
  :tag "TODO State")

(defun galactron-agenda--clock-in-h ()
  (when (equal (org-get-todo-state) "TODO")
    (org-todo "WIP")))

(when galactron-agenda-enabled-todo-state
  (with-eval-after-load 'org
    (galactron-default org-todo-keywords
                    '((sequence "TODO" "WIP" "|" "DONE")))
    (add-hook 'org-clock-in-hook 'galactron-agenda--clock-in-h)))
;; [[help:galactron-agenda-enabled-todo-state][TODO状态]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-agenda-enabled-dashboard\]\[Dashboard\]\] (请先\[\[elisp:(package-install 'org-super-agenda)\]\[安装org-super-agenda\]\])][[[help:galactron-agenda-enabled-dashboard][Dashboard]] (请先[[elisp:(package-install 'org-super-agenda)][安装org-super-agenda]]):1]]
(defcustom galactron-agenda-enabled-dashboard t
  "Dashboard"
  :type 'boolean
  :tag "Dashboard")

(defun galactron-agenda-dashboard ()
  (let ((org-agenda-window-setup 'current-window))
    (org-agenda nil "d")))

(when galactron-agenda-enabled-dashboard
  (galactron-with-package-installed org-super-agenda
    (with-eval-after-load 'org-agenda
      (org-super-agenda-mode t)
      (galactron-default-assoc
       org-agenda-custom-commands
       "d"
       '("Dashboard"
         ((agenda
           ""
           ((org-agenda-overriding-header "DEADLINE")
            (org-agenda-span 1)
            (org-agenda-time-grid nil)
            (org-agenda-show-all-dates nil)
            (org-agenda-entry-types '(:deadline))
            (org-deadline-warning-days 30)
            (org-agenda-sorting-strategy
             '(deadline-up priority-down))))
          (agenda
           ""
           ((org-agenda-overriding-header "TODO")
            (org-agenda-span 8)
            (org-agenda-time-grid nil)
            (org-agenda-show-all-dates nil)
            (org-agenda-skip-function
             '(or (org-agenda-skip-entry-if 'deadline)
                  (org-agenda-skip-entry-if 'todo 'done)))
            (org-agenda-sorting-strategy
             '(priority-down scheduled-up))
            (org-super-agenda-groups
             '((:name "WIP" :todo ("WIP"))
               (:name "INBOX" :not (:habit t))
               (:name "HABIT" :habit t)))))
          (tags
           "+SCHEDULED>=\"<+7d>\"-STYLE=\"habit\""
           ((org-agenda-sorting-strategy
             '(priority-down scheduled-up))
            (org-agenda-overriding-header "UPCOMING")))
          (todo
           "TODO"
           ((org-agenda-overriding-header "Backlog")
            (org-agenda-todo-ignore-with-date t)
            (org-agenda-sorting-strategy '(priority-down))))))))

    (galactron-shortcut-add-item
     "<agenda-dashboard>" "待办事项"
     'galactron-agenda-dashboard
     :help "待办事项")
    (keymap-global-set "C-x M-d" 'galactron-agenda-dashboard)))
;; [[help:galactron-agenda-enabled-dashboard][Dashboard]] (请先[[elisp:(package-install 'org-super-agenda)][安装org-super-agenda]]):1 ends here
;; [[file:README.org::*\[\[help:galactron-agenda-enabled-diary\]\[日程表\]\]][[[help:galactron-agenda-enabled-diary][日程表]]:1]]
(defcustom galactron-agenda-enabled-diary t
  "Diary"
  :type 'boolean
  :tag "Diary")

(defun galactron-agenda-diary ()
  (let ((org-agenda-window-setup 'current-window))
    (org-agenda nil "a")))

(when galactron-agenda-enabled-diary
  (with-eval-after-load 'org-agenda
    (galactron-default-assoc
     org-agenda-custom-commands
     "a"
     '("日程表"
       ((agenda*
         ""
         ((org-agenda-span 'day)
          (org-agenda-include-diary t)))))))

  (galactron-shortcut-add-item
   "<agenda-agenda>" "日程表"
   'galactron-agenda-diary
   :help "日程表"))
;; [[help:galactron-agenda-enabled-diary][日程表]]:1 ends here
(provide 'galactron-agenda)
;;; galactron-agenda.el ends here
