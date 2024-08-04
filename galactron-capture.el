;;; galactron-capture.el --- Org Capture  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-capture t "启用Org Capture" :type 'boolean :tag "Capture")

(defgroup galactron-capture nil
  "Org Capture"
  :prefix "galactron-capture-"
  :tag "Capture"
  :group 'galactron)
;; [[file:README.org::*\[\[elisp:(customize-group 'galactron-capture)\]\[Org Capture\]\]][[[elisp:(customize-group 'galactron-capture)][Org Capture]]:2]]
(require 'orb-capture)

;;;###autoload
(defun galactron-capture-collect-targets ()
  (save-excursion
    (save-restriction
      (widen)
      (apply 'append (mapcar 'orb-capture-collect
                             (mapcar 'car orb-capture-targets))))))

;;;###autoload
(defun galactron-capture-define-targets (targets)
  (pcase-dolist (`(,capture ,key ,args) targets)
    (when-let ((symbol
                (assoc-default capture orb-capture-targets)))
      (galactron--default-assoc symbol key args))))
;; [[elisp:(customize-group 'galactron-capture)][Org Capture]]:2 ends here
;; [[file:README.org::*\[\[help:galactron-capture-enabled-note\]\[随手记\]\]][[[help:galactron-capture-enabled-note][随手记]]:1]]
(defcustom galactron-capture-enabled-note t
  "随手记"
  :type 'boolean
  :tag "Note")

(when galactron-capture-enabled-note
  (galactron-shortcut-add-item
   "<capture-note>" "随手记"
   'orb-capture-note
   :help "随手记")
  (keymap-global-set "C-x M-n" 'orb-capture-note))
;; [[help:galactron-capture-enabled-note][随手记]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-capture-enabled-todo\]\[任务\]\]][[[help:galactron-capture-enabled-todo][任务]]:1]]
(defcustom galactron-capture-enabled-todo t
  "任务"
  :type 'boolean
  :tag "TODO")

(when galactron-capture-enabled-todo
  (galactron-shortcut-add-item
   "<capture-todo-with-context>" "记录关联任务"
   'orb-capture-todo-with-context
   :enable '(derived-mode-p 'org-mode)
   :help "记录关联任务")

  (galactron-shortcut-add-item
   "<capture-todo>" "记录无关任务"
   'orb-capture-todo
   :help "记录无关任务")

  (keymap-global-set "C-x M-t" 'orb-capture-todo)
  (with-eval-after-load 'org
    (keymap-set org-mode-map
                "C-c t" 'orb-capture-todo-with-context)))
;; [[help:galactron-capture-enabled-todo][任务]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-capture-enabled-clip\]\[网页剪辑\]\] (请先\[\[elisp:(package-install 'html2org)\]\[安装htmlorg\]\])][[[help:galactron-capture-enabled-clip][网页剪辑]] (请先[[elisp:(package-install 'html2org)][安装htmlorg]]):1]]
(defcustom galactron-capture-enabled-clip t
  "网页剪辑"
  :type 'boolean
  :tag "Clip")

(when galactron-capture-enabled-clip
  (galactron-with-package-installed html2org
    (advice-add 'server-execute
                :before 'orb-capture-clip--load-protocol)))
;; [[help:galactron-capture-enabled-clip][网页剪辑]] (请先[[elisp:(package-install 'html2org)][安装htmlorg]]):1 ends here
;; [[file:README.org::*\[\[help:galactron-capture-enabled-asset\]\[收录文件\]\]][[[help:galactron-capture-enabled-asset][收录文件]]:1]]
(defcustom galactron-capture-enabled-asset t
  "收录文件"
  :type 'boolean
  :tag "Asset")

(when galactron-capture-enabled-asset
  (galactron-shortcut-add-item
   "<capture-asset>" "收录文件"
   'orb-capture-asset
   :help "收录文件")
  (keymap-global-set "C-x M-a" 'orb-capture-asset))
;; [[help:galactron-capture-enabled-asset][收录文件]]:1 ends here
(provide 'galactron-capture)
;;; galactron-capture.el ends here
