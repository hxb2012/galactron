;;; galactron-display.el --- 修改显示设置  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-display t "启用修改显示设置" :type 'boolean :tag "Display")

(defgroup galactron-display nil
  "修改显示设置"
  :prefix "galactron-display-"
  :tag "Display"
  :group 'galactron)
;; [[file:README.org::*\[\[help:galactron-display-enabled-glyphless\]\[不可见字符\]\]][[[help:galactron-display-enabled-glyphless][不可见字符]]:1]]
(defcustom galactron-display-enabled-glyphless t
  "不可见字符"
  :type 'boolean
  :tag "Glyphless")

(defun galactron-display--enable-glyphless-h ()
  (glyphless-display-mode t))

(when galactron-display-enabled-glyphless
  (dolist (hook
           '(ert-simple-view-mode-hook
             ert-results-mode-hook
             messages-buffer-mode-hook
             text-mode-hook
             prog-mode-hook))
    (add-hook hook 'galactron-display--enable-glyphless-h)))
;; [[help:galactron-display-enabled-glyphless][不可见字符]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-display-enabled-whitespace\]\[高亮显示空白\]\]][[[help:galactron-display-enabled-whitespace][高亮显示空白]]:1]]
(defcustom galactron-display-enabled-whitespace t
  "高亮显示空白"
  :type 'boolean
  :tag "Whitespace")

(defun galactron-display--enable-whitespace-h ()
  (whitespace-mode t))

(when galactron-display-enabled-whitespace
  (galactron-default whitespace-style
                  '(face tabs trailing indentation tab-mark))
  (dolist (hook
           '(text-mode-hook
             prog-mode-hook))
    (add-hook hook 'galactron-display--enable-whitespace-h)))
;; [[help:galactron-display-enabled-whitespace][高亮显示空白]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-display-enabled-line-number\]\[行号\]\]][[[help:galactron-display-enabled-line-number][行号]]:1]]
(defcustom galactron-display-enabled-line-number t
  "行号"
  :type 'boolean
  :tag "Line number")

(defun galactron-display--enable-line-number-h ()
  (if (< emacs-major-version 29)
      (linum-mode t)
    (display-line-numbers-mode t)))

(when galactron-display-enabled-line-number
  (dolist (hook
           '(prog-mode-hook))
    (add-hook hook 'galactron-display--enable-line-number-h)))
;; [[help:galactron-display-enabled-line-number][行号]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-display-enabled-word-wrap-by-category\]\[中文折行\]\]][[[help:galactron-display-enabled-word-wrap-by-category][中文折行]]:1]]
(defcustom galactron-display-enabled-word-wrap-by-category t
  "中文折行"
  :type 'boolean
  :tag "Word wrap by category")

(when galactron-display-enabled-word-wrap-by-category
  (galactron-default word-wrap-by-category t))
;; [[help:galactron-display-enabled-word-wrap-by-category][中文折行]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-display-enabled-visual-fill-column\]\[按Fill Column折行\]\]][[[help:galactron-display-enabled-visual-fill-column][按Fill Column折行]]:1]]
(defcustom galactron-display-enabled-visual-fill-column t
  "按Fill Column折行"
  :type 'boolean
  :tag "Visual fill column")

(when galactron-display-enabled-visual-fill-column
  (galactron-with-package-installed visual-fill-column
    (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
    (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
    (galactron-default visual-fill-column-center-text t)
    (galactron-default
     visual-fill-column-enable-sensible-window-split t)
    (galactron-default visual-line-fringe-indicators '(nil  t))
    (galactron-default
     visual-fill-column-fringes-outside-margins nil)))
;; [[help:galactron-display-enabled-visual-fill-column][按Fill Column折行]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-display-enabled-font-setup\]\[中英文字符等宽\]\] (仅支持Flatpak)][[[help:galactron-display-enabled-font-setup][中英文字符等宽]] (仅支持Flatpak):1]]
(defcustom galactron-display-enabled-font-setup t
  "中英文字符等宽"
  :type 'boolean
  :tag "Font setup")

(defun galactron-display--font-setup-h ()
  (add-to-list 'face-font-rescale-alist '("Noto" . 1.2))
  (add-to-list 'face-font-rescale-alist '("Symbola" . 1.2))
  (set-fontset-font t 'symbol "Noto Sans CJK SC")
  (set-fontset-font t 'symbol "Symbola" nil 'append)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'emoji "Noto Sans CJK SC")
  (set-fontset-font t 'emoji "Symbola" nil 'append)
  (set-fontset-font t 'emoji "Noto Color Emoji" nil 'append))

(when (equal (getenv "container") "flatpak")
  (when galactron-display-enabled-font-setup
    (add-hook 'window-setup-hook 'galactron-display--font-setup-h)))
;; [[help:galactron-display-enabled-font-setup][中英文字符等宽]] (仅支持Flatpak):1 ends here
;; [[file:README.org::*\[\[help:galactron-display-enabled-font-setup\]\[中英文字符等宽\]\] (仅支持Flatpak)][[[help:galactron-display-enabled-font-setup][中英文字符等宽]] (仅支持Flatpak):2]]
(defun galactron-display--set-char-width (alist)
  (let ((table (make-char-table nil)))
    (dolist (pair alist)
      (set-char-table-range table (car pair) (cdr pair)))
    (optimize-char-table table)
    (set-char-table-parent table char-width-table)
    (setq char-width-table table)))

(when galactron-display-enabled-font-setup
  (galactron-display--set-char-width
   '(((?Ё . ?ё) . 1)
     (?№ . 1)
     (?ü . 1)
     (?‘ . 1)
     (?’ . 1)
     (?“ . 1)
     (?” . 1)
     (?’ . 1))))
;; [[help:galactron-display-enabled-font-setup][中英文字符等宽]] (仅支持Flatpak):2 ends here
;; [[file:README.org::*\[\[help:galactron-display-enabled-imenu\]\[IMenu\]\]][[[help:galactron-display-enabled-imenu][IMenu]]:1]]
(defcustom galactron-display-enabled-imenu t
  "Imenu"
  :type 'boolean
  :tag "Imenu")

(defun galactron-display--enable-imenu-h ()
  (ignore-errors
    (imenu-add-to-menubar "Imenu")))

(when galactron-display-enabled-imenu
  (dolist (hook
           '(font-lock-mode-hook))
    (add-hook hook 'galactron-display--enable-imenu-h)))
;; [[help:galactron-display-enabled-imenu][IMenu]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-display-enabled-appt\]\[Appointments\]\]][[[help:galactron-display-enabled-appt][Appointments]]:1]]
(defcustom galactron-display-enabled-appt t
  "Appointments"
  :type 'boolean
  :tag "Appointments")

(defvar galactron-display--appt-notification-id nil)

(defun galactron-display--appt-delete ()
  (require 'notifications)
  (when galactron-display--appt-notification-id
    (notifications-close-notification
     galactron-display--appt-notification-id)))

(defun galactron-display--appt-disp (min-to-app new-time appt-msg)
  (require 'notifications)
  (let ((title
         (and (string-match "[0-9:\-]+\s+\\([^:]+\\)" appt-msg)
              (match-string 1 appt-msg))))
    (setq galactron-display--appt-notification-id
          (notifications-notify
           :title title
           :body appt-msg
           :replace-id galactron-display--appt-notification-id
           :timeout (* 60000 (string-to-number min-to-app))))))

(when galactron-display-enabled-appt
  (galactron-default appt-display-diary nil)
  (galactron-default appt-display-mode-line nil)
  (galactron-default appt-disp-window-function
                     'galactron-display--appt-disp)
  (galactron-default appt-delete-window-function
                     'galactron-display--appt-delete)
  (add-hook 'window-setup-hook #'appt-activate))
;; [[help:galactron-display-enabled-appt][Appointments]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-display-enabled-shortcut-menu\]\[快捷菜单\]\]][[[help:galactron-display-enabled-shortcut-menu][快捷菜单]]:1]]
(defcustom galactron-display-enabled-shortcut-menu t
  "快捷菜单"
  :type 'boolean
  :tag "Shortcut Menu")

(when galactron-display-enabled-shortcut-menu
  (require 'galactron-shortcut)
  (keymap-set
   (keymap-lookup (current-global-map) "<menu-bar>")
   "<galactron-shortcut-menu>"
   (cons "快捷菜单" galactron-shortcut-menu))
  (add-to-list 'menu-bar-final-items
               'galactron-shortcut-menu 'append))
;; [[help:galactron-display-enabled-shortcut-menu][快捷菜单]]:1 ends here
(provide 'galactron-display)
;;; galactron-display.el ends here
