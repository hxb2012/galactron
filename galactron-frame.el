;;; galactron-frame.el --- 修改Frame设置  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-frame t "启用修改Frame设置" :type 'boolean :tag "Frame")

(defgroup galactron-frame nil
  "修改Frame设置"
  :prefix "galactron-frame-"
  :tag "Frame"
  :group 'galactron)
;; [[file:README.org::*\[\[help:galactron-frame-enabled-no-menu-bar\]\[禁用菜单栏\]\]][[[help:galactron-frame-enabled-no-menu-bar][禁用菜单栏]]:1]]
(defcustom galactron-frame-enabled-no-menu-bar nil
  "禁用菜单栏"
  :type 'boolean
  :tag "No menu bar")

(when galactron-frame-enabled-no-menu-bar
  (unless (galactron--variable-saved-p 'menu-bar-mode)
    (put 'menu-bar-mode 'customized-value '(nil))))
;; [[help:galactron-frame-enabled-no-menu-bar][禁用菜单栏]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-frame-enabled-no-tool-bar\]\[禁用工具栏\]\]][[[help:galactron-frame-enabled-no-tool-bar][禁用工具栏]]:1]]
(defcustom galactron-frame-enabled-no-tool-bar t
  "禁用工具栏"
  :type 'boolean
  :tag "No tool bar")

(when galactron-frame-enabled-no-tool-bar
  (unless (galactron--variable-saved-p 'tool-bar-mode)
    (put 'tool-bar-mode 'customized-value '(nil))))
;; [[help:galactron-frame-enabled-no-tool-bar][禁用工具栏]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-frame-enabled-borderless\]\[禁用边框\]\]][[[help:galactron-frame-enabled-borderless][禁用边框]]:1]]
(defcustom galactron-frame-enabled-borderless t
  "禁用边框"
  :type 'boolean
  :tag "Borderless")

(when (and galactron-frame-enabled-borderless (display-graphic-p))
  (galactron-default-assoc default-frame-alist 'undecorated t))
;; [[help:galactron-frame-enabled-borderless][禁用边框]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-frame-enabled-maximized\]\[最大化\]\]][[[help:galactron-frame-enabled-maximized][最大化]]:1]]
(defcustom galactron-frame-enabled-maximized t
  "最大化"
  :type 'boolean
  :tag "Maximized")

(when (and galactron-frame-enabled-maximized (display-graphic-p))
  (galactron-default-assoc default-frame-alist
                           'fullscreen 'maximized))
;; [[help:galactron-frame-enabled-maximized][最大化]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-frame-enabled-window-auto-balance\]\[自动等分Window\]\]][[[help:galactron-frame-enabled-window-auto-balance][自动等分Window]]:1]]
(defcustom galactron-frame-enabled-window-auto-balance t
  "最大化"
  :type 'boolean
  :tag "Window auto balance")

(when galactron-frame-enabled-window-auto-balance
  (galactron-default window-combination-resize t))
;; [[help:galactron-frame-enabled-window-auto-balance][自动等分Window]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-frame-enabled-minibuffer-at-top\]\[把minibuffer放到顶部\]\]][[[help:galactron-frame-enabled-minibuffer-at-top][把minibuffer放到顶部]]:1]]
(defcustom galactron-frame-enabled-minibuffer-at-top nil
  "把minibuffer放到顶部"
  :type 'boolean
  :tag "Minibuffer at top")
;; [[help:galactron-frame-enabled-minibuffer-at-top][把minibuffer放到顶部]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-frame-enabled-minibuffer-at-top\]\[把minibuffer放到顶部\]\]][[[help:galactron-frame-enabled-minibuffer-at-top][把minibuffer放到顶部]]:2]]
(defun galactron-frame--delay-read-from-minibuffer-a (&rest _rest)
  (redisplay t)
  (read-char "" nil 0.02))
;; [[help:galactron-frame-enabled-minibuffer-at-top][把minibuffer放到顶部]]:2 ends here
;; [[file:README.org::*\[\[help:galactron-frame-enabled-minibuffer-at-top\]\[把minibuffer放到顶部\]\]][[[help:galactron-frame-enabled-minibuffer-at-top][把minibuffer放到顶部]]:3]]
(defun galactron-frame--resize-minibuffer-a (fun frame alist)
  (let ((minibuffer (window-frame (minibuffer-window frame))))
    (if (eq minibuffer frame)
        (funcall fun frame alist)
      (let* ((old-width (car (assoc-default 'outer-size
                                            (frame-geometry frame))))
             (result
              (prog1
                  (funcall fun frame alist)
                (redisplay t)))
             (height (tab-bar-height frame t))
             (new-width (car (assoc-default 'outer-size
                                            (frame-geometry frame)))))
        (unless (equal old-width new-width)
          (set-frame-size minibuffer new-width height t))
        result))))

(defun galactron-frame--minibuffer-at-top-h ()
  (advice-add 'modify-frame-parameters
              :around 'galactron-frame--resize-minibuffer-a))

(defun galactron-frame--create-frame-a (fun params)
  (let ((frame (funcall fun params)))
    (redisplay t)
    (if (eq 'only (frame-parameter frame 'minibuffer))
        (let* ((parent (frame-parent frame))
               (width (car (assoc-default 'outer-size
                                          (frame-geometry parent))))
               (height (tab-bar-height parent t)))
          (set-frame-size frame width height t))
      (unless (memq (frame-parameter frame 'fullscreen)
                    '(maximized fullwidth))
        (when-let ((width (assoc-default 'width params)))
          (unless (equal width (frame-parameter frame 'width))
            (set-frame-parameter frame 'width width))))
      (unless (memq (frame-parameter frame 'fullscreen)
                    '(maximized fullheight))
        (when-let ((height (assoc-default 'height params)))
          (unless (equal height (frame-parameter frame 'height))
            (set-frame-parameter frame 'height height)))))
      frame))
;; [[help:galactron-frame-enabled-minibuffer-at-top][把minibuffer放到顶部]]:3 ends here
;; [[file:README.org::*\[\[help:galactron-frame-enabled-minibuffer-at-top\]\[把minibuffer放到顶部\]\]][[[help:galactron-frame-enabled-minibuffer-at-top][把minibuffer放到顶部]]:4]]
(when (and galactron-frame-enabled-minibuffer-at-top
           (display-graphic-p))
  (galactron-default frame-resize-pixelwise t)
  (galactron-default header-line-format mode-line-format)
  (galactron-default mode-line-format nil)
  (galactron-default-assoc minibuffer-frame-alist 'height 1)
  (galactron-default-assoc default-frame-alist
                        'minibuffer 'child-frame)
  (galactron-default-assoc minibuffer-frame-alist 'top 0)
  (galactron-default-assoc minibuffer-frame-alist 'left 0)
  (put 'tab-bar-mode 'customized-value '(t))
  (add-hook 'window-setup-hook 'galactron-frame--minibuffer-at-top-h)
  (advice-add 'frame-creation-function
              :around 'galactron-frame--create-frame-a)
  (advice-add 'read-from-minibuffer
              :before 'galactron-frame--delay-read-from-minibuffer-a))
;; [[help:galactron-frame-enabled-minibuffer-at-top][把minibuffer放到顶部]]:4 ends here
;; [[file:README.org::*\[\[help:galactron-frame-enabled-transparent\]\[透明背景\]\]][[[help:galactron-frame-enabled-transparent][透明背景]]:1]]
(defcustom galactron-frame-enabled-transparent nil
  "透明背景

数字的含义是 100 - alpha，改成100就完全看不见了"
  :type 'natnum
  :tag "Transparent Background")

(when (and galactron-frame-enabled-transparent (display-graphic-p))
  (galactron-default-assoc
   default-frame-alist
   'alpha-background (- 100 galactron-frame-enabled-transparent)))
;; [[help:galactron-frame-enabled-transparent][透明背景]]:1 ends here
(provide 'galactron-frame)
;;; galactron-frame.el ends here
