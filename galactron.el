;;; galactron.el --- 加拉特隆  -*- coding: utf-8; lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.0") (org "9.6"))
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:


;; 终止所有纷争，重塑整个世界。


;;; Code:
;; [[file:README.org::*停用\[\[elisp:(customize-group 'galactron)\]\[Galactron\]\]设置][停用[[elisp:(customize-group 'galactron)][Galactron]]设置:1]]
;;;###autoload(defgroup galactron nil "在启动时用Galactron配置Emacs" :tag "Galactron" :prefix "galactron-" :group 'emacs)
;; 停用[[elisp:(customize-group 'galactron)][Galactron]]设置:1 ends here
;; [[file:README.org::*停用\[\[elisp:(customize-group 'galactron)\]\[Galactron\]\]设置][停用[[elisp:(customize-group 'galactron)][Galactron]]设置:2]]
;;;###autoload(defgroup galactron-group nil "启用或停用一组Galactron设置" :prefix "galactron-group-" :group 'galactron)
;; 停用[[elisp:(customize-group 'galactron)][Galactron]]设置:2 ends here
;; [[file:README.org::*添加到\[\[help:galactron-init-files\]\[galactron-init-files\]\]][添加到[[help:galactron-init-files][galactron-init-files]]:1]]
(defcustom galactron-init-files nil
  "Galactron最先加载的配置文件(会比Galactron自带的配置文件先加载)"
  :type '(repeat (file :must-match t))
  :group 'galactron)
;; 添加到[[help:galactron-init-files][galactron-init-files]]:1 ends here
;; [[file:README.org::*添加到\[\[help:galactron-init-files\]\[galactron-init-files\]\]][添加到[[help:galactron-init-files][galactron-init-files]]:2]]
(defvar galactron--init-files nil)
(setq galactron--init-files (copy-sequence galactron-init-files))

(defmacro galactron-load-file (file-name)
  (let ((full-name
         (expand-file-name file-name
                           (file-name-directory
                            (or byte-compile-dest-file
                                load-file-name)))))
    `(unless (member ,full-name galactron--init-files)
       (if galactron--init-files
           (setcdr (last galactron--init-files) (list ,full-name))
         (setq galactron--init-files (list ,full-name))))))
;; 添加到[[help:galactron-init-files][galactron-init-files]]:2 ends here
;; [[file:README.org::*修改Emacs默认设置][修改Emacs默认设置:1]]
(defun galactron--variable-saved-p (symbol)
  (or (get symbol 'saved-value)
      (get symbol 'saved-variable-comment)))
;; 修改Emacs默认设置:1 ends here
;; [[file:README.org::*修改Emacs默认设置][修改Emacs默认设置:2]]
(defun galactron--default (symbol value)
  (unless (galactron--variable-saved-p symbol)
    (customize-set-variable symbol value)))

(defmacro galactron-default (symbol value)
  `(galactron--default ',symbol ,value))
;; 修改Emacs默认设置:2 ends here
;; [[file:README.org::*修改Emacs默认设置][修改Emacs默认设置:3]]
(defun galactron--default-member (symbol value)
  (unless (or (galactron--variable-saved-p symbol)
              (member value (symbol-value symbol)))
    (customize-set-variable symbol
                            (append (symbol-value symbol)
                                    (list value)))))

(defmacro galactron-default-member (symbol value)
  `(galactron--default-member ',symbol ,value))
;; 修改Emacs默认设置:3 ends here
;; [[file:README.org::*修改Emacs默认设置][修改Emacs默认设置:4]]
(defun galactron--default-assoc (symbol key value)
  (unless (or (galactron--variable-saved-p symbol)
              (equal (cons key value)
                     (assoc key (symbol-value symbol))))
     (customize-set-variable
      symbol
      (append (assoc-delete-all key
                                (copy-alist (symbol-value symbol)))
              (list (cons key value))))))

(defmacro galactron-default-assoc (symbol key value)
  `(galactron--default-assoc ',symbol ,key ,value))
;; 修改Emacs默认设置:4 ends here
;; [[file:README.org::*修改Emacs默认设置][修改Emacs默认设置:5]]
(defun galactron-set-default-value (symbol value)
  (unless (and (boundp symbol)
               (eq (symbol-value symbol) value))
    (set-default-toplevel-value symbol value)
    (funcall
     (intern (string-replace "-enabled-" "-set-enabled-"
                             (symbol-name symbol)))
     value)))
;; 修改Emacs默认设置:5 ends here
;; [[file:README.org::*延迟加载][延迟加载:1]]
(defmacro galactron--with-messages (&rest body)
  (declare (indent 0) (debug (form body)))
  `(with-current-buffer (get-buffer "*Messages*")
     (save-excursion
       (goto-char (point-max))
       (let ((inhibit-read-only t))
         (unless (zerop (current-column)) (insert "\n"))
         ,@body))))

(defun galactron--package-install (package)
  (interactive)
  (when (yes-or-no-p
         (format "Do you want to install '%s' now?" package))
    (package-install package)))

(defun galactron--find-location (location)
  (when (file-exists-p (car location))
    (find-file (car location))
    (goto-char (cdr location))))

(defmacro galactron-when-package-installed (package &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((autoloads (intern (format "%s-autoloads" package))))
    `(with-eval-after-load ',autoloads
       ,@body)))

(defmacro galactron-with-package-installed (package &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((file (or byte-compile-current-file load-file-name))
        (pos (with-current-buffer
                 (or byte-compile-current-buffer
                     (car eval-buffer-list))
             (save-excursion
               (backward-list)
               (point)))))
    `(progn
       (unless (package-installed-p ',package)
         (galactron--with-messages
           (insert
            (concat
             "由于未"
             (buttonize (format "安装%S" ',package)
                        'galactron--package-install ',package)
             "，部分配置未生效，"
             (buttonize "查看详情" 'galactron--find-location
                        (cons ,file ,pos))
             "\n"))))
       (galactron-when-package-installed ,package ,@body))))
;; 延迟加载:1 ends here
;; [[file:README.org::*设置 ~after-init-hook~][设置 ~after-init-hook~:1]]
;;;###autoload(add-hook 'after-init-hook 'galactron--first-h -100)
;;;###autoload(add-hook 'after-init-hook 'galactron--last-h 100)
;; 设置 ~after-init-hook~:1 ends here
;; [[file:README.org::*设置 ~after-init-hook~][设置 ~after-init-hook~:2]]
(defun galactron--check-after-init ()
  (cond
   ((not after-init-time)
    (lwarn 'galactron :error "called before `after-init-hook'")
    nil)
   ((and
     (eq (car after-init-hook) 'galactron--first-h)
     (eq (car (last after-init-hook)) 'galactron--last-h))
    t)
   (t
    (lwarn 'galactron :error "bad `after-init-hook'")
    nil)))
;; 设置 ~after-init-hook~:2 ends here
;; [[file:README.org::*设置 ~after-init-hook~][设置 ~after-init-hook~:3]]
(defvar galactron--before-init-time nil)
(defvar galactron--first-h-finished nil)

;;;###autoload
(defun galactron--first-h ()
  (if galactron--before-init-time
      (lwarn 'galactron
             :warning "`galactron--first-h' called twice")
    (setq galactron--before-init-time (current-time))
    (when (galactron--check-after-init)
      (let ((result :error))
        (unwind-protect
            (progn
              (galactron--load-init-files)
              (galactron--restore-bars)
              (setq result t))
          (setq galactron--first-h-finished result))))))
;; 设置 ~after-init-hook~:3 ends here
;; [[file:README.org::*设置 ~after-init-hook~][设置 ~after-init-hook~:4]]
(defvar galactron--last-h-started nil)
(defvar galactron--after-init-time nil)
(defvar galactron--load-time nil)

;;;###autoload
(defun galactron--last-h ()
  (if galactron--last-h-started
      (lwarn 'galactron :warning "`galactron--last-h' called twice")
    (setq galactron--last-h-started t)
    (cond
     ((not galactron--before-init-time)
      (lwarn
       'galactron
       :error
       "`galactron--last-h' called before `galactron--first-h'"))
     ((not galactron--first-h-finished)
      (lwarn
       'galactron
       :error "`galactron--last-h' called in `galactron--first-h'"))
     ((not (eq galactron--first-h-finished t))
      (lwarn 'galactron :error "`galactron--first-h' failed"))
     ((galactron--check-after-init)
      (setq galactron--after-init-time (current-time))
      (push (cons
             (float-time (time-subtract after-init-time
                                        before-init-time))
             "emacs-init-time")
            galactron--load-time)
      (galactron--record-time "after-init-hook"
                              galactron--before-init-time)
      (galactron--record-time "" before-init-time)
      (message "time   config-file-name")
      (let ((pkg-dir
             (package-desc-dir (cadr (assq 'galactron
                                           package-alist)))))
        (pcase-dolist (`(,time . ,file-name)
                       (seq-sort-by 'car '> galactron--load-time))
          (message
           "%.04f %s" time
           (cond
            ((galactron--org-file-p file-name)
             (file-relative-name file-name org-directory))
            ((file-name-absolute-p file-name)
             (file-relative-name file-name pkg-dir))
            (t
             file-name)))))))))
;; 设置 ~after-init-hook~:4 ends here
;; [[file:README.org::*设置 ~window-setup-hook~][设置 ~window-setup-hook~:1]]
;;;###autoload(add-hook 'window-setup-hook 'galactron--start-time-h 100)

(defun galactron--start-time-h ()
  (message "emacs took more than %s seconds to start"
           (float-time (time-since before-init-time))))
;; 设置 ~window-setup-hook~:1 ends here
;; [[file:README.org::*跳过Menu/Tab/Tool Bar][跳过Menu/Tab/Tool Bar:1]]
;;;###autoload(setq emacs-basic-display t)
;; 跳过Menu/Tab/Tool Bar:1 ends here
;; [[file:README.org::*加载配置文件][加载配置文件:1]]
(defun galactron--load-init-files ()
  (dolist (file-name galactron--init-files)
    (let ((start-time (current-time)))
      (galactron--load-file file-name)
      (galactron--record-time file-name start-time)))
  (galactron--record-time "galactron" galactron--before-init-time))
;; 加载配置文件:1 ends here
;; [[file:README.org::*加载配置文件][加载配置文件:2]]
(defun galactron--record-time (file-name time)
  (push (cons (float-time (time-since time)) file-name)
        galactron--load-time))
;; 加载配置文件:2 ends here
;; [[file:README.org::*加载配置文件][加载配置文件:3]]
(defun galactron--org-file-p (file-name)
  (provided-mode-derived-p
   (assoc-default file-name auto-mode-alist 'string-match-p)
   'org-mode))

(defun galactron--load-file (file-name)
  (if (galactron--org-file-p file-name)
      (let ((tangled (file-name-with-extension file-name ".el")))
        (if (file-newer-than-file-p file-name tangled)
            (org-babel-load-file file-name)
          (load-file tangled)))
    (load file-name)))
;; 加载配置文件:3 ends here
;; [[file:README.org::*加载内置配置文件][加载内置配置文件:1]]
(when galactron-group-enabled-server
  (galactron-load-file "galactron-server"))

(when galactron-group-enabled-package
  (galactron-load-file "galactron-package"))

(when galactron-group-enabled-display
  (galactron-load-file "galactron-display"))

(when galactron-group-enabled-edit
  (galactron-load-file "galactron-edit"))

(when galactron-group-enabled-frame
  (galactron-load-file "galactron-frame"))

(when galactron-group-enabled-org
  (galactron-load-file "galactron-org"))

(when galactron-group-enabled-pdf
  (galactron-with-package-installed pdf-tools
    (galactron-load-file "galactron-pdf")))

(when galactron-group-enabled-nov
  (galactron-with-package-installed nov
    (galactron-load-file "galactron-nov")))

(when galactron-group-enabled-ssp
  (galactron-with-package-installed ssp
    (galactron-load-file "galactron-ssp")))

(when galactron-group-enabled-largefile
  (galactron-with-package-installed largefile
    (galactron-load-file "galactron-largefile")))

(when galactron-group-enabled-wanderlust
  (galactron-with-package-installed wanderlust
    (galactron-load-file "galactron-wanderlust")))

(when galactron-group-enabled-flatpak
  (galactron-load-file "galactron-flatpak"))

(when galactron-group-enabled-capture
  (galactron-with-package-installed orb
    (galactron-load-file "galactron-capture")))

(when galactron-group-enabled-agenda
  (galactron-load-file "galactron-agenda"))

(when galactron-group-enabled-orb
  (galactron-with-package-installed orb
    (galactron-load-file "galactron-orb")))

(when galactron-group-enabled-accessories
  (galactron-load-file "galactron-accessories"))
;; 加载内置配置文件:1 ends here
;; [[file:README.org::*恢复Menu/Tab/Tool Bar][恢复Menu/Tab/Tool Bar:1]]
(defun galactron--restore-bars ()
  (when (galactron--variable-reset-value menu-bar-mode)
    (push (cons 'menu-bar-lines 1) default-frame-alist))
  (when (galactron--variable-reset-value tab-bar-mode)
    (push (cons 'tab-bar-lines 1) default-frame-alist))
  (when (galactron--variable-reset-value tool-bar-mode)
    (push (cons 'tool-bar-lines 1) default-frame-alist)))
;; 恢复Menu/Tab/Tool Bar:1 ends here
;; [[file:README.org::*恢复Menu/Tab/Tool Bar][恢复Menu/Tab/Tool Bar:2]]
(defun galactron--variable-get-think-value (symbol)
  (cond
   ((get symbol 'customized-value)
    (eval (car (get symbol 'customized-value))))
   ((get symbol 'customized-variable-comment)
    nil)
   ((get symbol 'saved-value)
    (eval (car (get symbol 'saved-value))))
   ((get symbol 'saved-variabled-comment)
    nil)
   (t
    (eval (car (get symbol 'standard-value))))))

(defmacro galactron--variable-reset-value (symbol)
  (let ((think-var (gensym)))
    `(let ((,think-var (galactron--variable-get-think-value ',symbol)))
       (unless (equal ,think-var ,symbol)
         (setq ,symbol ,think-var)))))
;; 恢复Menu/Tab/Tool Bar:2 ends here
(provide 'galactron)
;;; galactron.el ends here
