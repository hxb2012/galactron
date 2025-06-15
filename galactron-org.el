;;; galactron-org.el --- 修改Org Mode设置  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-org t "启用修改Org Mode设置" :type 'boolean :tag "Org")

(defgroup galactron-org nil
  "修改Org Mode设置"
  :prefix "galactron-org-"
  :tag "Org"
  :group 'galactron)
;; [[file:README.org::*\[\[elisp:(customize-group 'galactron-org)\]\[Org-Mode\]\]][[[elisp:(customize-group 'galactron-org)][Org-Mode]]:2]]
(defvar org-capture-entry nil)
;; [[elisp:(customize-group 'galactron-org)][Org-Mode]]:2 ends here
;; [[file:README.org::*\[\[help:galactron-org-enabled-lazy-load-modules\]\[延迟加载模块\]\]][[[help:galactron-org-enabled-lazy-load-modules][延迟加载模块]]:1]]
(defcustom galactron-org-enabled-lazy-load-modules t
  "延迟加载模块"
  :type 'boolean
  :tag "Lazy load modules")

(when galactron-org-enabled-lazy-load-modules
  (setq org-modules-loaded t)
  (with-eval-after-load 'org
    (dolist (ext org-modules)
      (pcase ext
        ('ol-doi
         (autoload 'org-link-doi-open "ol-doi")
         (autoload 'org-link-doi-export "ol-doi")
         (org-link-set-parameters
          "doi"
          :follow #'org-link-doi-open
          :export #'org-link-doi-export))
        ('ol-w3m
         (autoload 'org-w3m-store-link "ol-w3m")
         (org-link-set-parameters "w3m" :store #'org-w3m-store-link))
        ('ol-bbdb
         (autoload 'org-bbdb-open "ol-bbdb")
         (autoload 'org-bbdb-export "ol-bbdb")
         (autoload 'org-bbdb-complete-link "ol-bbdb")
         (autoload 'org-bbdb-store-link "ol-bbdb")
         (org-link-set-parameters
          "bbdb"
          :follow #'org-bbdb-open
          :export #'org-bbdb-export
          :complete #'org-bbdb-complete-link
          :store #'org-bbdb-store-link))
        ('ol-bibtex
         (autoload 'org-bibtex-open "ol-bibtex")
         (autoload 'org-bibtex-store-link "ol-bibtex")
         (org-link-set-parameters
          "bibtex"
          :follow #'org-bibtex-open
          :store #'org-bibtex-store-link))
        ('ol-docview
         (autoload 'org-docview-open "ol-docview")
         (autoload 'org-docview-export "ol-docview")
         (autoload 'org-docview-store-link "ol-docview")
         (org-link-set-parameters
          "docview"
          :follow #'org-docview-open
          :export #'org-docview-export
          :store #'org-docview-store-link))
        ('ol-gnus
         (autoload 'org-gnus-open "ol-gnus")
         (autoload 'org-gnus-store-link "ol-gnus")
         (org-link-set-parameters
          "gnus"
          :follow #'org-gnus-open
          :store #'org-gnus-store-link))
        ('ol-info
         (autoload 'org-info-open "ol-info")
         (autoload 'org-info-export "ol-info")
         (autoload 'org-info-store-link "ol-info")
         (autoload 'org-info-description-as-command "ol-info")
         (org-link-set-parameters
          "info"
          :follow #'org-info-open
          :export #'org-info-export
          :store #'org-info-store-link
          :insert-description #'org-info-description-as-command))
        ('ol-irc
         (autoload 'org-irc-visit "ol-irc")
         (autoload 'org-irc-store-link "ol-irc")
         (autoload 'org-irc-export "ol-irc")
         (org-link-set-parameters
          "irc"
          :follow #'org-irc-visit
          :store #'org-irc-store-link
          :export #'org-irc-export))
        ('ol-mhe
         (autoload 'org-mhe-open "ol-mhe")
         (autoload 'org-mhe-store-link "ol-mhe")
         (org-link-set-parameters
          "mhe"
          :follow #'org-mhe-open
          :store #'org-mhe-store-link))
        ('ol-rmail
         (autoload 'org-rmail-open "ol-rmail")
         (autoload 'org-rmail-store-link "ol-rmail")
         (org-link-set-parameters
          "rmail"
          :follow #'org-rmail-open
          :store #'org-rmail-store-link))
        ('ol-eww
         (autoload 'org-eww-open "ol-eww")
         (autoload 'org-eww-store-link "ol-eww")
         (org-link-set-parameters
          "eww"
          :follow #'org-eww-open
          :store #'org-eww-store-link))))))
;; [[help:galactron-org-enabled-lazy-load-modules][延迟加载模块]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-org-enabled-autoload-org-babel\]\[设置Org babel函数自动加载\]\]][[[help:galactron-org-enabled-autoload-org-babel][设置Org babel函数自动加载]]:1]]
(defcustom galactron-org-enabled-autoload-org-babel t
  "设置Org babel函数自动加载"
  :type 'boolean
  :tag "Autoload org babel")

(when galactron-org-enabled-autoload-org-babel
  (pcase-dolist
      (`(,lang . ,module)
       '(
         ("C" . "C")
         ("C++" . "C")
         ("D" . "C")
         ("R" . "R")
         ("ash" . "shell")
         ("awk" . "awk")
         ("bash" . "shell")
         ("calc" . "calc")
         ("clojure" . "clojure")
         ("clojurescript" . "clojure")
         ("cpp" . "C")
         ("csh" . "shell")
         ("css" . "css")
         ("dash" . "shell")
         ("ditaa" . "ditaa")
         ("dot" . "dot")
         ("elisp" . "emacs-lisp")
         ("emacs-lisp" . "emacs-lisp")
         ("eshell" . "eshell")
         ("fish" . "shell")
         ("forth" . "forth")
         ("fortran" . "fortran")
         ("gnuplot" . "gnuplot")
         ("groovy" . "groovy")
         ("haskell" . "haskell")
         ("java" . "java")
         ("js" . "js")
         ("julia" . "julia")
         ("ksh" . "shell")
         ("latex" . "latex")
         ("lilypond" . "lilypond")
         ("lisp" . "lisp")
         ("lua" . "lua")
         ("makefile" . "makefile")
         ("matlab" . "matlab")
         ("maxima" . "maxima")
         ("mksh" . "mksh")
         ("ocaml" . "ocaml")
         ("octave" . "octave")
         ("org" . "org")
         ("perl" . "perl")
         ("plantuml" . "plantuml")
         ("posh" . "shell")
         ("processing" . "processing")
         ("python" . "python")
         ("ruby" . "ruby")
         ("sass" . "sass")
         ("scheme" . "scheme")
         ("screen" . "screen")
         ("sed" . "sed")
         ("sh" . "shell")
         ("shell" . "shell")
         ("sql" . "sql")
         ("sqlite" . "sqlite")
         ("zsh" . "shell")))
    (autoload
      (intern (format "org-babel-execute:%s" lang))
      (format "ob-%s" module))))
;; [[help:galactron-org-enabled-autoload-org-babel][设置Org babel函数自动加载]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-org-enabled-refile-outline-path\]\[refile路径提示\]\]][[[help:galactron-org-enabled-refile-outline-path][refile路径提示]]:1]]
(defcustom galactron-org-enabled-refile-outline-path t
  "refile路径提示"
  :type 'boolean
  :tag "Refile outline path")

(when galactron-org-enabled-refile-outline-path
  (galactron-default org-refile-use-outline-path 'buffer-name)
  (galactron-default org-outline-path-complete-in-steps nil))
;; [[help:galactron-org-enabled-refile-outline-path][refile路径提示]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-org-enabled-no-src-indentation\]\[禁用代码块缩进\]\]][[[help:galactron-org-enabled-no-src-indentation][禁用代码块缩进]]:1]]
(defcustom galactron-org-enabled-no-src-indentation t
  "禁用代码块缩进"
  :type 'boolean
  :tag "No source indent")

(when galactron-org-enabled-no-src-indentation
  (galactron-default org-src-preserve-indentation t)
  (galactron-default org-edit-src-content-indentation 0))
;; [[help:galactron-org-enabled-no-src-indentation][禁用代码块缩进]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-org-enabled-visual-line\]\[自动折行\]\]][[[help:galactron-org-enabled-visual-line][自动折行]]:1]]
(defcustom galactron-org-enabled-visual-line t
  "自动折行"
  :type 'boolean
  :tag "Visual line")

(when galactron-org-enabled-visual-line
  (add-hook 'org-mode-hook 'visual-line-mode))
;; [[help:galactron-org-enabled-visual-line][自动折行]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-org-enabled-log-done-time\]\[记录任务结束时间\]\]][[[help:galactron-org-enabled-log-done-time][记录任务结束时间]]:1]]
(defcustom galactron-org-enabled-log-done-time t
  "记录任务结束时间"
  :type 'boolean
  :tag "Log done time")

(when galactron-org-enabled-log-done-time
  (galactron-default org-log-done 'time))
;; [[help:galactron-org-enabled-log-done-time][记录任务结束时间]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-org-enabled-todo-dependencies\]\[任务依赖\]\]][[[help:galactron-org-enabled-todo-dependencies][任务依赖]]:1]]
(defcustom galactron-org-enabled-todo-dependencies t
  "记录任务结束时间"
  :type 'boolean
  :tag "Todo dependencies")

(when galactron-org-enabled-todo-dependencies
  (galactron-default org-enforce-todo-dependencies t))
;; [[help:galactron-org-enabled-todo-dependencies][任务依赖]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-org-enabled-clock-persistence\]\[关闭Emacs时，保持clock in\]\]][[[help:galactron-org-enabled-clock-persistence][关闭Emacs时，保持clock in]]:1]]
(defcustom galactron-org-enabled-clock-persistence t
  "关闭Emacs时，保持clock in"
  :type 'boolean
  :tag "Clock persistence")

(defun galactron-org--check-running-clock-h ()
  (not (equal (marker-buffer org-clock-marker) (current-buffer))))

(defun galactron-org--stop-killing-clocked-in-h ()
  (add-hook 'kill-buffer-query-functions
            'galactron-org--check-running-clock-h nil t))

(when galactron-org-enabled-clock-persistence
  (galactron-default org-clock-persist t)
  (galactron-default org-clock-in-resume t)
  (galactron-default org-clock-persist-query-resume nil)
  (galactron-default org-clock-ask-before-exiting nil)
  (add-hook 'org-mode-hook 'galactron-org--stop-killing-clocked-in-h)
  (with-eval-after-load 'org
    (org-clock-persistence-insinuate)))
;; [[help:galactron-org-enabled-clock-persistence][关闭Emacs时，保持clock in]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-org-enabled-publish-timestamp-directory\]\[Org publish时间戳目录位置\]\]][[[help:galactron-org-enabled-publish-timestamp-directory][Org publish时间戳目录位置]]:1]]
(defcustom galactron-org-enabled-publish-timestamp-directory t
  "Org publish时间戳目录位置"
  :type 'boolean
  :tag "Publish timestamp directory")

(when galactron-org-enabled-publish-timestamp-directory
  (galactron-default
   org-publish-timestamp-directory
   (file-name-as-directory
    (expand-file-name "org-timestamps" user-emacs-directory))))
;; [[help:galactron-org-enabled-publish-timestamp-directory][Org publish时间戳目录位置]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-org-enabled-publish-code-highlight\]\[Org publish代码高亮\]\] (请先\[\[elisp:(package-install 'htmlize)\]\[安装htmlize\]\])][[[help:galactron-org-enabled-publish-code-highlight][Org publish代码高亮]] (请先[[elisp:(package-install 'htmlize)][安装htmlize]]):1]]
(defcustom galactron-org-enabled-publish-code-highlight t
  "Org publish代码高亮"
  :type 'boolean
  :tag "Publish code highlight")

(when galactron-org-enabled-publish-code-highlight
  (galactron-with-package-installed htmlize))
;; [[help:galactron-org-enabled-publish-code-highlight][Org publish代码高亮]] (请先[[elisp:(package-install 'htmlize)][安装htmlize]]):1 ends here
;; [[file:README.org::*\[\[help:galactron-org-enabled-remember\]\[设置为Remember后端\]\]][[[help:galactron-org-enabled-remember][设置为Remember后端]]:1]]
(defcustom galactron-org-enabled-remember t
  "设置为Remember后端"
  :type 'boolean
  :tag "Remember")

(defun galactron-org--remember-handler ()
  (mark-whole-buffer)
  (let ((org-capture-entry
         (list
          "r"
          "remember"
          'entry
          (list 'file remember-data-file)
          "* %i"
          :immediate-finish t
          :jump-to-captured nil)))
    (org-capture)))

(defun galactron-org--remember-h ()
  (visual-fill-column-mode -1))

(when galactron-org-enabled-remember
  (galactron-default remember-data-file
                  (expand-file-name "remember.org" org-directory))
  (galactron-default remember-notes-initial-major-mode 'org-mode)
  (galactron-default remember-notes-buffer-name "*remember*")
  (galactron-default remember-handler-functions
                  '(galactron-org--remember-handler))
  (galactron-default remember-annotation-functions '())
  (when galactron-display-enabled-visual-fill-column
    (add-hook 'remember-notes-mode-hook 'galactron-org--remember-h))
  (keymap-global-set "C-x M-r" 'remember))
;; [[help:galactron-org-enabled-remember][设置为Remember后端]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-org-enabled-sidebar\]\[侧栏\]\] (请先\[\[elisp:(package-install 'orb)\]\[安装orb\]\])][[[help:galactron-org-enabled-sidebar][侧栏]] (请先[[elisp:(package-install 'orb)][安装orb]]):1]]
(defcustom galactron-org-enabled-sidebar t
  "侧栏"
  :type 'boolean
  :tag "Sidebar")

(when galactron-org-enabled-sidebar
  (galactron-with-package-installed orb
    (with-eval-after-load 'org
      (keymap-set org-mode-map "C-c r" 'orb-sidebar))))
;; [[help:galactron-org-enabled-sidebar][侧栏]] (请先[[elisp:(package-install 'orb)][安装orb]]):1 ends here
(provide 'galactron-org)
;;; galactron-org.el ends here
