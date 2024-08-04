;;; galactron-flatpak.el --- 修改Flatpak设置  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-flatpak t "启用修改Flatpak设置" :type 'boolean :tag "Flatpak")

(defgroup galactron-flatpak nil
  "修改Flatpak设置"
  :prefix "galactron-flatpak-"
  :tag "Flatpak"
  :group 'galactron)
;; [[file:README.org::*\[\[elisp:(customize-group 'galactron-flatpak)\]\[Flatpak\]\]][[[elisp:(customize-group 'galactron-flatpak)][Flatpak]]:2]]
(defun galactron-flatpak--executable-find (command)
  (zerop
   (call-process "flatpak-spawn" nil nil nil
                 "--host" "--watch-bus" "--" "which" command)))

(defvar galactron-flatpak--call-process-override-p nil)
(defvar galactron-flatpak--start-process-override-p nil)

(defun galactron-flatpak--call-process-a (fun &rest args)
  (if galactron-flatpak--call-process-override-p
      (apply fun args)
    (cl-letf* ((galactron-flatpak--call-process-override-p t)
               (call-process (symbol-function 'call-process))
               ((symbol-function 'call-process)
                (lambda (program &optional infile destination display &rest args)
                  (apply call-process
                         "flatpak-spawn"
                         infile destination display
                         "--host" "--watch-bus" "--"
                         program args))))
      (apply fun args))))

(defun galactron-flatpak--start-process-a (fun &rest args)
  (if galactron-flatpak--start-process-override-p
      (apply fun args)
    (cl-letf* ((galactron-flatpak--start-process-override-p t)
               (start-process (symbol-function 'start-process))
               ((symbol-function 'start-process)
                (lambda (name buffer program &rest program-args)
                  (apply start-process name buffer
                         "flatpak-spawn" "--host" "--watch-bus" "--"
                         program program-args))))
      (apply fun args))))

(defun galactron-flatpak--call-process-toolbox-a (fun &rest args)
  (if galactron-flatpak--call-process-override-p
      (apply fun args)
    (cl-letf* ((galactron-flatpak--call-process-override-p t)
               (call-process (symbol-function 'call-process))
               ((symbol-function 'call-process)
                (lambda (program &optional infile destination display &rest args)
                  (apply call-process
                         "flatpak-spawn"
                         infile destination display
                         "--host" "--watch-bus" "--"
                         "toolbox" "run" "--"
                         program args))))
      (apply fun args))))
;; [[elisp:(customize-group 'galactron-flatpak)][Flatpak]]:2 ends here
;; [[file:README.org::*\[\[help:galactron-flatpak-enabled-notifications\]\[桌面通知\]\]][[[help:galactron-flatpak-enabled-notifications][桌面通知]]:1]]
(defcustom galactron-flatpak-enabled-notifications t
  "Flatpak Notifications"
  :type 'boolean
  :tag "Flatpak Notifications")

(when (and galactron-flatpak-enabled-notifications
           (equal (getenv "container") "flatpak"))
  (with-eval-after-load 'notifications
    (defconst notifications-application-icon
      (expand-file-name
       "../../icons/hicolor/scalable/apps/org.gnu.emacs.svg"
       (getenv "GIO_LAUNCHED_DESKTOP_FILE")))
    (defconst notifications-application-name (getenv "FLATPAK_ID"))))
;; [[help:galactron-flatpak-enabled-notifications][桌面通知]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-flatpak-enabled-tramp\]\[Flatpak Tramp\]\] (请先\[\[elisp:(package-install 'flakpak-tramp)\]\[安装flatpak-tramp\]\])][[[help:galactron-flatpak-enabled-tramp][Flatpak Tramp]] (请先[[elisp:(package-install 'flakpak-tramp)][安装flatpak-tramp]]):1]]
(defcustom galactron-flatpak-enabled-tramp t
  "Flatpak Tramp"
  :type 'boolean
  :tag "Flatpak Tramp")

(when galactron-flatpak-enabled-tramp
  (galactron-with-package-installed flatpak-tramp
    (add-hook 'window-setup-hook 'switch-mode)
    (keymap-global-set "C-x M-s" 'switch-mode-switch)))
;; [[help:galactron-flatpak-enabled-tramp][Flatpak Tramp]] (请先[[elisp:(package-install 'flakpak-tramp)][安装flatpak-tramp]]):1 ends here
;; [[file:README.org::*\[\[help:galactron-flatpak-enabled-docview\]\[DocView调用Flatpak命令\]\]][[[help:galactron-flatpak-enabled-docview][DocView调用Flatpak命令]]:1]]
(defcustom galactron-flatpak-enabled-docview t
  "DocView调用Flatpak命令"
  :type 'boolean
  :tag "DocView")

(defun galactron-flatpak--docview-spawn-host-a (args)
  (list
   (car args)
   "flatpak-spawn"
   (append
    (list "--host" "--watch-bus" "--" (cadr args))
    (caddr args))
   (cadddr args)))

(defun galactron-flatpak--docview-executable-find-a (fun &rest args)
  (cl-letf (((symbol-function 'executable-find) 'always))
    (apply fun args)))

(defvar doc-view-pdfdraw-program)
(defvar doc-view-pdf->png-converter-ghostscript)
(defvar doc-view-pdf->png-converter-mupdf)

(when (and galactron-flatpak-enabled-docview
           (equal (getenv "container") "flatpak"))
  (advice-add 'doc-view-start-process
              :filter-args 'galactron-flatpak--docview-spawn-host-a)

  (dolist (fun
           '(doc-view-pdf-password-protected-ghostscript-p
             doc-view-pdf-password-protected-pdfdraw-p))
    (advice-add fun :around 'galactron-flatpak--call-process-a))

  (dolist (fun
           '(doc-view-mode-p
             doc-view-dvi->pdf
             doc-view-pdf->txt
             doc-view-ps->pdf
             doc-view-imenu-setup
             doc-view-initiate-display))
    (advice-add fun
                :around 'galactron-flatpak--docview-executable-find-a))

  (with-eval-after-load 'doc-view
    (galactron-default
     doc-view-ghostscript-program
     (cond
      ((galactron-flatpak--executable-find "gs") "gs")
      (t nil)))

    (galactron-default
     doc-view-pdfdraw-program
     (cond
      ((galactron-flatpak--executable-find "pdfdraw") "pdfdraw")
      ((galactron-flatpak--executable-find "mudraw") "mudraw")
      ((galactron-flatpak--executable-find "mutool") "mutool")
      (t nil)))

    (cl-letf (((symbol-function 'executable-find) 'identity))
      (galactron-default
       doc-view-pdf->png-converter-function
       (if doc-view-pdfdraw-program
           #'doc-view-pdf->png-converter-mupdf
         #'doc-view-pdf->png-converter-ghostscript)))

    (galactron-default
     doc-view-imenu-enabled
     (galactron-flatpak--executable-find "mutool"))

    (galactron-default
     doc-view-dvipdfm-program
     (cond
      ((galactron-flatpak--executable-find "dvipdfm") "dvipdfm")
      (t nil)))

    (galactron-default
     doc-view-dvipdf-program
     (cond
      ((galactron-flatpak--executable-find "dvipdf") "dvipdf")
      (t nil)))

    (galactron-default
     doc-view-odf->pdf-converter-program
     (cond
      ((galactron-flatpak--executable-find "soffice") "soffice")
      ((galactron-flatpak--executable-find "unoconv") "unoconv")
      (t nil)))

    (galactron-default
     doc-view-ps2pdf-program
     (cond
      ((galactron-flatpak--executable-find "ps2pdf") "ps2pdf")
      (t nil)))

    (galactron-default
     doc-view-pdftotext-program
     (cond
      ((galactron-flatpak--executable-find "pdftotext") "pdftotext")
      (t nil)))))
;; [[help:galactron-flatpak-enabled-docview][DocView调用Flatpak命令]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-flatpak-enabled-pdf\]\[PDF Tools调用Flatpak命令\]\]][[[help:galactron-flatpak-enabled-pdf][PDF Tools调用Flatpak命令]]:1]]
(defcustom galactron-flatpak-enabled-pdf t
  "PDF Tools调用Flatpak命令"
  :type 'boolean
  :tag "PDF Tools")

(when (and galactron-flatpak-enabled-pdf
           (equal (getenv "container") "flatpak"))
  (galactron-when-package-installed pdf-tools
    (dolist (fun
             '(pdf-info-check-epdfinfo
               pdf-util-convert
               pdf-util-image-file-size))
      (advice-add fun :around 'galactron-flatpak--call-process-a))

    (dolist (fun
             '(pdf-info-process-assert-running
               pdf-misc-print-document
               pdf-util-convert-asynch))
      (advice-add fun :around 'galactron-flatpak--start-process-a))))
;; [[help:galactron-flatpak-enabled-pdf][PDF Tools调用Flatpak命令]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-flatpak-enabled-djvu\]\[Djvu调用Flatpak命令\]\]][[[help:galactron-flatpak-enabled-djvu][Djvu调用Flatpak命令]]:1]]
(defcustom galactron-flatpak-enabled-djvu t
  "Djvu调用Flatpak命令"
  :type 'boolean
  :tag "Djvu")

(when (and galactron-flatpak-enabled-djvu
           (equal (getenv "container") "flatpak"))
  (galactron-when-package-installed djvu
    (dolist (item
             '((djvu-djvused . "djvused")
               (djvu-image . "ddjvu")
               (djvu-inspect-file . "djvused")
               (djvu-delete-page . "djvm")))
      (advice-add (car item) :around
                  (if (galactron-flatpak--executable-find (cdr item))
                      'galactron-flatpak--call-process-a
                    'galactron-flatpak--call-process-toolbox-a)))))
;; [[help:galactron-flatpak-enabled-djvu][Djvu调用Flatpak命令]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-flatpak-enabled-ssp-mpv\]\[SSP(mpv)调用Flatpak命令\]\]][[[help:galactron-flatpak-enabled-ssp-mpv][SSP(mpv)调用Flatpak命令]]:1]]
(defcustom galactron-flatpak-enabled-ssp-mpv 'smplayer
  "SSP(mpv)调用Flatpak命令"
  :type
  '(choice
    (const :tag "io.mpv.Mpv" mpv)
    (conat :tag "io.github.celluloid_player.Celluloid" celluloid)
    (const :tag "info.smplayer.SMPlayer" smplayer)
    (const :tag "禁用" nil))
  :tag "SSP(mpv)")

(when (and galactron-flatpak-enabled-ssp-mpv
           (equal (getenv "container") "flatpak"))
  (galactron-when-package-installed ssp
    (galactron-default
     ssp-player-mpv-command
     (list
      "flatpak-spawn" "--host" "--watch-bus" "--"
      "flatpak" "run" "-p" "--socket=x11" "--nosocket=wayland"
      "--filesystem=/tmp" "--command=mpv"
      (pcase galactron-flatpak-enabled-ssp-mpv
        (`mpv "io.mpv.Mpv")
        (`celluloid "io.github.celluloid_player.Celluloid")
        (`smplayer "info.smplayer.SMPlayer"))))))
;; [[help:galactron-flatpak-enabled-ssp-mpv][SSP(mpv)调用Flatpak命令]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-flatpak-enabled-ssp-vlc\]\[SSP(vlc)调用Flatpak命令\]\]][[[help:galactron-flatpak-enabled-ssp-vlc][SSP(vlc)调用Flatpak命令]]:1]]
(defcustom galactron-flatpak-enabled-ssp-vlc t
  "SSP(vlc)调用Flatpak命令"
  :type 'boolean
  :tag "SSP(vlc)")

(when (and galactron-flatpak-enabled-ssp-vlc
           (equal (getenv "container") "flatpak"))
  (galactron-when-package-installed ssp
    (galactron-default
     ssp-player-vlc-command
     '("flatpak-spawn" "--host" "--watch-bus" "--"
       "flatpak" "run" "-p" "--socket=x11" "--nosocket=wayland"
       "org.videolan.VLC"))))
;; [[help:galactron-flatpak-enabled-ssp-vlc][SSP(vlc)调用Flatpak命令]]:1 ends here
(provide 'galactron-flatpak)
;;; galactron-flatpak.el ends here
