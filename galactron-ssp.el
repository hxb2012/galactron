;;; galactron-ssp.el --- 修改SSP设置  -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require 'galactron)

;;;###autoload(defcustom galactron-group-enabled-ssp t "启用修改SSP设置" :type 'boolean :tag "Ssp")

(defgroup galactron-ssp nil
  "修改SSP设置"
  :prefix "galactron-ssp-"
  :tag "Ssp"
  :group 'galactron)
;; [[file:README.org::*\[\[help:galactron-ssp-enabled-player\]\[设置播放器\]\]][[[help:galactron-ssp-enabled-player][设置播放器]]:1]]
(defcustom galactron-ssp-enabled-player 'mpv
  "播放器"
  :type
  '(choice
    (const :tag "MPV" mpv)
    (const :tag "VLC" vlc)
    (const :tag "禁用" nil))
  :tag "Player")

(when galactron-ssp-enabled-player
  (galactron-default
   ssp-player
   (pcase galactron-ssp-enabled-player
     (`mpv 'ssp-player-mpv)
     (`vlc 'ssp-player-vlc))))
;; [[help:galactron-ssp-enabled-player][设置播放器]]:1 ends here
;; [[file:README.org::*\[\[help:galactron-ssp-enabled-auto-mode\]\[关联后缀\]\]][[[help:galactron-ssp-enabled-auto-mode][关联后缀]]:1]]
(defcustom galactron-ssp-enabled-auto-mode t
  "关联后缀"
  :type 'boolean
  :tag "Auto mode")

(when galactron-ssp-enabled-auto-mode
  (add-to-list 'auto-mode-alist '("\\.mp4\\'" . ssp-mode)))
;; [[help:galactron-ssp-enabled-auto-mode][关联后缀]]:1 ends here
(provide 'galactron-ssp)
;;; galactron-ssp.el ends here
