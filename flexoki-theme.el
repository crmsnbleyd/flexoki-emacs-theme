;;; flexoki-theme.el --- An inky color scheme for prose and code -*- lexical-binding:t -*-

;; Copyright (C) 2023 Andrew Jose, Steph Ango

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Andrew Jose <arnav.jose@gmail.com>
;; Maintainer: Andrew Jose <arnav.jose@gmail.com>
;; URL: https://github.com/crmsnbleyd/flexoki-emacs-theme
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, theme

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; The `flexoki-theme' is a pair of light and dark themes for GNU
;; Emacs based on the Flexoki colour scheme by Steph Ango.

;;; Code:
(defgroup flexoki-theme ()
  "Inky themes for prose and code."
  :group 'faces
  :link '(info-link "(flexoki-theme) Top")
  :link '(url-link
	  :tag "Homepage"
	  "https://github.com/crmsnbleyd/flexoki-emacs-theme")
  :prefix "flexoki-theme-"
  :tag "Flexoki Theme")

(defconst flexoki-theme-variants
  '(dark light)
  "Symbols of the flexoki themes")

(defcustom flexoki-themes-set-theme 'light
  "Choose which theme variant, light or dark, to use."
  :group 'flexoki-theme
  :type 'symbol)

(defcustom flexoki-theme-set-italic-comments t
  "If t then use italics for comments."
  :group 'flexoki-theme
  :type 'boolean)

(defface flexoki-bg         nil
  "Background face for flexoki-theme."       :group 'faces)
(defface flexoki-fg         nil
  "Foreground face for flexoki-theme."       :group 'faces)
(defface flexoki-red        nil
  "Red accent colour for flexoki-theme."     :group 'faces)
(defface flexoki-orange     nil
  "Orange accent colour for flexoki-theme."  :group 'faces)
(defface flexoki-yellow     nil
  "Yellow accent colour for flexoki-theme."  :group 'faces)
(defface flexoki-green      nil
  "Green accent colour for flexoki-theme."   :group 'faces)
(defface flexoki-cyan       nil
  "Cyan accent colour for flexoki-theme."    :group 'faces)
(defface flexoki-blue       nil
  "Blue accent colour for flexoki-theme."    :group 'faces)
(defface flexoki-purple     nil
  "Purple accent colour for flexoki-theme."  :group 'faces)
(defface flexoki-magenta    nil
  "Magenta accent colour for flexoki-theme." :group 'faces)
(defface flexoki-lowlight   nil
  "Grey colour for flexoki-theme."           :group 'faces)
(defface flexoki-highlight  nil
  "Grey colour for flexoki-theme."           :group 'faces)
(defface flexoki-ultralight nil
  "Grey colour for flexoki-theme."           :group 'faces)

(defun flexoki-theme-create (variant theme-name)
  "Define theme with THEME-NAME using VARIANT settings"
  (let ((flexoki-bg
	 (if (eq variant 'dark) "#fffcf0" "#100f0f"))
	(flexoki-fg
	 (if (eq variant 'dark) "#100f0f" "#fffcf0"))
	;; accents
	(flexoki-red
	 (if (eq variant 'dark) "#af3029" "#d14d41"))
	(flexoki-orange
	 (if (eq variant 'dark) "#bc5215" "#da702c"))
	(flexoki-yellow
	 (if (eq variant 'dark) "#ad8301" "#d0a215"))
	(flexoki-green
	 (if (eq variant 'dark) "#66800b" "#879a39"))
	(flexoki-cyan
	 (if (eq variant 'dark) "#24837b" "#3aa99f"))
	(flexoki-blue
	 (if (eq variant 'dark) "#205EA6" "#4385be"))
	(flexoki-purple
	 (if (eq variant 'dark) "#5E409D" "#8b7ec8"))
	(flexoki-magenta
	 (if (eq variant 'dark) "#a02f6f" "#ce5d97"))
	;; background variants
	(flexoki-blue
	 (if (eq variant 'dark) "#205EA6" "#4385be"))
	(flexoki-purple
	 (if (eq variant 'dark) "#5E409D" "#8b7ec8"))
	(flexoki-magenta
	 (if (eq variant 'dark) "#a02f6f" "#ce5d97")))

    (custom-theme-set-faces
     theme-name

     `(default  ((,class (:background ,flexoki-bg :foreground ,flexoki-fg))))
     `(cursor   ((,class (:background ,flexoki-fg))))
     `(fringe   ((,class (:background ,flexoki-bg :weight light))))
     `(hl-line  ((,class (:background ,flexoki-highlight))))
     )))

(provide 'flexoki-theme)
;;; flexoki-theme.el ends here
