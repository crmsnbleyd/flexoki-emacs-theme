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

(defconst flexoki-theme-collection
  '(flexoki-theme-dark flexoki-theme-light)
  "Symbols of the flexoki themes")

(provide 'flexoki-theme)
;;; flexoki-theme.el ends here
