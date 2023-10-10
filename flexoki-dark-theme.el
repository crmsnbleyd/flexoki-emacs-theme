;;; flexoki-dark-theme.el --- Dark variant of flexoki-theme -*- lexical-binding:t -*-

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
(require 'flexoki-theme)

(deftheme flexoki-dark "Flexoki theme, dark version")

(flexoki-theme-create 'dark 'flexoki-dark)

(run-hooks 'flexoki-theme-after-load-theme-hook)

(provide-theme 'flexoki-dark)

(provide 'flexoki-dark-theme)

;;; lambda-dark-theme.el ends here
