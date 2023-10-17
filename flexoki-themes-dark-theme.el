;;; flexoki-themes-dark-theme.el --- Dark variant of flexoki-theme -*- lexical-binding:t -*-

;; Copyright (C) 2023 Andrew Jose, Steph Ango

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Andrew Jose <arnav.jose@gmail.com>
;; Maintainer: Andrew Jose <arnav.jose@gmail.com>
;; URL: https://github.com/crmsnbleyd/flexoki-emacs-theme
;; Keywords: faces, theme

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; The `flexoki-themes' is a pair of light and dark themes for GNU
;; Emacs based on the Flexoki colour scheme by Steph Ango.

;;; Code:
(require 'flexoki-themes)

(deftheme flexoki-themes-dark "Flexoki theme, dark version.")

(flexoki-themes-create 'dark 'flexoki-themes-dark)

(run-hooks 'flexoki-themes-after-load-theme-hook)

(provide-theme 'flexoki-themes-dark)

(provide 'flexoki-themes-dark-theme)

;;; flexoki-themes-dark-theme.el ends here
