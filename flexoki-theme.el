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

(defcustom flexoki-theme-custom-colours nil
  "Specify a list of custom colours."
  :type 'alist
  :group 'flexoki-theme)

(defcustom flexoki-themes-set-theme 'light
  "Choose which theme variant, light or dark, to use."
  :group 'flexoki-theme
  :type 'symbol)

(defcustom flexoki-theme-set-italic-comments t
  "If t then use italics for comments."
  :group 'flexoki-theme
  :type 'boolean)

(defcustom flexoki-theme-set-italic-keywords t
  "If t then use italics for keywords."
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
  "A grey for flexoki-theme."                :group 'faces)
(defface flexoki-highlight  nil
  "Slightly lighter grey for flexoki-theme." :group 'faces)
(defface flexoki-ultralight nil
  "Very light grey for flexoki-theme."       :group 'faces)

;; Greys from the Flexoki definition
(defconst flexoki-colour-900 "#232726")
(defconst flexoki-colour-800 "#403e3c")
(defconst flexoki-colour-700 "#575653")
(defconst flexoki-colour-600 "#6f6e69")
(defconst flexoki-colour-500 "#878580")
(defconst flexoki-colour-300 "#b7b5ac")
(defconst flexoki-colour-200 "#cecdc3")
(defconst flexoki-colour-100 "#e6e4d9")

;;;; After Load Theme Hook
(defvar flexoki-theme-after-load-theme-hook nil
  "Hook run after flexoki-theme is loaded using `load-theme'.")

(defun flexoki-theme-create (variant theme-name)
  "Define theme with THEME-NAME using VARIANT settings"
  (let ((flexoki-bg
	 (if (eq variant 'light) "#fffcf0" "#100f0f"))
	(flexoki-faint-bg
	 (if (eq variant 'light) flexoki-colour-100 flexoki-colour-900))
	(flexoki-fg
	 (if (eq variant 'light) "#100f0f" "#fffcf0"))
	;; accents
	(flexoki-red
	 (if (eq variant 'light) "#af3029" "#d14d41"))
	(flexoki-orange
	 (if (eq variant 'light) "#bc5215" "#da702c"))
	(flexoki-yellow
	 (if (eq variant 'light) "#ad8301" "#d0a215"))
	(flexoki-green
	 (if (eq variant 'light) "#66800b" "#879a39"))
	(flexoki-cyan
	 (if (eq variant 'light) "#24837b" "#3aa99f"))
	(flexoki-blue
	 (if (eq variant 'light) "#205EA6" "#4385be"))
	(flexoki-purple
	 (if (eq variant 'light) "#5E409D" "#8b7ec8"))
	(flexoki-magenta
	 (if (eq variant 'light) "#a02f6f" "#ce5d97"))
	;; foreground variants
	(flexoki-lowlight
	 (if (eq variant 'light) flexoki-800 flexoki-200))
	(flexoki-highlight
	 (if (eq variant 'light) flexoki-700 flexoki-300))
	(flexoki-ultralight
	 (if (eq variant 'light) flexoki-600 flexoki-500))
	(flexoki-meek
	 (if (eq variant 'light) flexoki-500 flexoki-600)))

    ;; set any extra colours
    (dolist (item flexoki-theme-custom-colours)
      (pcase item
	(`(,cvar . ,val) (set cvar val))))
    
    (custom-theme-set-faces
     theme-name

     `(default
       ((,class (:background ,flexoki-bg :foreground ,flexoki-fg))))
     `(cursor
       ((,class (:background ,flexoki-fg))))
     `(fringe
       ((,class (:background ,flexoki-bg :weight light))))
     `(hl-line
       ((,class (:background ,flexoki-highlight))))
     `(region
       ((,class (:background ,flexoki-lowlight))))
     `(secondary-selection
       ((,class (:background ,flexoki-highlight))))
     `(buffer-menu-buffer
       ((,class (:background ,flexoki-fg))))
     `(minibuffer-prompt
       ((,class (:background ,flexoki-yellow))))
     `(vertical-border
       ((,class (:foreground ,flexoki-bg))))
     `(internal-border
       ((,class (:background ,flexoki-bg :foreground ,flexoki-bg))))
     `(show-paren-match
       ((,class
	 (:background ,flexoki-ultralight
	  :foreground ,flexoki-yellow
	  :weight bold))))
     `(show-paren-mismatch
       ((,class
	 (:background ,flexoki-ultralight
	  :foreground ,flexoki-red
	  :weight bold
	  :box t))))
     `(link
       ((,class
	 (:background ,flexoki-lowlight
	  :foreground ,flexoki-fg
	  :weight semi-bold
	  :underline t))))
     `(shadow
       ((,class (:foreground ,flexoki-ultralight))))

     ;; NOTE: We want the flexoki-theme- colors to be available as faces. It seems like there
     ;; should be a better way to do this but...
     `(flexoki-fg          ((,class (:foreground ,flexoki-fg))))
     `(flexoki-bg          ((,class (:background ,flexoki-bg))))
     `(flexoki-faint-bg    ((,class (:background ,flexoki-faint-bg))))
     `(flexoki-ultralight  ((,class (:background ,flexoki-ultralight))))
     `(flexoki-highlight   ((,class (:foreground ,flexoki-highlight))))
     `(flexoki-lowlight    ((,class (:foreground ,flexoki-lowlight))))
     `(flexoki-meek        ((,class (:background ,flexoki-meek))))
     `(flexoki-blue        ((,class (:foreground ,flexoki-blue))))
     `(flexoki-cyan        ((,class (:foreground ,flexoki-cyan))))
     `(flexoki-green       ((,class (:foreground ,flexoki-green))))
     `(flexoki-magenta     ((,class (:foreground ,flexoki-magenta))))
     `(flexoki-orange      ((,class (:foreground ,flexoki-orange))))
     `(flexoki-purple      ((,class (:foreground ,flexoki-purple))))
     `(flexoki-red         ((,class (:foreground ,flexoki-red))))
     `(flexoki-yellow      ((,class (:foreground ,flexoki-yellow))))

;;;;; Basic faces
     `(error               ((,class (:foreground ,flexoki-red :bold t))))
     `(success             ((,class (:foreground ,flexoki-green :bold t))))
     `(warning             ((,class (:foreground ,flexoki-yellow :bold t))))
     `(alert-low-face      ((,class (:foreground ,flexoki-orange))))
     `(escape-glyph        ((,class (:foreground ,flexoki-cyan))))
     `(highlight           ((,class (:background ,flexoki-highlight))))
     `(homoglyph           ((,class (:foreground ,flexoki-blue))))
     `(match               ((,class (:foreground ,flexoki-lowlight :background ,flexoki-blue))))
     
;;;;; built-in syntax (font-lock)

     `(font-lock-builtin-face
       ((,class (:foreground ,flexoki-fg :weight light))))
     `(font-lock-constant-face
       ((,class (:foreground ,flexoki-fg :weight light))))
     `(font-lock-comment-face
       ((,class (:foreground ,flexoki-meek
		 :slant ,(if flexoki-theme-set-italic-comments 'italic 'normal)
		 :weight normal))))
     `(font-lock-function-name-face
       ((,class (:foreground ,flexoki-highlight :weight bold))))
     `(font-lock-keyword-face
       ((,class (:foreground ,flexoki-fg
		 :weight light
		 :slant ,(if flexoki-theme-set-italic-keywords 'italic 'normal)))))
     `(font-lock-string-face
       ((,class (:foreground ,flexoki-fg :background ,flexoki-faint-bg))))
     `(font-lock-variable-name-face
       ((,class (:foreground ,flexoki-highlight :weight light))))
     `(font-lock-type-face
       ((,class (:foreground ,flexoki-fg :weight light))))
     `(font-lock-warning-face
       ((,class (:foreground ,flexoki-yellow :weight bold))))
     `(font-lock-preprocessor-face
       ((,class (:foreground ,flexoki-fg :weight medium))))

;;;;; Childframes
;;;;;; Mini-Frame
     `(mini-popup-background ((,class (:background ,flexoki-faint-bg))))
     `(mini-popup-border     ((,class (:background ,flexoki-faint-bg))))

     `;;;;;; Mini-Popup (Childframe)
     `(mini-popup-background ((,class (:background ,flexoki-faint-bg))))
     `(mini-popup-border     ((,class (:background ,flexoki-faint-bg))))

;;;;;; Posframe
     `(which-key-posframe
       ((,class (:background ,flexoki-faint-bg))))
     `(which-key-posframe-border
       ((,class (:background ,flexoki-faint-bg))))
     `(transient-posframe-border
       ((,class (:background ,flexoki-faint-bg))))
     `(transient-posframe
       ((,class (:foreground ,flexoki-highlight :background ,flexoki-faint-bg))))

;;;;; Completion/Narrowing
;;;;;; General Completion
     `(completions-annotations
       ((,class (:foreground ,flexoki-meek))))

;;;;;; Company-mode
     `(company-scrollbar-bg
       ((,class (:background ,flexoki-faint-bg))))
     `(company-scrollbar-fg
       ((,class (:background ,flexoki-mild))))
     `(company-tooltip
       ((,class (:background ,flexoki-mild))))
     `(company-tooltip-annotation
       ((,class (:foreground ,flexoki-green))))
     `(company-tooltip-annotation-selection
       ((,class (:inherit    company-tooltip-annotation))))
     `(company-tooltip-selection
       ((,class (:foreground ,flexoki-purple :background ,flexoki-faint-bg))))
     `(company-tooltip-common
       ((,class (:foreground ,flexoki-blue :underline t))))
     `(company-tooltip-common-selection
       ((,class (:foreground ,flexoki-blue :underline t))))
     `(company-preview-common
       ((,class (:foreground ,flexoki-highlight))))
     `(company-preview
       ((,class (:background ,flexoki-blue))))
     `(company-preview-search
       ((,class (:background ,flexoki-cyan))))
     `(company-template-field
       ((,class (:foreground ,flexoki-black :background ,flexoki-yellow))))
     `(company-echo-common
       ((,class (:foreground ,flexoki-red))))

;;;;;; Corfu
     `(corfu-annotations
       ((,class (:foreground ,flexoki-meek))))
     `(corfu-bar
       ((,class (:foreground ,flexoki-ultralight))))
     `(corfu-border
       ((,class (:foreground ,flexoki-faint-bg))))
     `(corfu-current
       ((,class (:foreground ,flexoki-yellow :background ,flexoki-highlight))))
     `(corfu-default
       ((,class (:inherit default :background ,flexoki-faint-bg))))
     `(corfu-deprecated
       ((,class (:foreground ,flexoki-highlight))))
     `(corfu-echo
       ((,class (:inherit default))))

;;;;;; Vertico
     `(vertico-current
       ((,class (:weight bold :background ,flexoki-highlight))))
     `(vertico-group-separator
       ((,class (:foreground ,flexoki-ultralight :strike-through t))))
     `(vertico-multiline
       ((,class (:foreground ,flexoki-meek))))
     `(vertico-group-title
       ((,class (:foreground ,flexoki-meek))))

;;;;; Diffs & VC

;;;;;; Diff
     `(diff-header
       ((,class (:foreground ,flexoki-fg))))
     `(diff-file-header
       ((,class (:foreground ,flexoki-fg))))
     `(diff-hunk-header
       ((,class (:foreground ,flexoki-fg))))
     `(diff-context
       ((,class (:background ,flexoki-lowlight))))
     `(diff-changed
       ((,class (:background unspecified :foreground ,flexoki-blue))))
     `(diff-refine-changed
       ((,class (:foreground ,flexoki-blue))))
     `(diff-added
       ((,class (:background unspecified :foreground ,flexoki-green))))
     `(diff-refine-added
       ((,class (:background unspecified :foreground ,flexoki-green))))
     `(diff-removed
       ((,class (:background unspecified :foreground ,flexoki-red))))
     `(diff-refine-removed
       ((,class (:background unspecified :foreground ,flexoki-meek :strike-through t))))
     `(diff-indicator-changed
       ((,class (:inherit diff-changed))))
     `(diff-indicator-added
       ((,class (:inherit diff-added))))
     `(diff-indicator-removed
       ((,class (:inherit diff-removed))))

;;;;;; Diff-hl
     `(diff-hl-change ((,class (:inherit default :foreground ,flexoki-blue ))))
     `(diff-hl-delete ((,class (:inherit default :foreground ,flexoki-red  ))))
     `(diff-hl-insert ((,class (:inherit default :foreground ,flexoki-green))))

;;;;;; Ediff
     `(ediff-even-diff-A
       ((,class (:background ,flexoki-lowlight))))
     `(ediff-even-diff-B
       ((,class (:background ,flexoki-lowlight))))
     `(ediff-even-diff-C
       ((,class (:background ,flexoki-lowlight))))
     `(ediff-even-diff-Ancestor
       ((,class (:background ,flexoki-lowlight))))
     `(ediff-odd-diff-A
       ((,class (:background ,flexoki-faint-bg))))
     `(ediff-odd-diff-B
       ((,class (:background ,flexoki-faint-bg))))
     `(ediff-odd-diff-C
       ((,class (:background ,flexoki-faint-bg))))
     `(ediff-odd-diff-Ancestor
       ((,class (:background ,flexoki-faint-bg))))

;;;;;; Magit
     `(magit-header-line
       ((,class (:foreground ,flexoki-fg :background ,flexoki-highlight))))
     `(magit-header-line-log-select
       ((,class (:foreground ,flexoki-fg :background ,flexoki-highlight))))
     `(magit-section-heading
       ((,class (:foreground ,flexoki-meek :height 1.4))))
     `(magit-dimmed
       ((,class (:foreground ,flexoki-meek))))
     `(magit-blame-dimmed
       ((,class (:foreground ,flexoki-meek))))

;;;;;; Rainbow delimiters
     `(rainbow-delimiters-depth-1-face
       ((,class (:foreground ,flexoki-green))))
     `(rainbow-delimiters-depth-2-face
       ((,class (:foreground ,flexoki-purple))))
     `(rainbow-delimiters-depth-3-face
       ((,class (:foreground ,flexoki-orange))))
     `(rainbow-delimiters-depth-4-face
       ((,class (:foreground ,flexoki-blue))))
     `(rainbow-delimiters-depth-5-face
       ((,class (:foreground ,flexoki-green))))
     `(rainbow-delimiters-depth-6-face
       ((,class (:foreground ,flexoki-purple))))
     `(rainbow-delimiters-depth-7-face
       ((,class (:foreground ,flexoki-orange))))
     `(rainbow-delimiters-depth-8-face
       ((,class (:foreground ,flexoki-blue))))
     `(rainbow-delimiters-depth-9-face
       ((,class (:foreground ,flexoki-green))))
     `(rainbow-delimiters-depth-10-face
       ((,class (:foreground ,flexoki-purple))))
     `(rainbow-delimiters-depth-11-face
       ((,class (:foreground ,flexoki-orange))))
     `(rainbow-delimiters-depth-12-face
       ((,class (:foreground ,flexoki-blue))))
     `(rainbow-delimiters-unmatched-face
       ((,class (:background ,flexoki-bg :foreground ,flexoki-red :weight bold))))

;;;;;; Eshell
     `(eshell-prompt
       ((,class (:foreground ,flexoki-yellow))))
     `(eshell-ls-archive
       ((,class (:foreground ,flexoki-meek))))
     `(eshell-ls-backup
       ((,class (:foreground ,flexoki-meek))))
     `(eshell-ls-clutter
       ((,class (:foreground ,flexoki-orange :weight bold))))
     `(eshell-ls-directory
       ((,class (:foreground ,flexoki-blue :weight bold))))
     `(eshell-ls-executable
       ((,class (:weight bold))))
     `(eshell-ls-missing
       ((,class (:foreground ,flexoki-red :bold t))))
     `(eshell-ls-product
       ((,class (:foreground ,flexoki-red))))
     `(eshell-ls-readonly
       ((,class (:backgtround ,flexoki-highlight :foreground ,flexoki-meek :weight light))))
     `(eshell-ls-special
       ((,class (:foreground ,flexoki-yellow :bold t))))
     `(eshell-ls-symlink
       ((,class (:foreground ,flexoki-red))))
     `(eshell-ls-unreadable
       ((,class (:foreground ,flexoki-red :bold t))))

;;;;;; Shell script
     `(sh-quoted-exec
       ((,class (:foreground ,flexoki-purple))))
     `(sh-heredoc
       ((,class (:foreground ,flexoki-orange))))
     )))

(provide 'flexoki-theme)

;;; flexoki-theme.el ends here
