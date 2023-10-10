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

;;;; After Load Theme Hook
(defvar flexoki-theme-after-load-theme-hook nil
  "Hook run after flexoki-theme is loaded using `load-theme'.")

(defun flexoki-theme-create (variant theme-name)
  "Define theme with THEME-NAME using VARIANT settings"
  (let*
      ((flexoki-colour-black "#100f0f")
       (flexoki-colour-900   "#232726")
       (flexoki-colour-800   "#403e3c")
       (flexoki-colour-700   "#575653")
       (flexoki-colour-600   "#6f6e69")
       (flexoki-colour-500   "#878580")
       (flexoki-colour-300   "#b7b5ac")
       (flexoki-colour-200   "#cecdc3")
       (flexoki-colour-100   "#e6e4d9")
       (flexoki-colour-paper "#fffcf0")
       (flexoki-bg
	(if (eq variant 'light)
	    flexoki-colour-paper flexoki-colour-black))
       (flexoki-faint-bg
	(if (eq variant 'light)
	    flexoki-colour-100 flexoki-colour-900))
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
	(if (eq variant 'light) flexoki-colour-800 flexoki-colour-200))
       (flexoki-highlight
	(if (eq variant 'light) flexoki-colour-700 flexoki-colour-300))
       (flexoki-ultralight
	(if (eq variant 'light) flexoki-colour-600 flexoki-colour-500))
       (flexoki-meek
	(if (eq variant 'light) flexoki-colour-500 flexoki-colour-600)))

    ;; set any extra colours
    (dolist (item flexoki-theme-custom-colours)
      (pcase item
	(`(,cvar . ,val) (set cvar val))))
    
    (custom-theme-set-faces
     theme-name

     `(default
       ((t (:background ,flexoki-bg :foreground ,flexoki-fg))))
     `(cursor
       ((t (:background ,flexoki-fg))))
     `(fringe
       ((t (:background ,flexoki-bg :weight light))))
     `(hl-line
       ((t (:background ,flexoki-highlight))))
     `(region
       ((t (:background ,flexoki-lowlight))))
     `(secondary-selection
       ((t (:background ,flexoki-highlight))))
     `(buffer-menu-buffer
       ((t (:background ,flexoki-fg))))
     `(minibuffer-prompt
       ((t (:background ,flexoki-yellow))))
     `(vertical-border
       ((t (:foreground ,flexoki-bg))))
     `(internal-border
       ((t (:background ,flexoki-bg :foreground ,flexoki-bg))))
     `(show-paren-match
       ((t
	 (:background ,flexoki-ultralight
	  :foreground ,flexoki-yellow
	  :weight bold))))
     `(show-paren-mismatch
       ((t
	 (:background ,flexoki-ultralight
	  :foreground ,flexoki-red
	  :weight bold
	  :box t))))
     `(link
       ((t
	 (:background ,flexoki-lowlight
	  :foreground ,flexoki-fg
	  :weight semi-bold
	  :underline t))))
     `(shadow
       ((t (:foreground ,flexoki-ultralight))))

     ;; NOTE: We want the flexoki-theme- colors to be available as faces. It seems like there
     ;; should be a better way to do this but...
     `(flexoki-fg          ((t (:foreground ,flexoki-fg))))
     `(flexoki-bg          ((t (:background ,flexoki-bg))))
     `(flexoki-faint-bg    ((t (:background ,flexoki-faint-bg))))
     `(flexoki-ultralight  ((t (:background ,flexoki-ultralight))))
     `(flexoki-highlight   ((t (:foreground ,flexoki-highlight))))
     `(flexoki-lowlight    ((t (:foreground ,flexoki-lowlight))))
     `(flexoki-meek        ((t (:background ,flexoki-meek))))
     `(flexoki-blue        ((t (:foreground ,flexoki-blue))))
     `(flexoki-cyan        ((t (:foreground ,flexoki-cyan))))
     `(flexoki-green       ((t (:foreground ,flexoki-green))))
     `(flexoki-magenta     ((t (:foreground ,flexoki-magenta))))
     `(flexoki-orange      ((t (:foreground ,flexoki-orange))))
     `(flexoki-purple      ((t (:foreground ,flexoki-purple))))
     `(flexoki-red         ((t (:foreground ,flexoki-red))))
     `(flexoki-yellow      ((t (:foreground ,flexoki-yellow))))

;;;;; Basic faces
     `(error               ((t (:foreground ,flexoki-red :bold t))))
     `(success             ((t (:foreground ,flexoki-green :bold t))))
     `(warning             ((t (:foreground ,flexoki-yellow :bold t))))
     `(alert-low-face      ((t (:foreground ,flexoki-orange))))
     `(escape-glyph        ((t (:foreground ,flexoki-cyan))))
     `(highlight           ((t (:background ,flexoki-highlight))))
     `(homoglyph           ((t (:foreground ,flexoki-blue))))
     `(match               ((t (:foreground ,flexoki-lowlight :background ,flexoki-blue))))
     
;;;;; built-in syntax (font-lock)

     `(font-lock-builtin-face
       ((t (:foreground ,flexoki-fg :weight light))))
     `(font-lock-constant-face
       ((t (:foreground ,flexoki-fg :weight light))))
     `(font-lock-comment-face
       ((t (:foreground ,flexoki-meek
		 :slant ,(if flexoki-theme-set-italic-comments 'italic 'normal)
		 :weight normal))))
     `(font-lock-function-name-face
       ((t (:foreground ,flexoki-highlight :weight bold))))
     `(font-lock-keyword-face
       ((t (:foreground ,flexoki-fg
		 :weight light
		 :slant ,(if flexoki-theme-set-italic-keywords 'italic 'normal)))))
     `(font-lock-string-face
       ((t (:foreground ,flexoki-fg :background ,flexoki-faint-bg))))
     `(font-lock-variable-name-face
       ((t (:foreground ,flexoki-highlight :weight light))))
     `(font-lock-type-face
       ((t (:foreground ,flexoki-fg :weight light))))
     `(font-lock-warning-face
       ((t (:foreground ,flexoki-yellow :weight bold))))
     `(font-lock-preprocessor-face
       ((t (:foreground ,flexoki-fg :weight medium))))

;;;;; Childframes
;;;;;; Mini-Frame
     `(mini-popup-background ((t (:background ,flexoki-faint-bg))))
     `(mini-popup-border     ((t (:background ,flexoki-faint-bg))))

     `;;;;;; Mini-Popup (Childframe)
     `(mini-popup-background ((t (:background ,flexoki-faint-bg))))
     `(mini-popup-border     ((t (:background ,flexoki-faint-bg))))

;;;;;; Posframe
     `(which-key-posframe
       ((t (:background ,flexoki-faint-bg))))
     `(which-key-posframe-border
       ((t (:background ,flexoki-faint-bg))))
     `(transient-posframe-border
       ((t (:background ,flexoki-faint-bg))))
     `(transient-posframe
       ((t (:foreground ,flexoki-highlight :background ,flexoki-faint-bg))))

;;;;; Completion/Narrowing
;;;;;; General Completion
     `(completions-annotations
       ((t (:foreground ,flexoki-meek))))

;;;;;; Company-mode
     `(company-scrollbar-bg
       ((t (:background ,flexoki-faint-bg))))
     `(company-scrollbar-fg
       ((t (:background ,flexoki-meek))))
     `(company-tooltip
       ((t (:background ,flexoki-meek))))
     `(company-tooltip-annotation
       ((t (:foreground ,flexoki-green))))
     `(company-tooltip-annotation-selection
       ((t (:inherit    company-tooltip-annotation))))
     `(company-tooltip-selection
       ((t (:foreground ,flexoki-purple :background ,flexoki-faint-bg))))
     `(company-tooltip-common
       ((t (:foreground ,flexoki-blue :underline t))))
     `(company-tooltip-common-selection
       ((t (:foreground ,flexoki-blue :underline t))))
     `(company-preview-common
       ((t (:foreground ,flexoki-highlight))))
     `(company-preview
       ((t (:background ,flexoki-blue))))
     `(company-preview-search
       ((t (:background ,flexoki-cyan))))
     `(company-template-field
       ((t (:foreground ,flexoki-colour-black :background ,flexoki-yellow))))
     `(company-echo-common
       ((t (:foreground ,flexoki-red))))

;;;;;; Corfu
     `(corfu-annotations
       ((t (:foreground ,flexoki-meek))))
     `(corfu-bar
       ((t (:foreground ,flexoki-ultralight))))
     `(corfu-border
       ((t (:foreground ,flexoki-faint-bg))))
     `(corfu-current
       ((t (:foreground ,flexoki-yellow :background ,flexoki-lowlight))))
     `(corfu-default
       ((t (:inherit default :background ,flexoki-faint-bg))))
     `(corfu-deprecated
       ((t (:foreground ,flexoki-highlight))))
     `(corfu-echo
       ((t (:inherit default))))

;;;;;; Vertico
     `(vertico-current
       ((t (:weight bold :background ,flexoki-highlight))))
     `(vertico-group-separator
       ((t (:foreground ,flexoki-ultralight :strike-through t))))
     `(vertico-multiline
       ((t (:foreground ,flexoki-meek))))
     `(vertico-group-title
       ((t (:foreground ,flexoki-meek))))

;;;;; Diffs & VC

;;;;;; Diff
     `(diff-header
       ((t (:foreground ,flexoki-fg))))
     `(diff-file-header
       ((t (:foreground ,flexoki-fg))))
     `(diff-hunk-header
       ((t (:foreground ,flexoki-fg))))
     `(diff-context
       ((t (:background ,flexoki-lowlight))))
     `(diff-changed
       ((t (:background unspecified :foreground ,flexoki-blue))))
     `(diff-refine-changed
       ((t (:foreground ,flexoki-blue))))
     `(diff-added
       ((t (:background unspecified :foreground ,flexoki-green))))
     `(diff-refine-added
       ((t (:background unspecified :foreground ,flexoki-green))))
     `(diff-removed
       ((t (:background unspecified :foreground ,flexoki-red))))
     `(diff-refine-removed
       ((t (:background unspecified :foreground ,flexoki-meek :strike-through t))))
     `(diff-indicator-changed
       ((t (:inherit diff-changed))))
     `(diff-indicator-added
       ((t (:inherit diff-added))))
     `(diff-indicator-removed
       ((t (:inherit diff-removed))))

;;;;;; Diff-hl
     `(diff-hl-change ((t (:inherit default :foreground ,flexoki-blue ))))
     `(diff-hl-delete ((t (:inherit default :foreground ,flexoki-red  ))))
     `(diff-hl-insert ((t (:inherit default :foreground ,flexoki-green))))

;;;;;; Ediff
     `(ediff-even-diff-A
       ((t (:background ,flexoki-lowlight))))
     `(ediff-even-diff-B
       ((t (:background ,flexoki-lowlight))))
     `(ediff-even-diff-C
       ((t (:background ,flexoki-lowlight))))
     `(ediff-even-diff-Ancestor
       ((t (:background ,flexoki-lowlight))))
     `(ediff-odd-diff-A
       ((t (:background ,flexoki-faint-bg))))
     `(ediff-odd-diff-B
       ((t (:background ,flexoki-faint-bg))))
     `(ediff-odd-diff-C
       ((t (:background ,flexoki-faint-bg))))
     `(ediff-odd-diff-Ancestor
       ((t (:background ,flexoki-faint-bg))))

;;;;;; Magit
     `(magit-header-line
       ((t (:foreground ,flexoki-fg :background ,flexoki-highlight))))
     `(magit-header-line-log-select
       ((t (:foreground ,flexoki-fg :background ,flexoki-highlight))))
     `(magit-section-heading
       ((t (:foreground ,flexoki-meek))))
     `(magit-dimmed
       ((t (:foreground ,flexoki-meek))))
     `(magit-blame-dimmed
       ((t (:foreground ,flexoki-meek))))

;;;;;; Rainbow delimiters
     `(rainbow-delimiters-depth-1-face
       ((t (:foreground ,flexoki-blue))))
     `(rainbow-delimiters-depth-2-face
       ((t (:foreground ,flexoki-purple))))
     `(rainbow-delimiters-depth-3-face
       ((t (:foreground ,flexoki-orange))))
     `(rainbow-delimiters-depth-4-face
       ((t (:foreground ,flexoki-yellow))))
     `(rainbow-delimiters-depth-5-face
       ((t (:foreground ,flexoki-blue))))
     `(rainbow-delimiters-depth-6-face
       ((t (:foreground ,flexoki-purple))))
     `(rainbow-delimiters-depth-7-face
       ((t (:foreground ,flexoki-orange))))
     `(rainbow-delimiters-depth-8-face
       ((t (:foreground ,flexoki-yellow))))
     `(rainbow-delimiters-depth-9-face
       ((t (:foreground ,flexoki-blue))))
     `(rainbow-delimiters-depth-10-face
       ((t (:foreground ,flexoki-purple))))
     `(rainbow-delimiters-depth-11-face
       ((t (:foreground ,flexoki-orange))))
     `(rainbow-delimiters-depth-12-face
       ((t (:foreground ,flexoki-yellow))))
     `(rainbow-delimiters-unmatched-face
       ((t (:background ,flexoki-bg :foreground ,flexoki-red :weight bold))))

;;;;;; Eshell
     `(eshell-prompt
       ((t (:foreground ,flexoki-yellow))))
     `(eshell-ls-archive
       ((t (:foreground ,flexoki-meek))))
     `(eshell-ls-backup
       ((t (:foreground ,flexoki-meek))))
     `(eshell-ls-clutter
       ((t (:foreground ,flexoki-orange :weight bold))))
     `(eshell-ls-directory
       ((t (:foreground ,flexoki-blue :weight bold))))
     `(eshell-ls-executable
       ((t (:weight bold))))
     `(eshell-ls-missing
       ((t (:foreground ,flexoki-red :bold t))))
     `(eshell-ls-product
       ((t (:foreground ,flexoki-red))))
     `(eshell-ls-readonly
       ((t (:backgtround ,flexoki-highlight :foreground ,flexoki-meek :weight light))))
     `(eshell-ls-special
       ((t (:foreground ,flexoki-yellow :bold t))))
     `(eshell-ls-symlink
       ((t (:foreground ,flexoki-red))))
     `(eshell-ls-unreadable
       ((t (:foreground ,flexoki-red :bold t))))

;;;;;; Shell script
     `(sh-quoted-exec
       ((t (:foreground ,flexoki-purple))))
     `(sh-heredoc
       ((t (:foreground ,flexoki-orange))))
     )))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'flexoki-theme)

;;; flexoki-theme.el ends here
