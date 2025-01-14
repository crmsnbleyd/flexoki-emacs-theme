;;; flexoki-themes.el --- An inky color scheme for prose and code -*- lexical-binding:t -*-
;;; Version: 0.20

;; Copyright (C) 2024 Andrew Jose, Jayaram, Steph Ango

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Andrew Jose <mail@drewsh.com>
;; Maintainer: Andrew Jose <mail@drewsh.com>
;; URL: https://github.com/crmsnbleyd/flexoki-emacs-theme
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, theme

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; The `flexoki-themes' is a pair of light and dark themes for GNU
;; Emacs based on the Flexoki colour scheme by Steph Ango.

;;; Code:
(defgroup flexoki-themes ()
  "Inky themes for prose and code."
  :group 'faces
  :link '(info-link "(flexoki-themes) Top")
  :link '(url-link
	  :tag "Homepage"
	  "https://github.com/crmsnbleyd/flexoki-emacs-theme")
  :prefix "flexoki-themes-"
  :tag "Flexoki Theme")

(defconst flexoki-themes-variants
  '(dark light)
  "Symbols of the flexoki themes.")

(defcustom flexoki-themes-custom-colours nil
  "Specify a list of custom colours."
  :type 'alist
  :group 'flexoki-themes)

(defcustom flexoki-themes-set-theme 'light
  "Choose which theme variant, light or dark, to use."
  :group 'flexoki-themes
  :type 'symbol)

(defcustom flexoki-themes-use-italic-comments t
  "If t then use italics for comments."
  :group 'flexoki-themes
  :type 'boolean)

(defcustom flexoki-themes-use-bold-keywords nil
  "If t then use bold weight for keywords."
  :group 'flexoki-themes
  :type 'boolean)

(defcustom flexoki-themes-use-bold-builtins nil
  "If t then use bold weight for font lock builtins."
  :group 'flexoki-themes
  :type 'boolean)

(defface flexoki-themes-bg         nil
  "Background face for flexoki-themes."       :group 'faces)
(defface flexoki-themes-fg         nil
  "Foreground face for flexoki-themes."       :group 'faces)
(defface flexoki-themes-red        nil
  "Red accent colour for flexoki-themes."     :group 'faces)
(defface flexoki-themes-orange     nil
  "Orange accent colour for flexoki-themes."  :group 'faces)
(defface flexoki-themes-yellow     nil
  "Yellow accent colour for flexoki-themes."  :group 'faces)
(defface flexoki-themes-green      nil
  "Green accent colour for flexoki-themes."   :group 'faces)
(defface flexoki-themes-cyan       nil
  "Cyan accent colour for flexoki-themes."    :group 'faces)
(defface flexoki-themes-blue       nil
  "Blue accent colour for flexoki-themes."    :group 'faces)
(defface flexoki-themes-purple     nil
  "Purple accent colour for flexoki-themes."  :group 'faces)
(defface flexoki-themes-magenta    nil
  "Magenta accent colour for flexoki-themes." :group 'faces)
(defface flexoki-themes-lowlight   nil
  "A grey for flexoki-themes."                :group 'faces)
(defface flexoki-themes-highlight  nil
  "Slightly lighter grey for flexoki-themes." :group 'faces)
(defface flexoki-themes-ultralight nil
  "Very light grey for flexoki-themes."       :group 'faces)

;; Greys from the Flexoki definition

;;;; After Load Theme Hook
(defvar flexoki-themes-after-load-theme-hook nil
  "Hook run after flexoki-themes is loaded using `load-theme'.")

(defun flexoki-themes-create (variant theme-name)
  "Define theme with THEME-NAME using VARIANT settings."
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
       (flexoki-themes-bg
	(if (eq variant 'light)
	    flexoki-colour-paper flexoki-colour-black))
       (flexoki-themes-faint-bg
	(if (eq variant 'light)
	    flexoki-colour-100 flexoki-colour-900))
       (flexoki-themes-faint-fg
	(if (eq variant 'light)
	    flexoki-colour-800 flexoki-colour-300))
       (flexoki-themes-fg
	(if (eq variant 'light) "#100f0f" "#fffcf0"))
       ;; accents
       (flexoki-themes-red
	(if (eq variant 'light) "#af3029" "#d14d41"))
       (flexoki-themes-orange
	(if (eq variant 'light) "#bc5215" "#da702c"))
       (flexoki-themes-yellow
	(if (eq variant 'light) "#ad8301" "#d0a215"))
       (flexoki-themes-green
	(if (eq variant 'light) "#66800b" "#879a39"))
       (flexoki-themes-cyan
	(if (eq variant 'light) "#24837b" "#3aa99f"))
       (flexoki-themes-blue
	(if (eq variant 'light) "#205EA6" "#4385be"))
       (flexoki-themes-purple
	(if (eq variant 'light) "#5E409D" "#8b7ec8"))
       (flexoki-themes-magenta
	(if (eq variant 'light) "#a02f6f" "#ce5d97"))
       ;; foreground variants
       (flexoki-themes-lowlight
	(if (eq variant 'light) flexoki-colour-200 flexoki-colour-800))
       (flexoki-themes-highlight
	(if (eq variant 'light) flexoki-colour-300 flexoki-colour-700))
       (flexoki-themes-ultralight
	(if (eq variant 'light) flexoki-colour-500 flexoki-colour-600))
       (flexoki-themes-meek
	(if (eq variant 'light) flexoki-colour-500 flexoki-colour-600)))

    ;; set any extra colours
    (dolist (item flexoki-themes-custom-colours)
      (pcase item
	(`(,cvar . ,val) (set cvar val))))

    (custom-theme-set-faces
     theme-name
     `(default
       ((t (:background ,flexoki-themes-bg
	    :foreground ,flexoki-themes-fg))))
     `(cursor
       ((t (:background ,flexoki-themes-fg))))
     `(fringe
       ((t (:background ,flexoki-themes-bg :weight light))))
     `(hl-line
       ((t (:background ,flexoki-themes-highlight))))
     `(region
       ((t (:background ,flexoki-themes-lowlight))))
     `(secondary-selection
       ((t (:background ,flexoki-themes-highlight))))
     `(buffer-menu-buffer
       ((t (:background ,flexoki-themes-fg))))
     `(minibuffer-prompt
       ((t (:foreground ,flexoki-themes-purple :weight semi-bold))))
     `(vertical-border
       ((t (:foreground ,flexoki-themes-fg))))
     `(internal-border
       ((t (:background ,flexoki-themes-bg
	    :foreground ,flexoki-themes-bg))))
     `(show-paren-match
       ((t
	 (:background ,flexoki-themes-lowlight
	  :foreground ,flexoki-themes-yellow
	  :weight bold))))
     `(show-paren-mismatch
       ((t
	 (:background ,flexoki-themes-ultralight
	  :foreground ,flexoki-themes-red
	  :weight bold
	  :box t))))
     `(link
       ((t
	 (:background ,flexoki-themes-bg
	  :foreground ,flexoki-themes-blue
	  :weight semi-bold
	  :underline t))))
     `(shadow
       ((t (:foreground ,flexoki-themes-ultralight))))

     ;; NOTE: We want the flexoki-themes- colors to be available as faces. It seems like there
     ;; should be a better way to do this but...
     `(flexoki-themes-fg
       ((t (:foreground ,flexoki-themes-fg))))
     `(flexoki-themes-bg
       ((t (:background ,flexoki-themes-bg))))
     `(flexoki-themes-faint-bg
       ((t (:background ,flexoki-themes-faint-bg))))
     `(flexoki-themes-ultralight
       ((t (:background ,flexoki-themes-ultralight))))
     `(flexoki-themes-highlight
       ((t (:foreground ,flexoki-themes-highlight))))
     `(flexoki-themes-lowlight
       ((t (:foreground ,flexoki-themes-lowlight))))
     `(flexoki-themes-meek
       ((t (:background ,flexoki-themes-meek))))
     `(flexoki-themes-blue
       ((t (:foreground ,flexoki-themes-blue))))
     `(flexoki-themes-cyan
       ((t (:foreground ,flexoki-themes-cyan))))
     `(flexoki-themes-green
       ((t (:foreground ,flexoki-themes-green))))
     `(flexoki-themes-magenta
       ((t (:foreground ,flexoki-themes-magenta))))
     `(flexoki-themes-orange
       ((t (:foreground ,flexoki-themes-orange))))
     `(flexoki-themes-purple
       ((t (:foreground ,flexoki-themes-purple))))
     `(flexoki-themes-red
       ((t (:foreground ,flexoki-themes-red))))
     `(flexoki-themes-yellow
       ((t (:foreground ,flexoki-themes-yellow))))

;;;;; Basic faces
     `(error
       ((t (:foreground ,flexoki-themes-red :bold t))))
     `(italic
       ((t (:foreground
	    ,flexoki-themes-faint-fg
	    :underline nil
	    :slant italic))))
     `(success
       ((t (:foreground ,flexoki-themes-green :bold t))))
     `(warning
       ((t (:foreground ,flexoki-themes-yellow :bold t))))
     `(alert-low-face
       ((t (:foreground ,flexoki-themes-orange))))
     `(escape-glyph
       ((t (:foreground ,flexoki-themes-cyan))))
     `(highlight
       ((t (:background ,flexoki-themes-highlight))))
     `(homoglyph
       ((t (:foreground ,flexoki-themes-blue))))
     `(match
       ((t (:foreground ,flexoki-themes-lowlight
	    :background ,flexoki-themes-blue))))

;;;;; built-in syntax (font-lock)
     `(font-lock-builtin-face
       ((t (:foreground
	    ,flexoki-themes-green
	    :weight
	    ,(if flexoki-themes-use-bold-builtins
		 'bold 'light)))))
     `(font-lock-constant-face
       ((t (:foreground ,flexoki-themes-purple :weight light))))
     `(font-lock-comment-face
       ((t (:foreground ,flexoki-themes-meek
	    :slant ,(if flexoki-themes-use-italic-comments
			 'italic 'normal)
	    :weight normal))))
     `(font-lock-function-name-face
       ((t (:foreground ,flexoki-themes-orange))))
     `(font-lock-keyword-face
       ((t (:foreground
	    ,flexoki-themes-magenta
	    :weight
	    ,(if flexoki-themes-use-bold-keywords
		 'bold 'light)))))
     `(font-lock-string-face
       ((t (:foreground ,flexoki-themes-cyan))))
     `(font-lock-variable-name-face
       ((t (:foreground ,flexoki-themes-blue :weight light))))
     `(font-lock-type-face
       ((t (:foreground ,flexoki-themes-yellow :weight medium))))
     `(font-lock-warning-face
       ((t (:foreground ,flexoki-themes-yellow :weight bold))))
     `(font-lock-preprocessor-face
       ((t (:foreground ,flexoki-themes-fg :weight medium))))

;;;;; more built-in faces
;;;;; Buttons
     `(custom-button
       ((t (:foreground
	    ,flexoki-themes-purple
	    :background ,flexoki-themes-faint-bg
	    :box
	    (:line-width
	     (2 . 1)
	     :color nil
	     :style released-button)))))
     `(custom-button-mouse
       ((t (:foreground
	    ,flexoki-themes-purple
	    :background ,flexoki-themes-bg
	    :inherit t
	    :box
	    (:line-width
	     (1 . 1)
	     :color ,flexoki-themes-lowlight)))))
     `(custom-button-pressed
       ((t (:box
	    (:line-width
	     (2 . 1)
	     :color nil
	     :style pressed-button)
	    :foreground ,flexoki-themes-purple
	    :background ,flexoki-themes-lowlight))))

;;;;;; Comint
     `(comint-highlight-prompt
       ((t (:foreground ,flexoki-themes-yellow
	    :weight semi-bold))))

;;;;;; Dired
     `(dired-symlink
       ((t (:foreground ,flexoki-themes-purple :weight bold))))

;;;;;; Eshell
     `(eshell-prompt
       ((t (:foreground ,flexoki-themes-yellow))))
     `(eshell-ls-archive
       ((t (:foreground ,flexoki-themes-meek))))
     `(eshell-ls-backup
       ((t (:foreground ,flexoki-themes-meek))))
     `(eshell-ls-clutter
       ((t (:foreground ,flexoki-themes-orange :weight bold))))
     `(eshell-ls-directory
       ((t (:foreground ,flexoki-themes-blue :weight bold))))
     `(eshell-ls-executable
       ((t (:weight bold))))
     `(eshell-ls-missing
       ((t (:foreground ,flexoki-themes-red :bold t))))
     `(eshell-ls-product
       ((t (:foreground ,flexoki-themes-red))))
     `(eshell-ls-readonly
       ((t (:background ,flexoki-themes-highlight
	    :foreground ,flexoki-themes-meek
	    :weight light))))
     `(eshell-ls-special
       ((t (:foreground ,flexoki-themes-yellow :bold t))))
     `(eshell-ls-symlink
       ((t (:foreground ,flexoki-themes-red))))
     `(eshell-ls-unreadable
       ((t (:foreground ,flexoki-themes-red :bold t))))

;;;;;; Line number
     `(line-number
       ((t (:inherit 'default :foreground ,flexoki-themes-lowlight))))
     `(line-number-current-line
       ((t (:inherit line-number :foreground ,flexoki-themes-purple))))

;;;;;; Modeline
     `(mode-line
       ((t (:foreground ,flexoki-themes-fg
	    :background ,flexoki-themes-faint-bg
	    :box nil))))
     `(mode-line-inactive
       ((t (:foreground ,flexoki-themes-meek
	    :background ,flexoki-themes-bg
	    :box nil))))

;;;;; tab-bar
     `(tab-bar ((t (:height 1.1 :foreground ,flexoki-themes-fg :background ,flexoki-themes-bg))))
     `(tab-bar-tab
       ((t (:background
	    ,flexoki-themes-bg
	    :foreground ,flexoki-themes-yellow
	    :box (:line-width 1 :style released-button)))))
     `(tab-bar-tab-inactive
       ((t (:inherit
	    tab-bar-tab
	    :background ,flexoki-themes-lowlight
	    :foreground ,flexoki-themes-bg))))

;;;;; tab-line
     `(tab-line ((t (:foreground ,flexoki-themes-fg :background ,flexoki-themes-bg))))
     `(tab-line-close-highlight ((t (:foreground ,flexoki-themes-red))))
     `(tab-line-tab
       ((t (:background
	    ,flexoki-themes-bg
	    :foreground ,flexoki-themes-yellow
	    :box (:line-width 1 :style released-button)))))
     `(tab-line-tab-inactive
       ((t (:inherit tab-line-tab
	    :background ,flexoki-themes-lowlight
	    :foreground ,flexoki-themes-bg))))

;;;;;; Term
     `(term
       ((t (:foreground
	    ,flexoki-themes-fg
	    :background ,flexoki-themes-bg))))
     `(term-bold
       ((t  (:weight bold))))
     `(term-color-black
       ((t  (:background
	     ,flexoki-colour-black
	     :foreground ,flexoki-colour-black))))
     `(term-color-red
       ((t  (:background
	     ,flexoki-themes-red
	     :foreground ,flexoki-themes-red))))
     `(term-color-green
       ((t  (:background
	     ,flexoki-themes-green
	     :foreground ,flexoki-themes-green))))
     `(term-color-yellow
       ((t  (:background
	     ,flexoki-themes-yellow
	     :foreground ,flexoki-themes-yellow))))
     `(term-color-blue
       ((t  (:background
	     ,flexoki-themes-blue
	     :foreground ,flexoki-themes-blue))))
     `(term-color-magenta
       ((t  (:background
	     ,flexoki-themes-magenta
	     :foreground ,flexoki-themes-magenta))))
     `(term-color-cyan
       ((t  (:background
	     ,flexoki-themes-cyan
	     :foreground ,flexoki-themes-cyan))))
     `(term-color-white
       ((t  (:background
	     ,flexoki-colour-paper
	     :foreground ,flexoki-colour-paper))))

;;;;;; Vterm
     `(vterm
       ((t (:foreground
	    ,flexoki-themes-fg
	    :background ,flexoki-themes-bg))))
     `(vterm-color-black
       ((t  (:background
	     ,flexoki-colour-black
	     :foreground ,flexoki-colour-black))))
     `(vterm-color-red
       ((t  (:background
	     ,flexoki-themes-red
	     :foreground ,flexoki-themes-red))))
     `(vterm-color-green
       ((t  (:background
	     ,flexoki-themes-green
	     :foreground ,flexoki-themes-green))))
     `(vterm-color-yellow
       ((t  (:background
	     ,flexoki-themes-yellow
	     :foreground ,flexoki-themes-yellow))))
     `(vterm-color-blue
       ((t  (:background
	     ,flexoki-themes-blue
	     :foreground ,flexoki-themes-blue))))
     `(vterm-color-magenta
       ((t  (:background
	     ,flexoki-themes-magenta
	     :foreground ,flexoki-themes-magenta))))
     `(vterm-color-cyan
       ((t  (:background
	     ,flexoki-themes-cyan
	     :foreground ,flexoki-themes-cyan))))
     `(vterm-color-white
       ((t  (:background
	     ,flexoki-colour-paper
	     :foreground ,flexoki-colour-paper))))

;;;;; Childframes
;;;;;; Mini-Frame
     `(mini-popup-background ((t (:background ,flexoki-themes-faint-bg))))
     `(mini-popup-border     ((t (:background ,flexoki-themes-faint-bg))))

;;;;;; Mini-Popup (Childframe)
     `(mini-popup-background ((t (:background ,flexoki-themes-faint-bg))))
     `(mini-popup-border     ((t (:background ,flexoki-themes-faint-bg))))

;;;;;; Posframe
     `(which-key-posframe
       ((t (:background ,flexoki-themes-faint-bg))))
     `(which-key-posframe-border
       ((t (:background ,flexoki-themes-faint-bg))))
     `(transient-posframe-border
       ((t (:background ,flexoki-themes-faint-bg))))
     `(transient-posframe
       ((t (:foreground ,flexoki-themes-highlight
	    :background ,flexoki-themes-faint-bg))))

;;;;; Completion/Narrowing
;;;;;; General Completion
     `(completions-annotations
       ((t (:foreground ,flexoki-themes-meek))))

;;;;;; Company-mode
     `(company-scrollbar-bg
       ((t (:background ,flexoki-themes-faint-bg))))
     `(company-scrollbar-fg
       ((t (:background ,flexoki-themes-meek))))
     `(company-tooltip
       ((t (:background ,flexoki-themes-meek))))
     `(company-tooltip-annotation
       ((t (:foreground ,flexoki-themes-green))))
     `(company-tooltip-annotation-selection
       ((t (:inherit    company-tooltip-annotation))))
     `(company-tooltip-selection
       ((t (:foreground ,flexoki-themes-purple
	    :background ,flexoki-themes-faint-bg))))
     `(company-tooltip-common
       ((t (:foreground ,flexoki-themes-blue :underline t))))
     `(company-tooltip-common-selection
       ((t (:foreground ,flexoki-themes-blue :underline t))))
     `(company-preview-common
       ((t (:foreground ,flexoki-themes-highlight))))
     `(company-preview
       ((t (:background ,flexoki-themes-blue))))
     `(company-preview-search
       ((t (:background ,flexoki-themes-cyan))))
     `(company-template-field
       ((t (:foreground ,flexoki-colour-black
	    :background ,flexoki-themes-yellow))))
     `(company-echo-common
       ((t (:foreground ,flexoki-themes-red))))

;;;;;; Corfu
     `(corfu-annotations
       ((t (:foreground ,flexoki-themes-meek))))
     `(corfu-bar
       ((t (:foreground ,flexoki-themes-ultralight))))
     `(corfu-border
       ((t (:foreground ,flexoki-themes-faint-bg))))
     `(corfu-current
       ((t (:foreground ,flexoki-themes-yellow
	    :background ,flexoki-themes-lowlight))))
     `(corfu-default
       ((t (:inherit default :background ,flexoki-themes-faint-bg))))
     `(corfu-deprecated
       ((t (:foreground ,flexoki-themes-highlight))))
     `(corfu-echo
       ((t (:inherit default))))

;;;;;; Isearch
     `(isearch
       ((t (:foreground ,flexoki-colour-paper
	    :background ,flexoki-themes-purple
	    :weight bold))))
     `(isearch-fail
       ((t (:background ,flexoki-themes-red))))
     `(isearch-group-1
       ((t (:background ,flexoki-themes-magenta))))
     `(isearch-group-2
       ((t (:background ,flexoki-themes-yellow))))
     `(lazy-highlight
       ((t (:foreground ,flexoki-themes-purple :background ,flexoki-themes-lowlight))))

;;;;;; Vertico
     `(vertico-current
       ((t (:weight bold :background ,flexoki-themes-lowlight))))
     `(vertico-group-separator
       ((t (:foreground ,flexoki-themes-ultralight :strike-through t))))
     `(vertico-multiline
       ((t (:foreground ,flexoki-themes-meek))))
     `(vertico-group-title
       ((t (:foreground ,flexoki-themes-meek))))

;;;;; Diffs & VC

;;;;;; Diff
     `(diff-header
       ((t (:foreground ,flexoki-themes-fg))))
     `(diff-file-header
       ((t (:foreground ,flexoki-themes-fg))))
     `(diff-hunk-header
       ((t (:foreground ,flexoki-themes-fg))))
     `(diff-context
       ((t (:background ,flexoki-themes-lowlight))))
     `(diff-changed
       ((t (:background unspecified
	    :foreground ,flexoki-themes-blue))))
     `(diff-refine-changed
       ((t (:foreground ,flexoki-themes-blue))))
     `(diff-added
       ((t (:background unspecified
	    :foreground ,flexoki-themes-green))))
     `(diff-refine-added
       ((t (:background unspecified
	    :foreground ,flexoki-themes-green))))
     `(diff-removed
       ((t (:background unspecified
	    :foreground ,flexoki-themes-red))))
     `(diff-refine-removed
       ((t (:background unspecified
	    :foreground ,flexoki-themes-meek
	    :strike-through t))))
     `(diff-indicator-changed
       ((t (:inherit diff-changed))))
     `(diff-indicator-added
       ((t (:inherit diff-added))))
     `(diff-indicator-removed
       ((t (:inherit diff-removed))))

;;;;;; Diff-hl
     `(diff-hl-change ((t (:inherit default
			   :foreground ,flexoki-themes-blue ))))
     `(diff-hl-delete ((t (:inherit default
			   :foreground ,flexoki-themes-red  ))))
     `(diff-hl-insert ((t (:inherit default
			   :foreground ,flexoki-themes-green))))

;;;;;; Ediff
     `(ediff-even-diff-A
       ((t (:background ,flexoki-themes-lowlight))))
     `(ediff-even-diff-B
       ((t (:background ,flexoki-themes-lowlight))))
     `(ediff-even-diff-C
       ((t (:background ,flexoki-themes-lowlight))))
     `(ediff-even-diff-Ancestor
       ((t (:background ,flexoki-themes-lowlight))))
     `(ediff-odd-diff-A
       ((t (:background ,flexoki-themes-faint-bg))))
     `(ediff-odd-diff-B
       ((t (:background ,flexoki-themes-faint-bg))))
     `(ediff-odd-diff-C
       ((t (:background ,flexoki-themes-faint-bg))))
     `(ediff-odd-diff-Ancestor
       ((t (:background ,flexoki-themes-faint-bg))))

;;;;;; Magit
     `(magit-branch-local
       ((t (:foreground ,flexoki-themes-purple))))
     `(magit-branch-remote
       ((t (:foreground ,flexoki-themes-cyan))))
     `(git-commit-summary
       ((t (:foreground ,flexoki-themes-green))))
     `(git-commit-overlong-summary
       ((t (:foreground ,flexoki-themes-red :weight semi-bold))))
     `(magit-dimmed
       ((t (:foreground ,flexoki-themes-meek))))
     `(magit-blame-dimmed
       ((t (:foreground ,flexoki-themes-meek))))
     `(magit-header-line
       ((t (:foreground ,flexoki-themes-fg
	    :background ,flexoki-themes-highlight))))
     `(magit-header-line-log-select
       ((t (:foreground ,flexoki-themes-fg
	    :background ,flexoki-themes-highlight))))
     `(magit-section-heading
       ((t (:foreground ,flexoki-themes-meek :height 1.2))))
     `(magit-section-highlight
       ((t (:background ,flexoki-themes-lowlight :extend t))))

;;;;; light-weight markup
;;;;;; Outline
     `(outline-minor-0
       ((t (:background ,flexoki-themes-lowlight :height 1.1))))
     `(outline-1
       ((t (:inherit 'default
	    :foreground ,flexoki-themes-blue
	    :weight semi-bold))))
     `(outline-2
       ((t (:inherit 'default
	    :foreground ,flexoki-themes-purple
	    :weight semi-bold))))
     `(outline-3
       ((t (:inherit 'default
	    :foreground ,flexoki-themes-orange
	    :weight semi-bold))))
     `(outline-4
       ((t (:inherit 'default
	    :foreground ,flexoki-themes-magenta
	    :weight semi-bold))))
     `(outline-5
       ((t (:inherit 'default
	    :foreground ,flexoki-themes-cyan
	    :weight semi-bold))))
     `(outline-6
       ((t (:inherit outline-1))))
     `(outline-7
       ((t (:inherit outline-2))))
     `(outline-8
       ((t (:inherit outline-3))))

;;;;;; Markdown-mode
     `(markdown-header-delimiter-face
       ((t (:foreground ,flexoki-themes-highlight
	    :weight semi-bold))))
     `(markdown-header-face-1
       ((t (:inherit outline-1))))
     `(markdown-header-face-2
       ((t (:inherit outline-2))))
     `(markdown-header-face-3
       ((t (:inherit outline-3))))
     `(markdown-header-face-4
       ((t (:inherit outline-4))))
     `(markdown-header-face-5
       ((t (:inherit outline-5))))
     `(markdown-header-face-6
       ((t (:inherit outline-6))))
     `(markdown-url-face
       ((t (:foreground ,flexoki-themes-cyan))))
     `(markdown-code-face
       ((t (:foreground ,flexoki-themes-purple
	    :background ,flexoki-themes-faint-bg
	    :extend t))))
     `(markdown-inline-code-face
       ((t (:foreground ,flexoki-themes-purple))))
     `(markdown-footnote-marker-face
       ((t (:foreground ,flexoki-themes-meek))))
     `(markdown-list-face
       ((t (:foreground ,flexoki-themes-meek))))
     `(markdown-markup-face
       ((t (:foreground ,flexoki-themes-lowlight))))
     `(markdown-italic-face
       ((t (:foreground ,flexoki-themes-orange))))
     `(markdown-html-tag-delimiter-face
       ((t (:inherit 'default))))

;;;;;; Org-mode
     `(org-block
       ((t (:inherit 'default :background ,flexoki-themes-faint-bg))))
     `(org-code
       ((t (:foreground ,flexoki-themes-purple))))
     `(org-date
       ((t (:foreground ,flexoki-themes-green :underline t ))))
     `(org-drawer
       ((t (:foreground ,flexoki-themes-yellow))))
     `(org-todo
       ((t (:inherit outline-1 :foreground ,flexoki-themes-red))))
     `(org-done
       ((t (:inherit outline-1 :foreground ,flexoki-themes-lowlight))))
     `(org-headline-done
       ((t (:foreground ,flexoki-themes-lowlight))))
     `(org-checkbox
       ((t (:foreground ,flexoki-themes-green :weight semi-bold))))
     `(org-table
       ((t (:foreground ,flexoki-themes-purple))))
     `(org-document-info
       ((t (:foreground ,flexoki-themes-cyan))))
     `(org-document-title
       ((t (:inherit org-document-info :weight bold))))

;;;;; Other Packages
;;;;;; Neotree
     `(neo-banner-face
       ((t (:foreground ,flexoki-themes-magenta))))
     `(neo-dir-link-face
       ((t (:foreground ,flexoki-themes-blue :weight bold))))
     `(neo-root-dir-face
       ((t (:foreground ,flexoki-themes-cyan :weight bold))))
     `(neo-vc-added-face
       ((t (:foreground ,flexoki-themes-green))))
    `(neo-vc-edited-face
       ((t (:foreground ,flexoki-themes-orange))))
    `(neo-vc-missing-face
       ((t (:foreground ,flexoki-themes-red))))

;;;;;; Rainbow delimiters
     `(rainbow-delimiters-depth-1-face
       ((t (:foreground ,flexoki-themes-blue))))
     `(rainbow-delimiters-depth-2-face
       ((t (:foreground ,flexoki-themes-orange))))
     `(rainbow-delimiters-depth-3-face
       ((t (:foreground ,flexoki-themes-purple))))
     `(rainbow-delimiters-depth-4-face
       ((t (:foreground ,flexoki-themes-yellow))))
     `(rainbow-delimiters-depth-5-face
       ((t (:foreground ,flexoki-themes-cyan))))
     `(rainbow-delimiters-depth-6-face
       ((t (:foreground ,flexoki-themes-magenta))))
     `(rainbow-delimiters-depth-7-face
       ((t (:inherit rainbow-delimiters-depth-1-face))))
     `(rainbow-delimiters-depth-8-face
       ((t (:inherit rainbow-delimiters-depth-2-face))))
     `(rainbow-delimiters-depth-9-face
       ((t (:inherit rainbow-delimiters-depth-3-face))))
     `(rainbow-delimiters-depth-10-face
       ((t (:inherit rainbow-delimiters-depth-4-face))))
     `(rainbow-delimiters-depth-11-face
       ((t (:inherit rainbow-delimiters-depth-5-face))))
     `(rainbow-delimiters-depth-12-face
       ((t (:inherit rainbow-delimiters-depth-6-face))))
     `(rainbow-delimiters-unmatched-face
       ((t (:background ,flexoki-themes-bg
	    :foreground ,flexoki-themes-red
	    :weight bold))))

;;;;; Programming modes
;;;;;; js2-mode
     `(js2-function-call
       ((t (:foreground ,flexoki-themes-green))))
     `(js2-object-property
       ((t (:foreground ,flexoki-themes-purple))))
     `(js2-object-property-access
       ((t (:inherit js2-object-property))))

;;;;;; Shell script
     `(sh-quoted-exec
       ((t (:foreground ,flexoki-themes-purple))))
     `(sh-heredoc
       ((t (:foreground ,flexoki-themes-orange))))

;;;;;; uiua-mode
     `(uiua-number
       ((t (:foreground ,flexoki-themes-orange))))
     `(uiua-monadic-function
       ((t (:foreground ,flexoki-themes-green))))
     `(uiua-noadic-or-constant
       ((t (:foreground ,flexoki-themes-red))))
     `(uiua-ocean-function
       ((t (:weight semi-bold))))

;;;;;; web-mode
     `(web-mode-doctype-face ((t (:foreground ,flexoki-themes-orange))))
     `(web-mode-function-call-face
       ((t (:inherit font-lock-function-name-face :weight bold))))
     `(web-mode-function-call-face
       ((t (:inherit font-lock-function-name-face))))
     `(web-mode-string-face ((t (:foreground ,flexoki-themes-cyan))))
     `(web-mode-html-tag-face ((t (:foreground ,flexoki-themes-orange))))
     `(web-mode-html-tag-bracket-face ((t (:foreground ,flexoki-themes-magenta))))
     `(web-mode-html-attr-name-face ((t (:foreground ,flexoki-themes-orange))))
     `(web-mode-json-key-face
       ((t (:inherit font-lock-keyword-face :weight light))))
     `(web-mode-json-context-face ((t (:inherit font-lock-keyword-face)))))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'flexoki-themes)

;;; flexoki-themes.el ends here
