;;; art-faces.el --- Config for custom faces -*- lexical-binding: t; -*-

;; Author: Arto Levi
;; Version: 1.1
;; Package-Requires: ((emacs "30.0.50"))
;; Created: 2023-12-18
;; This file is NOT part of GNU Emacs.

;; The MIT License (MIT)
;;
;; Copyright (c) 2023 Arto Levi
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

;;; DEFAULTS
(defgroup art-faces ()
  "Customise group for the `art-faces' Library."
  :group 'art)

(defcustom art-faces-path
  (expand-file-name "libraries/art-faces.el" user-emacs-directory)
  "Variable for the path of the module `art-faces'."
  :type 'file
  :group 'art-faces)

;;; CUSTOMS
(defcustom art-font-family-monospaced "Roboto Mono"
  "Name of the font-family to use for art modeline."
  :group 'art-faces
  :type 'string)

(defcustom art-font-family-proportional nil
  "Font to use for variable pitch faces.
Setting this allows art to display variable pitch faces when,
for instance, 'variable-pitch-mode' or 'mixed-pitch-mode' is active in a buffer.
Defaults to nil."
  :group 'art-faces
  :type 'string)

(defcustom art-font-size 14
  "Default value for the font size of art-theme in pt units."
  :group 'art-faces
  :type 'integer)

;;; FACES
(defface art-face-default nil
  "Default face is used for regular information."
  :group 'art-faces)

(defface art-face-variable-pitch nil
  "Default variable-pitch face is used for variable pitch mode."
  :group 'art-faces)

(defface art-face-critical nil
  "Critical face is for information that requires immediate action."
  :group 'art-faces)

(defface art-face-popout nil
  "Popout face is used for information that needs attention."
  :group 'art-faces)

(defface art-face-strong nil
  "Strong face is used for information of a structural nature."
  :group 'art-faces)

(defface art-face-salient nil
  "Salient face is used for information that are important."
  :group 'art-faces)

(defface art-face-faded nil
  "Faded face is for information that are less important."
  :group 'art-faces)

(defface art-face-subtle nil
  "Subtle face is used to suggest a physical area on the screen."
  :group 'art-faces)

(defface art-face-header-default nil
  "Default face for ther header line."
  :group 'art-faces)

(defface art-face-header-critical nil
  "Critical face for ther header line."
  :group 'art-faces)

(defface art-face-header-popout nil
  "Popout face for ther header line."
  :group 'art-faces)

(defface art-face-header-strong nil
  "Strong face for ther header line."
  :group 'art-faces)

(defface art-face-header-salient nil
  "Salient face for ther header line."
  :group 'art-faces)

(defface art-face-header-faded nil
  "Faded face for ther header line."
  :group 'art-faces)

(defface art-face-header-subtle nil
  "Subtle face for ther header line."
  :group 'art-faces)

(defface art-face-header-highlight nil
  "Highlight face for ther header line."
  :group 'art-faces)

(defface art-face-header-separator nil
  "Face for separating item in the header line (internal use)"
  :group 'art-faces)

(defface art-face-header-filler nil
  "Face compsenting spaces in the header line (internal use) "
  :group 'art-faces)

(defface art-face-tag-default nil
  "Default face for tags"
  :group 'art-faces)

(defface art-face-tag-faded nil
  "Faded face for tags"
  :group 'art-faces)

(defface art-face-tag-strong nil
  "Strong face for tags"
  :group 'art-faces)

(defface art-face-tag-salient nil
  "Salient face for tags"
  :group 'art-faces)

(defface art-face-tag-popout nil
  "Popout face for tags"
  :group 'art-faces)

(defface art-face-tag-critical nil
  "Critical face for tags"
  :group 'art-faces)

;;; FUNCTIONS
(defun enable-art-faces ()
  "Derive face attributes for art-faces using art-theme values."
  (set-face-attribute 'art-face-default nil
                      :foreground art-color-foreground
                      :background art-color-background
                      :family     art-font-family-monospaced
                      :height       (* art-font-size 10))
  (set-face-attribute 'art-face-critical nil
                      :foreground art-color-foreground
                      :background art-color-critical)
  (set-face-attribute 'art-face-popout nil
                      :foreground art-color-popout)

  (set-face-attribute 'art-face-variable-pitch nil
                          :foreground (face-foreground 'art-face-default)
                          :background (face-background 'art-face-default)
                          :family art-font-family-proportional
                          :height (* art-font-size 10))
  (if (display-graphic-p)
      (set-face-attribute 'art-face-strong nil
                          :foreground art-color-strong
                          :weight 'medium)
    (set-face-attribute 'art-face-strong nil
                        :foreground art-color-strong
                        :weight 'bold))

  (set-face-attribute 'art-face-salient nil
                      :foreground art-color-salient
                      :weight 'light)

  (set-face-attribute 'art-face-faded nil
                      :foreground art-color-faded
                      :weight 'light)

  (set-face-attribute 'art-face-subtle nil
                      :background art-color-subtle)

  (set-face-attribute 'art-face-header-default nil
                      :foreground art-color-foreground
                      :background art-color-subtle
                      :box `(:line-width 1
                                         :color ,art-color-background
                                         :style nil))

  (set-face-attribute 'art-face-tag-default nil
                      :foreground art-color-foreground
                      :background art-color-background
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 art-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,art-color-foreground
                                         :style nil))

  (set-face-attribute 'art-face-header-strong nil
                      :foreground art-color-strong
                      :background art-color-subtle
                      :inherit 'art-face-strong
                      :box `(:line-width 1
                                         :color ,art-color-background
                                         :style nil))

  (set-face-attribute 'art-face-tag-strong nil
                      :foreground art-color-strong
                      :background art-color-subtle
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 art-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,art-color-strong
                                         :style nil))

  (set-face-attribute 'art-face-header-salient nil
                      :foreground art-color-background
                      :background art-color-salient
                      :box `(:line-width 1
                                         :color ,art-color-background
                                         :style nil))

  (set-face-attribute 'art-face-tag-salient nil
                      :foreground art-color-background
                      :background art-color-salient
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 art-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,art-color-salient
                                         :style nil))

  (set-face-attribute 'art-face-header-popout nil
                      :foreground art-color-background
                      :background art-color-popout
                      :box `(:line-width 1
                                         :color ,art-color-background
                                         :style nil))

  (set-face-attribute 'art-face-tag-popout nil
                      :foreground art-color-background
                      :background art-color-popout
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 art-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,art-color-popout
                                         :style nil))

  (set-face-attribute 'art-face-header-faded nil
                      :foreground art-color-background
                      :background art-color-faded
                      :box `(:line-width 1
                                         :color ,art-color-background
                                         :style nil))

  (set-face-attribute 'art-face-tag-faded nil
                      :foreground art-color-background
                      :background art-color-faded
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 art-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,art-color-faded
                                         :style nil))

  (set-face-attribute 'art-face-header-subtle nil)

  (set-face-attribute 'art-face-header-critical nil
                      :foreground art-color-background
                      :background art-color-critical
                      :box `(:line-width 1
                                         :color ,art-color-background
                                         :style nil))
  (set-face-attribute 'art-face-tag-critical nil
                      :foreground art-color-background
                      :background art-color-critical
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 art-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,art-color-critical
                                         :style nil))

  (set-face-attribute 'art-face-header-separator nil
                      :inherit 'art-face-default
                      :height 0.1)
  (set-face-attribute 'art-face-header-filler nil
                      :inherit 'art-face-header-default
                      :height 0.1)
  (set-face-attribute 'art-face-header-highlight nil
                      :inherit 'art-face-header-faded
                      :box nil))

(defun enable-art-theme ()
  "Apply dark Art theme base."
  ;; Colors from Nord theme at https://www.nordtheme.com
  (setq frame-background-mode     'dark)
  (setq art-color-foreground "#ECEFF4") ;; Snow Storm 3  / nord  6
  (setq art-color-background "#2E3440") ;; Polar Night 0 / nord  0
  (setq art-color-highlight  "#3B4252") ;; Polar Night 1 / nord  1
  (setq art-color-critical   "#EBCB8B") ;; Aurora        / nord 11
  (setq art-color-salient    "#81A1C1") ;; Frost         / nord  9
  (setq art-color-strong     "#ECEFF4") ;; Snow Storm 3  / nord  6
  (setq art-color-popout     "#D08770") ;; Aurora        / nord 12
  (setq art-color-subtle     "#434C5E") ;; Polar Night 2 / nord  2
  (setq art-color-faded      "#677691"))

(enable-art-theme)
(enable-art-faces)


(provide 'art-faces)
;;; art-faces.el ends here
