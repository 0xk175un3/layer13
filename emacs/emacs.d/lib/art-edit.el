;;; art-edit.el --- Config for text manipulations -*- lexical-binding: t; -*-

;; Author: Arto Levi
;; Version: 1.3
;; Package-Requires: ((emacs "30.0.50"))
;; Created: 2023-12-12
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
(defgroup art-edit ()
  "Customise group for the `art-edit' Library."
  :group 'art)

(defcustom art-edit-path
  (expand-file-name "libraries/art-edit.el" user-emacs-directory)
  "Variable for the path of the module `art-edit'."
  :type 'file
  :group 'art-edit)

;;; LIBRARIES
(require 'anzu)
(require 'expand-region)
(require 'company)

;;; CONFIGURATION SETTINGS
;;;; Defaults
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
;;;; Expand-region
(autoload 'er/expand-region "expand-region" nil t)
;;;; Company
(defun my-tab ()
  (interactive)
      (company-indent-or-complete-common nil))

(setq company-format-margin-function nil)
(setq company-require-match nil)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.2)
(setq company-tooltip-align-annotation t)
(setq company-frontends '(company-pseudo-tooltip-frontend
                          company-echo-metadata-frontend))

;;; KEYBINDINGS
;;;; Anzu
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-c r") 'anzu-query-replace-regexp)
;;;; Expand-region
(global-set-key (kbd "C-c i") 'er/expand-region)
;;;; Company
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<tab>") 'my-tab)
  (define-key company-active-map (kbd "TAB") 'my-tab)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-mode-map (kbd "<tab>") 'my-tab)
  (define-key company-mode-map (kbd "TAB") 'my-tab))

;;; HOOKS
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'comint-mode-hook 'company-mode)

;;; ENABLE MODES
(delete-selection-mode 1)
(global-anzu-mode 1)


(provide 'art-edit)
;;; art-edit.el ends here
