;;; art-notes.el --- Config for writing notes -*- lexical-binding: t; -*-

;; Author: Arto Levi
;; Version: 1.2
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
(defgroup art-notes ()
  "Customise group for the `art-notes' Library."
  :group 'art)

(defcustom art-notes-path
  (expand-file-name "libraries/art-notes.el" user-emacs-directory)
  "Variable for the path of the module `art-notes'."
  :type 'file
  :group 'art-notes)

;;; LIBRARIES
(require 'howm)
(require 'markdown-mode)

;;; CONFIGURATION SETTINGS
;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(setq markdown-command "multimarkdown")
(setq markdown-split-window-direction 'right)
(setq markdown-live-preview-delete-export 'delete-on-export)
(setq markdown-make-gfm-checkboxes-buttons 't)
;; Directory configuration
(setq howm-home-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Celestial Library")
(setq howm-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Celestial Library")
(setq howm-keyword-file (expand-file-name ".howm-keys" howm-home-directory))
(setq howm-history-file (expand-file-name ".howm-history" howm-home-directory))
(setq howm-file-name-format "%Y%m%Y%m%d%H%M%S.md")
;; Use ripgrep as grep
(setq howm-view-use-grep t)
(setq howm-view-grep-command "rg")
(setq howm-view-grep-option "-nH --no-heading --color never")
(setq howm-view-grep-extended-option nil)
(setq howm-view-grep-fixed-option "-F")
(setq howm-view-grep-expr-option nil)
(setq howm-view-grep-file-stdin-option nil)

;;; FUNCTIONS
(defun howm-list--counsel-rg (match)
  (if (string= match "")
  (howm-list-all)
(if (or (null ivy--old-cands)
	(equal ivy--old-cands '("No matches found")))
        (message "No match")
  (let ((howm-view-use-grep
	 #'(lambda (str file-list &optional fixed-p force-case-fold)
                 (mapcar
                  (lambda (cand)
		(if (string-match "\\`\\(.*\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
                        (let ((file (match-string-no-properties 1 cand))
			  (line (match-string-no-properties 2 cand))
			  (match-line (match-string-no-properties 3 cand)))
                          (list (expand-file-name file howm-directory)
                                (string-to-number line)
                                match-line))))
                  ivy--old-cands))))
        (howm-search ivy--old-re t)
        (riffle-set-place
     (1+ (cl-position match ivy--old-cands :test 'string=)))))))

(defun howm-counsel-rg ()
  "Interactively grep for a string in your howm notes using rg."
  (interactive)
  (let ((default-directory howm-directory)
        (counsel-ag-base-command counsel-rg-base-command)
        (counsel-ag-command (counsel--format-ag-command "--glob=!*~" "%s")))
    (ivy-read "Search all (rg): "
	      #'counsel-ag-function
	      :dynamic-collection t
	      :keymap counsel-ag-map
	      :action #'howm-list--counsel-rg
	      :require-match t
	      :caller 'counsel-rg)))

;;; KEYBINDINGS
(define-key global-map (concat howm-prefix "r") 'howm-counsel-rg)
(define-key howm-menu-mode-map "\C-h" nil)
(define-key riffle-summary-mode-map "\C-h" nil)
(define-key howm-view-contents-mode-map "\C-h" nil)
(define-key markdown-mode-map (kbd "C-c C-e") 'markdown-do)
(define-key html-mode-map (kbd "M-o") nil)

;;; ADVICE HACKS
;; Default recent to sorting by mtime
(advice-add 'howm-list-recent :after #'howm-view-sort-by-mtime)
;; Default all to sorting by creation, newest first
(advice-add 'howm-list-all :after #'(lambda () (howm-view-sort-by-date t)))

;;; HOOKS
;; Rename buffers to their title
(add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
(add-hook 'after-save-hook 'howm-mode-set-buffer-name)

;;; ENABLE MODES
(add-hook 'howm-mode-hook 'markdown-mode)


(provide 'art-notes)
;;; art-notes.el ends here
