;;; art-git.el --- Config for Git -*- lexical-binding: t; -*-

;; Author: Arto Levi
;; Version: 1.1
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
(defgroup art-git ()
  "Customise group for the `art-git' Library."
  :group 'art)

(defcustom art-git-path
  (expand-file-name "libraries/art-git.el" user-emacs-directory)
  "Variable for the path of the module `art-git'."
  :type 'file
  :group 'art-git)

;;; LIBRARIES
(require 'git-gutter)
(require 'magit)

;;; KEYBINDINGS
;;;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;;; ENABLE MODES
(global-git-gutter-mode)


(provide 'art-git)
;;; art-git.el ends here
