;;; art-parts.el --- Module to handle other modules -*- lexical-binding: t; -*-

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
(defgroup art-parts ()
  "Customise group for the `art-parts' Library."
  :group 'art)

(defcustom art-path-path
  (expand-file-name "libraries/art-parts.el" user-emacs-directory)
  "Variable for the path of the module `art-parts'."
  :type 'file
  :group 'art-parts)

;;; PACKAGES TO BE INSTALLED
(defcustom art-parts-package-list
  '(ace-window
    ag
    anzu
    company
    counsel
    doom-themes
    elixir-mode
    exec-path-from-shell
    expand-region
    flycheck
    git-gutter
    howm
    hydra
    json-mode
    lsp-mode
    magit
    markdown-mode
    projectile
    rbenv
    ruby-end
    ruby-mode
    s
    tree-sitter
    tree-sitter-langs
    web-mode
    yasnippet
    zig-mode)
  "List of packages to be installed for the Emacs config to work as configured.
The packages will be installed with `art-parts-install-packages'."
  :type '(repeat symbol)
  :group 'art)

(defcustom art-parts-outdated-packages (package--upgradeable-packages)
  "List of Packages that are available to Upgrade."
  :type '(repeat symbol)
  :group 'art-parts)

;;; SETUP PACKAGE INSTALLATION
(defun art-parts-install-packages ()
  "Check if all packages in `art-parts-package-list' are installed.
Install the missing packages and if not."
  (interactive)
  (let ((missing-packages
         (cl-remove-if #'package-installed-p art-parts-package-list)))
    (when missing-packages
      ;; check for new packages (package versions)
      (message "%s" "Reloading packages DB...")
      (package-refresh-contents)
      (message "%s" " done.")
      ;; install the missing packages
      (mapc #'package-install missing-packages))))

(defmacro art-parts-require (&rest modules)
  "Load MODULES, if they are not already loaded and are installed.
Skips any modules that are not found.
Displays a message in the minibuffer for any modules that are not found.
Displays a message in the minibuffer for any modules that are already loaded."

  `(progn
     ,@(mapcar (lambda (module)
                 `(when (not (featurep ',module))
                    (if (locate-library ,(symbol-name module))
                        (progn
                          (require ',module)
                          (message "Loaded module '%s'" ',module))
                      (message "Module '%s' not found" ',module))))
               modules)))

(defun art-parts-load ()
  (interactive)
  (require 'art-base)
  (require 'art-edit)
  (require 'art-faces)
  (require 'art-git)
  (require 'art-modeline)
  (require 'art-notes)
  (require 'art-programming)
  (require 'art-uxui))


(provide 'art-parts)
;;; art-parts.el ends here
