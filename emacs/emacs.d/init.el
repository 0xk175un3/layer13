;;; init.el --- Custom settings -*- lexical-binding: t -*-

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

;;; Commentary:
;; `init' to load my settings into Emacs.
;; This includes declarations of `package-archives', installation of the
;; packages and the loading of custom modules with `art-parts-load'.

;;; Code:


;;; PATHS
(add-to-list 'load-path art-lib-path)
(add-to-list 'load-path (expand-file-name "local/packages" user-emacs-directory))
(add-to-list 'load-path "~/.emacs.d/lib")

(customize-set-variable 'custom-file
                        (expand-file-name "custom.el" user-emacs-directory))

;;; SETUP PACKAGE SOURCES
(customize-set-variable 'package-enable-at-startup nil)

(pcase system-type
  ('windows-nt
   (customize-set-variable 'package-check-signature nil)))

(customize-set-variable 'package-archives
                        '(("melpa" . "https://melpa.org/packages/")
                          ("gnu" . "https://elpa.gnu.org/packages/")
                          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(customize-set-variable 'package-archive-priorities
                        '(("melpa" . 9)
                          ("gnu" . 6)
                          ("nongnu" . 2)))

(package-initialize)
;; get path variables
(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))
(require 'exec-path-from-shell)
;; disable M-o for html-mode
(require 'sgml-mode)
(define-key html-mode-map (kbd "M-o") nil)

;;; LOAD THE CONFIG
(toggle-debug-on-error)
;; run package installation
(require 'art-parts)
(art-parts-install-packages)

;; Because: This variable is used by `package-autoremove' to decide.
(customize-set-variable 'package-selected-packages art-parts-package-list)

;; load custom libraries
(let ((default-directory user-emacs-directory)
      (file-name-handler-alist nil)
      (gc-cons-percentage .6)
      (gc-cons-threshold most-positive-fixnum)
      (read-process-output-max (* 1024 1024)))
  (art-parts-load)
  (garbage-collect))

(toggle-debug-on-error)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s" (emacs-init-time))))


;;; init.el ends here
