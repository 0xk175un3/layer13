;;; art-base.el --- Defaults for Emacs configuration -*- lexical-binding: t; -*-

;; Author: Arto Levi
;; Version: 1.2
;; Package-Requires: ((emacs "30.0.50"))
;; Created: 2023-12-12
;; Homepage:
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


(require 'server)
(require 'transient)

;;; DEFAULTS
(defgroup art-base ()
  "Customise group for the `art-base' Library."
  :group 'art)

(defcustom art-base-path
  (expand-file-name "libraries/art-base.el" user-emacs-directory)
  "Variable for the path of the module `art-base'."
  :type 'file
  :group 'art-base)

(defcustom art-base-interactive-functions nil
  "Variable to store all custom interactive functions in the configuration."
  :type '(repeat symbol)
  :group 'art-base)

(defcustom art-base-non-interactive-functions nil
  "Variable to store all custom non interactive functions in the configuration."
  :type '(repeat symbol)
  :group 'art-base)

;;; FUNCTIONS
(defmacro art-base-setv (&rest args)
  "Handle ARGS like `setq' using `customize-set-variable'."
  (let (body)
    (while args
      (let* ((var (pop args)) (val (pop args)))
        (push `(customize-set-variable ',var ,val) body)))
    (macroexp-progn (nreverse body))))

(defun art-base-disable-yes-or-no-p (orig-fun &rest args)
  (advice-add 'yes-or-no-p :around (lambda (&rest _) t))
  (advice-add 'y-or-n-p :around (lambda (&rest _) t))
  (unwind-protect
      (apply orig-fun args)
    (advice-remove 'yes-or-no-p (lambda (&rest _) t))
    (advice-remove 'y-or-n-p (lambda (&rest _) t))))

(defun art-base-async-shell-command-no-window (command)
  (interactive)
  (let ((display-buffer-alist
         (list (cons
                "\\*Async Shell Command\\*.*"
                (cons #'display-buffer-no-window nil)))))
    (async-shell-command command)))

(defun art-base-load-init-files ()
  "Load `early-init.el' & `init.el' file."
  (interactive)
  (progn
    (load-file early-init-file)
    (load-file user-init-file)))

(defun open-icloud-in-dired ()
  "Open iCloud directory in Dired."
  (interactive)
  (dired "~/Library/Mobile Documents/com~apple~CloudDocs/"))

;;; ENVIRONMENT
(pcase system-type
  ((or 'darwin 'gnu/linux)
   (exec-path-from-shell-initialize)
   (exec-path-from-shell-copy-envs '("PATH"))))

;;; VARIABLES
(customize-set-variable 'art-base-interactive-functions
                        (cl-remove-if-not
                         (lambda (symbol)
                           (and (fboundp symbol)
                                (commandp symbol)
                                (string-prefix-p "art-" (symbol-name symbol))))
                         (mapcar #'intern (all-completions "art-" obarray))))

(customize-set-variable 'art-base-non-interactive-functions
                        (cl-remove-if-not
                         (lambda (symbol)
                           (and (fboundp symbol)
                                (not (commandp symbol))
                                (string-prefix-p "art-" (symbol-name symbol))
                                (not (string-suffix-p "-map" (symbol-name symbol)))))
                         (mapcar #'intern (all-completions "art-" obarray))))

(setq default-directory "~/")

(if (boundp 'use-short-answers)
    (customize-set-variable 'use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(customize-set-variable
 'bookmark-file
 (expand-file-name "local/bookmarks" user-emacs-directory))
(customize-set-variable
 'project-list-file
 (expand-file-name "local/projects" user-emacs-directory))
(customize-set-variable
 'tramp-persistency-file-name
 (expand-file-name "local/tramp" user-emacs-directory))
(customize-set-variable
 'transient-history-file
 (expand-file-name "local/history.el" user-emacs-directory))
(customize-set-variable
 'url-configuration-directory
 (expand-file-name "local/url/" user-emacs-directory))
(customize-set-variable
 'url-cache-directory
 (expand-file-name "local/url/cache" user-emacs-directory))
(customize-set-variable
 'git-packages-directory
 (expand-file-name "git-packages" user-emacs-directory))

(customize-set-variable 'global-auto-revert-non-file-buffers t)
(customize-set-variable 'load-prefer-newer t)
(customize-set-variable 'confirm-kill-processes nil)
(customize-set-variable 'kill-do-not-save-duplicates t)
(customize-set-variable 'tab-always-indent 'complete)
(customize-set-variable 'display-fill-column-indicator-column 79)
(customize-set-variable 'warning-minimum-level :error)
(customize-set-variable 'help-window-select t)
(customize-set-variable 'display-buffer-base-action
                        '((display-buffer-in-previous-window
                           display-buffer-reuse-window
                           display-buffer-use-some-window
                           display-buffer--maybe-pop-up-frame-or-window
                           display-buffer-in-side-window
                           display-buffer-pop-up-frame)))

(put 'narrow-to-region 'disabled nil)

;;; RECENT FILES
(customize-set-variable 'recentf-save-file
                        (expand-file-name "local/recentf" user-emacs-directory))
(customize-set-variable 'recentf-max-saved-items 20)
(customize-set-variable 'recentf-exclude
                        (list "COMMIT_EDITMSG"
                              "~$"
                              "/tmp/"
                              "/ssh:"
                              "/sudo:"
                              "/scp:"
                              ;; binary
                              "\\.mkv$"
                              "\\.mp[34]$"
                              "\\.avi$"
                              "\\.pdf$"
                              "\\.docx?$"
                              "\\.xlsx?$"))

;;; BACKUPS AND AUTO-SAVES
;; store all backup and auto-save files in the tmp dir
(customize-set-variable 'backup-directory-alist
                        `((".*" . ,temporary-file-directory)))

(customize-set-variable 'auto-save-file-name-transforms
                        `((".*" ,temporary-file-directory t)))

(customize-set-variable
 'auto-save-list-file-prefix
 (expand-file-name "emacs-auto-save-list/.saves-" temporary-file-directory))

;;; KEYBINDINGS
(global-set-key (kbd "C-c e") #'eshell)

;;; PERSONAL INFO
(customize-set-variable 'user-full-name "Arto Levi")
(customize-set-variable 'user-mail-address "arto.levi")


(provide 'art-base)
;;; art-base.el ends here
