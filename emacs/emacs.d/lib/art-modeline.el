;;; art-modeline.el --- Config for modeline -*- lexical-binding: t; -*-

;; Author: Arto Levi
;; Version: 1.2
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
(defgroup art-modeline ()
  "Customise group for the `art-modeline' Library."
  :group 'art)

(defcustom art-modeline-path
  (expand-file-name "libraries/art-modeline.el" user-emacs-directory)
  "Variable for the path of the module `art-modeline'."
  :type 'file
  :group 'art-modeline)

;;; CONFIGURATION SETTINGS
(setq Info-use-header-line nil)
(setq eshell-status-in-modeline nil)
(setq-default mode-line-format nil)

;;;; Derived modes
(defun art-modeline-message-mode-p ()
  (derived-mode-p 'message-mode))
(defun art-modeline-info-mode-p ()
  (derived-mode-p 'Info-mode))
(defun art-modeline-prog-mode-p ()
  (derived-mode-p 'prog-mode))
(defun art-modeline-text-mode-p ()
  (derived-mode-p 'text-mode))

;;;; Helper functions
(defun vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

(defun art-mode-name ()
  (if (listp mode-name) (car mode-name) mode-name))

(defun art-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)"
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "**") (read-only "RO") (t "RW"))))

(defun art-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width (window-font-width nil 'header-line))
         (window (get-buffer-window (current-buffer)))
         (space-up +0.15)
         (space-down -0.20)
         (prefix (cond ((string= status "RO")
                        (propertize (if (window-dedicated-p) " -- " " RO ")
                                    'face 'art-face-header-popout))
                       ((string= status "**")
                        (propertize (if (window-dedicated-p) " -- " " ** ")
                                    'face 'art-face-header-critical))
                       ((string= status "RW")
                        (propertize (if (window-dedicated-p) " -- " " RW ")
                                    'face 'art-face-header-faded))
                       (t (propertize status 'face 'art-face-header-popout))))
         (left (concat
                (propertize " " 'face 'art-face-header-default
                            'display `(raise ,space-up))
                (propertize name 'face 'art-face-header-strong)
                (propertize " " 'face 'art-face-header-default
                            'display `(raise ,space-down))
                (propertize primary 'face 'art-face-header-default)))
         (right (concat secondary " ")))

    ;; Calculate the exact display width of strings
    (defun string-display-width (str)
      (let ((len 0))
        (mapc (lambda (c) (setq len (+ len (char-width c))))
              (string-to-list str))
        len))

    ;; Adjusted width calculation
    (let* ((prefix-width (string-display-width prefix))
           (left-width (string-display-width left))
           (right-width (string-display-width right))
           (divider-width (max 0 (/ (window-right-divider-width) char-width)))
           (available-width (- (window-total-width)
                               prefix-width left-width right-width
                               divider-width)))

      ;; Ensure available-width is non-negative
      (setq available-width (max 0 available-width))

      ;; Compose the final string
      (concat prefix
              left
              (propertize (make-string available-width ?\ )
                          'face 'art-face-header-default)
              (propertize right 'face `(:inherit art-face-header-default
                                        :foreground ,art-color-faded))))))

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

(defun art-modeline-info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
	(node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
	line)
    (while  (> depth 0)
      (setq node (nth 1 (assoc node nodes)))
      (if node (push node crumbs))
      (setq depth (1- depth)))
    (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
			         crumbs (cons nil crumbs))))
    (forward-line 1)
    (dolist (node crumbs)
      (let ((text
	     (if (not (equal node "Top")) node
	       (format "%s"
		       (if (stringp Info-current-file)
			   (file-name-sans-extension
			    (file-name-nondirectory Info-current-file))
			 Info-current-file)))))
	(setq line (concat line (if (null line) "" " > ")
                                (if (null node) "..." text)))))
    (if (and cnode (not (equal cnode "Top")))
        (setq line (concat line (if (null line) "" " > ") cnode)))
    line))

(defun art-modeline-update-windows ()
  "Modify the mode line depending on the presence of a window
below or a buffer local variable 'no-mode-line'."
  (dolist (window (window-list))
    (with-selected-window window
	  (with-current-buffer (window-buffer window)
        (if (or (not (boundp 'no-mode-line)) (not no-mode-line))
            (set-window-parameter window 'mode-line-format
                                  (cond ((not mode-line-format) 'none)
                                        ((one-window-p t 'visible) (list ""))
                                        ((eq (window-in-direction 'below) (minibuffer-window)) (list ""))
                                        ((not (window-in-direction 'below)) (list ""))
                                        (t 'none))))))))

(defun buffer-menu-mode-header-line ()
  (face-remap-add-relative
   'header-line `(:background ,(face-background 'nano-face-subtle))))

;;; MODELINE MODE FUNCTIONS
(defun art-modeline-message-mode ()
  (art-modeline-compose (art-modeline-status)
                        "Message" "(draft)" ""))
(defun art-modeline-info-mode ()
  (art-modeline-compose (art-modeline-status)
                         "Info"
                         (concat "("
                                 (art-modeline-info-breadcrumbs)
                                 ")")
                         ""))
(defun art-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (art-mode-name))
          (branch      (vc-branch))
          (position    (format-mode-line "%l:%c")))
      (art-modeline-compose (art-modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                            (propertize branch 'face 'italic)))
                                     ")" )
                             position)))

(defun enable-art-modeline ()
  "Install a header line whose content is dependend on the major mode"
  (interactive)
  (setq-default header-line-format
  '((:eval
     (cond ((art-modeline-prog-mode-p)            (art-modeline-default-mode))
           ((art-modeline-message-mode-p)         (art-modeline-message-mode))
           ((art-modeline-info-mode-p)            (art-modeline-info-mode))
           ((art-modeline-text-mode-p)            (art-modeline-default-mode))
           (t                                     (art-modeline-default-mode)))))))

;;; HOOKS
(add-hook 'Buffer-menu-mode-hook #'buffer-menu-mode-header-line)
(add-hook 'window-configuration-change-hook 'art-modeline-update-windows)


(provide 'art-modeline)
;;; art-modeline.el ends here
