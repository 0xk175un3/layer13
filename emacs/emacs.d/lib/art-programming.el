;;; art-programming.el --- Config for programming -*- lexical-binding: t; -*-

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
(defgroup art-programming ()
  "Customise group for the `art-programming' Library."
  :group 'art)

(defcustom art-edit-path
  (expand-file-name "libraries/art-programming.el" user-emacs-directory)
  "Variable for the path of the module `art-programming'."
  :type 'file
  :group 'art-programming)

;;; LIBRARIES
(require 'elixir-mode)
(require 'flycheck)
(require 'lsp-mode)
(require 'rbenv)
(require 'ruby-mode)

;;; CONFIGURATION SETTINGS
;;;; Elixir
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
(add-to-list 'interpreter-mode-alist '("elixir" . elixir-mode))
(add-to-list 'exec-path (expand-file-name "git-packages" user-emacs-directory))
(setq lsp-elixir-suggest-specs t
      lsp-elixir-enable-test-lenses t
      lsp-elixir-fetch-deps t
      lsp-elixir-local-server-command (concat (expand-file-name "git-packages" user-emacs-directory) "/elixir-ls/language_server.sh"))
;;;; Flycheck
(setq flycheck-disabled-checkers '(ruby-reek))
;;;; LSP
(setq lsp-completion-provider :capf)
(setq lsp-headerline-breadcrumb-enable nil)
;;;; Ruby
(setq rbenv-show-active-ruby-in-modeline nil)
(setq rbenv-modeline-function 'rbenv--modeline-plain)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.\\(pry\\|irb\\)rc$" . ruby-mode))
(add-to-list 'auto-mode-alist '("/\\(Gem\\|Cap\\|Vagrant\\|Rake\\|Pod\\|Puppet\\|Berks\\)file$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(defun my-ruby-mode-config ()
  "Ruby mode customizations."
  (setq ruby-insert-encoding-magic-comment nil)
  (setq ruby-deep-indent-paren t))

;;; HOOKS
;;;; Elixir
(add-hook 'elixir-mode-hook #'lsp-mode)
(add-hook 'elixir-mode-hook #'flycheck-mode)
;;;; Ruby
(global-rbenv-mode)
(add-hook 'ruby-mode-hook #'lsp-mode)
(add-hook 'ruby-mode-hook #'my-ruby-mode-config)
(add-hook 'ruby-mode-hook #'flycheck-mode)

;;; FUNCTIONS
(defun art-download-latest-elixir-ls ()
  "Download the latest release of elixir-lsp/elixir-ls, unpack it, remove the old version, and clean up the .zip file."
  (interactive)
  (let* ((download-dir (expand-file-name "git-packages" user-emacs-directory))
         (unpack-dir (concat download-dir "/elixir-ls"))
         (download-command (concat "mkdir -p " download-dir " && "
                                   "curl -s 'https://api.github.com/repos/elixir-lsp/elixir-ls/releases/latest' "
                                   "| jq -r '.assets[] | select(.name | contains(\"elixir-ls-\")) | .browser_download_url' "
                                   "| xargs -I {} wget -nv -P " download-dir " '{}'"))
         (cleanup-and-unzip-command (concat "rm -rf " unpack-dir "/* && "
                                            "mkdir -p " unpack-dir " && "
                                            "find " download-dir " -name 'elixir-ls-*.zip' "
                                            "-exec unzip -o -d " unpack-dir " {} \\; "
                                            "-exec rm {} \\;")))

    ;; Download the file
    (shell-command download-command)
    ;; Cleanup the old version, unzip the downloaded file into the elixir-ls directory, and remove the zip file
    (shell-command cleanup-and-unzip-command)
    (message "Downloaded and set up latest elixir-ls release in %s" unpack-dir)))


(provide 'art-programming)
;;; art-programming.el ends here
