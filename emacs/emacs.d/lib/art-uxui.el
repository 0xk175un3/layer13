;;; art-uxui.el --- Config for improves UX and UI of Emacs -*- lexical-binding: t; -*-

;; Author: Arto Levi
;; Version: 1.4
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
(defgroup art-uxui ()
  "Customise group for the `art-uxui' Library."
  :group 'art)

(defcustom art-edit-path
  (expand-file-name "libraries/art-uxui.el" user-emacs-directory)
  "Variable for the path of the module `art-uxui'."
  :type 'file
  :group 'art-uxui)

;;; FONTS
(set-frame-font "PragmataPro Mono Liga:pixelsize=17:antialias=true:autohint=true" nil t)
;; (add-to-list 'load-path "~/.emacs.d/git-packages/emacs-pragmatapro-ligatures")

;;; LIBRARIES
(require 'ace-window)
(require 'counsel)
(require 'doom-themes)
(require 'hydra)
(require 'ivy)
;; require 'pragmatapro-lig)
(require 'projectile)
(require 'swiper)

;;; THEME
(load-theme 'doom-nord t)

;;; CONFIGURATION SETTINGS
;;;; ui
(setq art-font-family-monospaced "PragmataPro Mono Liga")
(setq art-font-family-proportional nil)
(setq art-font-size 14)
;;;; Ace-window
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;;; Ivy
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "%d/%d ")
(setq ivy-display-style 'fancy)
;;;; Projectile
(setq projectile-completion-system 'ivy)
(setq projectile-enable-caching nil)
(setq projectile-verbose nil)
(setq projectile-do-log nil)
;;;; Swiper
(setq ivy-use-virtual-buffers t)
(setq ivy-display-style 'fancy)

;;; KEYBINDINGS
;;;; Ace-window
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-p") 'ace-delete-window)
;;;; Ivy
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;;;; Projectile
(global-set-key (kbd "C-c p") 'projectile-command-map)
;;;; Swiper
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c C-f") 'counsel-find-file)

;;; HYDRA HEADS
;;;; Projectile head
(defhydra hydra-projectile (:color gray
                            :exit t
                            :hint nil)
  "
      ╔═════════════════════════^^^══════════^^^══════════════════════╗
      ║                         ^^^Projectile^^^                      ║
      ╠═════^═════════^═════╦═════^══════════^════╦═════^═══════^═════╣
      ║     ^  Search  ^    ║    ^  Buffers  ^    ║     ^ Other ^     ║
      ╠═════^═════════^═════╬═════^══════════^════╬═════^═══════^═════╣
      ║ _f_: file           ║ _l_: list           ║ _i_: reset cache  ║
      ║ _d_: directory      ║ _k_: kill all       ║^ ^                ║
      ║ _p_: project        ║^ ^                  ║^ ^                ║
      ║ _s_: grep           ║^ ^                  ║^ ^                ║
      ╚═════^═════════^═════╩═════^══════════^════╩═════^═══════^═════╝
  "
  ("f" projectile-find-file)
  ("d" projectile-find-dir)
  ("p" projectile-switch-project)
  ("s" projectile-ag)
  ("l" projectile-switch-to-buffer)
  ("k" projectile-kill-buffer)
  ("i" projectile-invalidate-cache))
(global-set-key (kbd "C-c p") 'hydra-projectile/body)

;;;; HOWM head
(defhydra hydra-howm (:color gray
                      :exit t
                      :hint nil)
  "
      ╔═════════════════════════^^^══════════^^^══════════════════════╗
      ║                            ^^^HOWM^^^                         ║
      ╠═════^═════════^═════╦═════^══════════^════╦═════^═══════^═════╣
      ║     ^  Search  ^    ║     ^  Lists   ^    ║     ^ Other ^     ║
      ╠═════^═════════^═════╬═════^══════════^════╬═════^═══════^═════╣
      ║ _r_: counsel rg     ║ _a_: list all       ║ _c_: new note     ║
      ║ _g_: list grep      ║ _l_: list recent    ║ _d_: insert date  ║
      ║ _t_: today note     ║ _w_: random walk    ║ _k_: kill all     ║
      ║^ ^                  ║^ ^                  ║ _m_: main menu    ║
      ╚═════^═════════^═════╩═════^══════════^════╩═════^═══════^═════╝
  "
  ("c" howm-create)
  ("d" howm-insert-date)
  ("k" howm-kill-all)
  ("m" howm-menu)
  ("a" howm-list-all)
  ("l" howm-list-recent)
  ("w" howm-random-walk)
  ("g" howm-list-grep)
  ("r" howm-counsel-rg)
  ("t" howm-find-today))

(global-set-key (kbd "C-c n") 'hydra-howm/body)

;;; HOOKS
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq mode-line-format nil)))

;;; ENABLE MODES
(ivy-mode 1)
;; (pragmatapro-lig-global-mode 1)
(projectile-mode 1)
(enable-art-modeline)


(provide 'art-uxui)
;;; art-uxui.el ends here
