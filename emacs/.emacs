;; Window title
(setq-default frame-title-format '("%b - Emacs"))

(require 'helm)
(require 'helm-projectile)

;; Removes the startup message
(setq inhibit-startup-message t)

;; Projectile
(add-to-list 'projectile-globally-ignored-file-suffixes ".o")
(add-to-list 'projectile-globally-ignored-directories "limine")
(setq projectile-indexing-method 'hybrid)
;; elcord
(require 'elcord)
(elcord-mode)
;; Clang stuff
(require 'clang-format)
(setq clang-format-style "file")
;; Magit
(require 'magit)
;; Removes GUI stuff
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)

(setq mac-command-modifier 'meta)
;; Jump to definition
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;; Shows lines number
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; Removes the annoying bell
(setq visible-bell 1)
;; Removes backup files

(setq make-backup-files nil)

;; Adds the repo melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(require 'use-package)
;; Loads the Doom Palenight theme
(load-theme 'doom-palenight t)

;; Rainbow brackets
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Enables company: an autocompleter
(global-company-mode t)

;; Set font to Cascadia Code
(set-face-attribute 'default nil :font "Cascadia Code 12" )
(set-frame-font "Cascadia Code 12" nil t)

(doom-themes-neotree-config)
(setq doom-themes-neotree-file-icons t)
(defun text-scale-twice ()(interactive)(progn(text-scale-adjust 0)(text-scale-decrease 0.8)))
(add-hook 'neo-after-create-hook (lambda (_)(call-interactively 'text-scale-twice)))
;; Enables doom-modeline
(require 'doom-modeline)

(doom-modeline-mode 1)

(setq doom-modeline-height 25)

;; Changes cursor

(setq-default cursor-type 'bar)

;; Org mode

(setq org-hide-emphasis-markers t)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Ligatures
(use-package ligature
  :load-path "~/emacs/ligature.el"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode '("!=" "==" "===" "->" "<-" "==>" "<==" ">>>" "<<<" "=>" "<=" ">=" "::" "..." "&&" "<<" ">>" "__" ".." "///" "##" "###" "####" "<>" "!!" ";;" "||" "|||" ":=" "??"))
  (global-ligature-mode t))
;; Keybindings
(global-set-key (kbd "C-S-x") 'kill-whole-line)

(global-unset-key (kbd "<left>") )

(global-unset-key (kbd "<right>") )

(global-unset-key (kbd "<up>") )

(global-unset-key (kbd "<down>") )

(global-set-key (kbd "C-1") 'neotree-toggle)

(global-set-key (kbd "C-S-j") 'next-line)
(global-set-key (kbd "C-S-k") 'previous-line)
(global-set-key (kbd "C-S-h") 'backward-char)
(global-set-key (kbd "C-S-l") 'forward-char)
(global-set-key (kbd "C-S-c") 'clang-format-buffer)
(global-set-key (kbd "C-x C-f") 'helm-projectile)
(global-set-key (kbd "C-x C-r") 'helm-recentf)

(defun duplicate-line ()
   (interactive)
   (let ((col (current-column)))
     (move-beginning-of-line 1)
     (kill-line)
     (yank)
     (newline)
     (yank)
     (move-to-column col)))


 (global-set-key (kbd "C-S-d") 'duplicate-line)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "9e39a8334e0e476157bfdb8e42e1cea43fad02c9ec7c0dbd5498cf02b9adeaf1" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "188fed85e53a774ae62e09ec95d58bb8f54932b3fd77223101d036e3564f9206" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(package-selected-packages
   '(helm-projectile helm ag ivy dumb-jump clang-format magit rainbow-delimiters elcord org-bullets org-preview-html org all-the-icons-dired color-theme-sanityinc-tomorrow ## monokai-theme use-package pacmacs neotree doom-themes doom-modeline company-web company-lsp company-c-headers cobalt ayu-theme atom-one-dark-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
