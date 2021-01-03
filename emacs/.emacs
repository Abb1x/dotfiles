
;; Adds the repo melpa
(require 'use-package)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(dolist (package '(use-package))
   (unless (package-installed-p package)
     (package-install package)))

(dolist (package '(helm-projectile helm ag ivy dumb-jump clang-format magit rainbow-delimiters elcord org-bullets org-preview-html org all-the-icons-dired color-theme-sanityinc-tomorrow monokai-theme use-package pacmacs neotree doom-themes doom-modeline company-c-headers))
 (unless (package-installed-p package)
   (package-install package))
   (require package))

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

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
;; (require 'elcord)
;; (elcord-mode)
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
