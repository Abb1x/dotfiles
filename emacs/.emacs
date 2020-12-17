

;; Removes the startup message
(setq inhibit-startup-message t)

;; Removes GUI stuff
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)

(setq mac-command-modifier 'meta)

;; Shows lines number
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; Removes the annoying bell
(setq visible-bell 1)

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
(setq-default cursor-type 'bar)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


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

