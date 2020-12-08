
;; Removes the startup message
(setq inhibit-startup-message t)

;; Removes GUI stuff
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)

(setq mac-command-modifier 'meta)

;; Shows lines number
(global-display-line-numbers-mode)
;; Removes the annoying bell
(setq visible-bell 1)

;; Adds the repo melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; Loads the Doom Palenight theme
(load-theme 'doom-palenight t)

;; Enables company: an autocompleter
(global-company-mode t)

;; Set font to Cascadia Code
(set-face-attribute 'default nil :font "Cascadia Code 12" )
(set-frame-font "Cascadia Code 12" nil t)

;; Customizes Neotree
(setq neo-theme (if (display-graphic-p) 'arrow 'arrow))

;; Enables doom-modeline
(require 'doom-modeline)

(doom-modeline-mode 1)

(setq doom-modeline-height 25)

;; Keybindings
(global-set-key (kbd "C-k") 'kill-whole-line)

(global-unset-key (kbd "<left>") )

(global-unset-key (kbd "<right>") )

(global-unset-key (kbd "<up>") )

(global-unset-key (kbd "<down>") )

(global-set-key (kbd "C-1") 'neotree-toggle)
