;;######################
;; Initialization
;;######################

(require 'package)

;; Set package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Refresh
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)


(setq use-package-always-ensure t)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


;; Dashboard

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs!")

;; Set the banner
(setq dashboard-startup-banner 'logo)

;; Icons
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

;; Navigator
(setq dashboard-set-navigator t)


;; Widgets
(setq dashboard-items '((recents  . 5)))

;;######################
;; GUI Settings
;;######################

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips      

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
		helm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set font to Cascadia Code
(set-face-attribute 'default nil :font "Cascadia Code" :height 120)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Cascadia Code" :height 120)


(add-to-list 'default-frame-alist '(font . "Cascadia Code 12"))
;; Set cursor

(setq-default cursor-type 'bar)

;;######################
;; Misc Settings
;;######################


;; Remove backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "/usr/bin/sbcl")

;;######################
;; UI Packages
;;######################

;; Set colorscheme
(use-package doom-themes
:init (load-theme 'doom-nord t))
;; Icons for the modeline
(use-package all-the-icons)

;; Better modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))


;;(use-package centaur-tabs
;;  :demand
;;  :config
;;  (centaur-tabs-mode t)
;;  :bind
;;  ("C-<prior>" . centaur-tabs-backward)
;;  ("C-<next>" . centaur-tabs-forward)
;;)

;;(setq centaur-tabs-style "bar")
;;(setq centaur-tabs-set-icons t)
;;(setq centaur-tabs-height 15)
;;(setq centaur-tabs-set-bar 'left)
;;(setq centaur-tabs-set-modified-marker t)

;;######################
;; Misc tools
;;######################

(use-package helm)
(use-package helm-projectile)

(add-to-list 'projectile-globally-ignored-file-suffixes ".o")
(add-to-list 'projectile-globally-ignored-directories "limine")

(setq projectile-indexing-method 'hybrid)
;; Autopair
(use-package autopair
  :config (electric-pair-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode) . 'rainbow-delimiters-mode))


;;######################
;; C/C++ Tools
;;######################

(use-package clang-format)
(setq clang-format-style "file")

;; Auto-completion

(use-package irony
  :hook ((c-mode) . 'irony-mode)
  )

(use-package company
  :config (add-to-list 'company-backends '(company-irony company-irony-c-headers))
  (global-company-mode))

(setq company-backends (delete 'company-semantic company-backends))

(use-package company-lsp)
(require 'company-lsp)
(push 'company-lsp company-backends)



(require 'cc-mode)
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

;;######################
;; Python tools
;;######################

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

;;######################
;; Org mode
;;######################
(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
   ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))))
  
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (efs/org-font-setup))


(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

  
(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))


;;######################
;; Keybindings
;;######################


(evil-mode)
(global-set-key (kbd "C-S-x") 'kill-whole-line)

(global-unset-key (kbd "<left>") )

(global-unset-key (kbd "<right>") )

(global-unset-key (kbd "<up>") )

(global-unset-key (kbd "<down>") )



(global-set-key (kbd "C-S-x") 'kill-whole-line)

(global-unset-key (kbd "<left>") )

(global-unset-key (kbd "<right>") )

(global-unset-key (kbd "<up>") )

(global-unset-key (kbd "<down>") )

(global-set-key (kbd "C-S-c") 'clang-format-buffer)

(global-set-key (kbd "C-x C-f") 'helm-projectile)

(global-set-key (kbd "C-S-f") 'find-file)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-x C-r") 'helm-recentf)

(global-set-key (kbd "C-)") 'compile)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" default))
 '(package-selected-packages
   '(visual-fill-column weblorg web-mode use-package rainbow-delimiters pfuture org-bullets lsp-pyright irony hydra htmlize helm-projectile go-mode evil doom-themes doom-modeline dashboard company-lsp clang-format cfrs autopair ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
