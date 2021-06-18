(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
;; Refresh
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure that use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)


(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips      

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
		    helm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "JetBrains Mono" :height 120)

(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 120)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono 12"))

;; Set cursor
(setq-default cursor-type 'bar)

(use-package dashboard
      :ensure t
      :config
      (dashboard-setup-startup-hook))

    ;; Set the title
    (setq dashboard-banner-logo-title "Welcome to Emacs!")

    ;; Set the banner
    (setq dashboard-startup-banner "/home/abbix/.emacs.d/text.txt")

    ;; Icons
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)

    ;; Navigator
    (setq dashboard-set-navigator t)

  (setq dashboard-navigator-buttons
`(;; line1
            ((,(all-the-icons-faicon "undo" :height 1.1 :v-adjust 0.0)
              "Update"
              "Update Emacs"
              (lambda (&rest _) (package-refresh-contents)))
             (,(all-the-icons-octicon "times" :height 1.1 :v-adjust 0.0)
              "Quit"
              "Close Emacs"
              (lambda (&rest _) (save-buffers-kill-terminal))))))

    (setq dashboard-center-content t)


    ;; Widgets
    (setq dashboard-items '((recents  . 5)
                            (projects . 5)))

(defun abx/download-ayu ()
(shell-command "cd ~/.emacs.d/elpa/doom-themes*/ && wget https://raw.githubusercontent.com/LoveSponge/emacs-doom-themes/master/themes/doom-ayu-dark-theme.el")
(load-theme 'doom-ayu-dark t))
  (use-package doom-themes
    :init (abx/download-ayu))

;; Icons
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))

(defun enable-doom-modeline-icons (_frame)
  (setq doom-modeline-icon t))
  
(add-hook 'after-make-frame-functions 
          #'enable-doom-modeline-icons)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package helm)
(use-package helm-projectile)

(add-to-list 'projectile-globally-ignored-file-suffixes ".o")
(add-to-list 'projectile-globally-ignored-directories "limine")

(setq projectile-indexing-method 'hybrid)

(electric-pair-mode)

(use-package rainbow-delimiters
  :hook ((prog-mode) . 'rainbow-delimiters-mode))

(use-package dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(use-package magit)

(use-package clang-format)
(setq clang-format-style "file")

;; Auto-completion
(use-package irony
  :hook ((c-mode) . 'irony-mode)
  )/

(use-package company
  :config (add-to-list 'company-backends '(company-irony company-irony-c-headers))
  (global-company-mode))

(setq company-backends (delete 'company-semantic company-backends))

(require 'cc-mode)
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

(add-hook 'c-mode-common-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

(defun abx/org-font-setup ()
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

(defun abx/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode .  abx/org-mode-setup)
  :ensure org-plus-contrib
  :config
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (abx/org-font-setup))

(require 'ox-groff)
(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(defun abx/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'abx/org-babel-tangle-config)))

(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process
      (list (concat "latexmk -"
                    org-latex-compiler 
                    " -recorder -synctex=1 -bibtex-cond %b")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun abx/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . abx/org-mode-visual-fill))

(setq evil-want-keybinding nil)
  (use-package evil)
      (evil-mode)

    (use-package evil-collection
      :after evil
      :ensure t
      :config
      (evil-collection-init))

(global-unset-key (kbd "<left>"))

(global-unset-key (kbd "<right>"))

(global-unset-key (kbd "<up>"))

(global-unset-key (kbd "<down>"))



(global-set-key (kbd "C-S-x") 'kill-whole-line)

(global-set-key (kbd "C-S-c") 'clang-format-buffer)

(global-set-key (kbd "C-x C-f") 'helm-projectile)

(global-set-key (kbd "C-S-f") 'find-file)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-x C-r") 'helm-recentf)

(global-set-key (kbd "C-)") 'compile)
