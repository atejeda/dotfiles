;; generated from emacs.org file

(setq gc-cons-threshold (* 50 1000 1000))

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages '(
  use-package
  doom-themes
  lsp-mode
  multiple-cursors 
  all-the-icons
  powerline
  neotree
  fill-column-indicator
  bazel
  groovy-mode
  yaml
  yaml-mode
  haskell-mode
  elfeed
  json-mode
  rainbow-delimiters
  which-key
  ivy
  ivy-rich
  counsel
  treemacs
  visual-fill
  visual-fill-column
  dashboard
  org-auto-tangle
  evil
  undo-fu
  evil-collection
  swiper
  smooth-scrolling
  no-littering
  doom-modeline
  nix-haskell-mode
  projectile
  magit))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(defun dev-mode ()
  (interactive)
  (save-excursion
    (require 'doom-themes)
    (load-theme 'doom-one t)
    (text-scale-set 0)
    (cond
     ((eq system-type 'gnu/linux)
      (set-face-attribute 'default nil :family "Hack" :height 100)
      (set-background-color "#1c2023")
      (set-foreground-color "#c7ccd1"))
     ((eq system-type 'darwin)
      (set-face-attribute 'default nil :family "Courier New" :height 140)
      (set-background-color "#121212")
      (set-foreground-color "#d8dee8")))))
;;(global-set-key (kbd "C-c r m") 'dev-mode)

(defun presentation-mode ()
  (interactive)
  (save-excursion
    (dev-mode)
    (require 'doom-themes)
    (load-theme 'doom-one-light t)
    (text-scale-set 3)))
;;(global-set-key (kbd "C-c r m") 'presentation-mode)

(dev-mode)

(display-time)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(column-number-mode 1)
(line-number-mode 1)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(savehist-mode 1)
(global-linum-mode 1)
;;(global-hl-line-mode 1)
(set-fringe-mode 10)
(define-key read-expression-map (kbd "TAB") #'lisp-complete-symbol)

(require 'powerline)
(powerline-default-theme)
(setq powerline-arrow-shape 'arrow)

(use-package all-the-icons
  :if (display-graphic-p))

(add-hook 'prog-mode-hook (lambda () (hl-line-mode 1)))
(add-hook 'text-mode-hook (lambda () (hl-line-mode 1)))
(add-hook 'org-mode-hook (lambda () (hl-line-mode 1)))

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;;(setq dashboard-projects-backend "projectile")
  (setq dashboard-startup-banner "./emacs.dashboard.33.png")
  (setq dashboard-banner-logo-title "Gizmo.. caca!")
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5))))

(defun custom/dashboard-mode-hooks ()
  (fci-mode 0)
  (linum-mode 0)
  (hl-line-mode 0))
(add-hook 'dashboard-mode-hook 'custom/dashboard-mode-hooks)

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "/bin/bash")
  (fci-mode 0)
  (linum-mode 0)
  (hl-line-mode 0))

(global-set-key "\C-l" 'goto-line)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-unset-key (kbd "C-z"))
(fset 'yes-or-no-p 'y-or-n-p)

(defalias 'select-all 'mark-whole-buffer)

(require 'uniquify)
(setq uniquify-separator "/"
      uniquify-buffer-name-style 'forward)

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default py-indent-offset 4)
(setq standard-indent 4)
(setq c-default-style "linux" c-basic-offset 4)
(setq scroll-step 1)
(setq make-backup-files nil)
(setq auto-fill-mode 1)
(setq next-line-add-newlines nil)

(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (
  lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-column 79)
(setq fci-rule-width 1)
(setq fci-rule-color "grey22")

(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode 1)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(show-paren-mode 1)
(setq show-paren-delay 0)

(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-word-like-this)

(use-package treemacs
  :ensure t
  :defer t
  :init
  :config
  (progn (setq treemacs-no-png-images t))
  (treemacs-resize-icons 14)
  (dolist (face '(treemacs-root-face
                  treemacs-git-unmodified-face
                  treemacs-git-modified-face
                  treemacs-git-renamed-face
                  treemacs-git-ignored-face
                  treemacs-git-untracked-face
                  treemacs-git-added-face
                  treemacs-git-conflict-face
                  treemacs-directory-face
                  treemacs-directory-collapsed-face
                  treemacs-file-face
                  treemacs-tags-face))
    (set-face-attribute face nil :family "Courier New" :height 140)))

(setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(defun custom/org-mode-visual-fill ()
  (setq visual-fill-column-width 80
        visual-fill-column-center-text t
        fci-mode 0)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook
  (org-mode . custom/org-mode-visual-fill))

(setq-default prettify-symbols-alist '(("#+begin_src" . "")
                                       ("#+begin_src emacs-lisp" . "")
                                       ("#+begin_src text :tangle no" . "")
                                       ("#+end_src" . "")))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(use-package org
  :config
  (setq org-hide-emphasis-markers t))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("nn" . "src text :tangle no"))

(setq org-confirm-babel-evaluate nil)

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(defun custom/org-babel-tangle-config()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "some/absolute/path/emacs.org"))
    ;; let dynamic scoping?
    (let ((org-confirm-babel-evaluate-nil))
      (org-babel-table))))

(add-hook 'org-mode-hook
  (lambda ()
  (add-hook 'after-save-hook #'custom/org-babel-tangle-config)))

;;(require 'org-auto-tangle)
;;(add-hook 'org-mode-hook 'org-auto-tangle-mode)
(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(defun custom/org-mode-hooks ()
  (fci-mode 0)
  (linum-mode 0)
  (org-display-inline-images))
(add-hook 'org-mode-hook 'custom/org-mode-hooks)

(setq elfeed-feeds (quote
  (("https://news.ycombinator.com/rss" tech hackernews)
   ("https://blog.tartanllama.xyz/feed.xml" programming cpp)
   ("https://linuxnewbieguide.org/feed/" tech linux))))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(defun custom/ivy-tab-hooks ()
  (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-partial)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done))
(add-hook 'ivy-mode-hook 'custom/ivy-tab-hooks)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ;;("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minubuffer-history))
  :config (fci-mode 0))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :hook (rust-mode . lsp))

(setq lsp-prefer-capf t)
(setq lsp-completion-provider :capf)
(setq lsp-completion-enable t)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects/")
    (setq projectile-project-search-path '("~/projects/")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package rust-mode
  :ensure t)

(defun custom/rust-mode-hooks ()
  (setq indent-tabs-mode nil)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))
(add-hook 'rust-mode-hook 'custom/rust-mode-hooks)
(add-hook 'rust-mode-hook #'lsp)

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(setq gc-cons-threshold (* 2 1000 1000))

;;(set-face-bold-p 'bold nil) ;; disable bold fonts
(defun custom/is-org-face (face)
  (setq matchstr nil)
  (setq facestr (format "%s" face))
  (save-match-data
    (and (string-match "^.*\\(org\\).*$" facestr)
         (setq matchstr (match-string 1 facestr))))
  (if (null matchstr)
      (set-face-attribute face nil :weight 'normal :underline nil)
      ;; (with-current-buffer "*scratch*"
      ;;   (goto-char (point-max))
      ;;   (insert (format "\n%s" facestr)))
    ))

(mapc (lambda (face)(custom/is-org-face face)) (face-list))

;; eof
;; below this line, there's pure garbage
