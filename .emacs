;; generated from emacs.org file

;; gc for faster initialization
(setq gc-cons-threshold (* 50 1000 1000))

;; melpa repository
(setq custom/v-melpa "https://melpa.org/package")

;; foreground, background color
(setq custom/v-color-bg nil)
(setq custom/v-color-fg nil)

;; font family, height
(setq custom/v-font-fam nil)
(setq custom/v-font-ht nil)

;; is linux
(setq custom/v-is-linux nil)

;; is darwin
(setq custom/v-is-darwin nil)

;; linux or darwin
(cond
 ((eq system-type 'gnu/linux)
  (setq custom/v-color-bg '(background-color . "#1c2023"))
  (setq custom/v-color-fg '(foreground-color . "#c7ccd1"))
  (setq custom/v-font-fam "Hack")
  (setq custom/v-font-ht 100)
  (setq custom/v-is-linux t))
 ((eq system-type 'darwin)
  (setq custom/v-color-bg '(background-color . "#121212"))
  (setq custom/v-color-fg '(foreground-color . "#d8dee8"))
  (setq custom/v-font-fam "Courier New")
  (setq custom/v-font-ht 140)
  (setq custom/v-is-darwin t)))

;; package
(require 'package)
(add-to-list 'package-archives (cons "melpa" custom/v-melpa) t)
(package-initialize)

;; package list
(setq package-selected-packages
      '(
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
        magit
        rust-mode))

;; auto install
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; use-package
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(display-time)
(savehist-mode 1)

;; no startaup screen, no bell
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

;; look
(defun custom/f-config-look ()
  (interactive)
  (save-excursion)

  ;; theme
  (use-package doom-themes :defer nil)
  ;;(load-theme 'doom-one t)
  (load-theme 'doom-nord t)

  ;; general settings
  (menu-bar-mode -1)      ;; no bar
  (tool-bar-mode -1)      ;; no tool bar
  (scroll-bar-mode -1)    ;; no scroll bar
  (set-fringe-mode 10)    ;; fringe to 10
  (column-number-mode 1)  ;; column number in the mode line
  (line-number-mode 1)    ;; line number in the mode line
  (global-linum-mode 0)   ;; line number in the buffer left margin
  (global-hl-line-mode 0) ;; line highlight

  (setq vc-follow-symlinks t)  ;; follow symlinks, default 'ask'

  ;; enable parenthesis hightlight
  (show-paren-mode 1)
  (setq show-paren-delay 0)

  ;; reset text scale
  (text-scale-set 0)

  ;; tabs
  (setq-default indent-tabs-mode nil)
  (setq-default c-basic-offset 4)
  (setq-default py-indent-offset 4)
  (setq standard-indent 4)
  (setq c-default-style "linux" c-basic-offset 4)
  (setq scroll-step 1)
  (setq make-backup-files nil)
  (setq auto-fill-mode 1)
  (setq next-line-add-newlines nil)

  ;; paren
  (show-paren-mode 1)
  (setq show-paren-delay 0)

  ;; frame parameters

  ;; initial-frame-alist
  (add-to-list 'initial-frame-alist custom/v-color-bg)
  (add-to-list 'initial-frame-alist custom/v-color-fg)
  ;;(add-to-list 'initial-frame-alist '(fullscreen . maximized))

  ;; default-frame-alist
  (add-to-list 'default-frame-alist custom/v-color-bg)
  (add-to-list 'default-frame-alist custom/v-color-fg)
  ;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; font
  (set-face-attribute 'default nil
                      :family custom/v-font-fam
                      :height custom/v-font-ht)
  )

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (custom/f-config-look))))
  (custom/f-config-look))

(if custom/v-is-linux
    ((use-package smooth-scrolling
       :defer nil
       :config
       (smooth-scrolling-mode 1))))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 18)
	   (doom-modeline-icon nil)))

(use-package all-the-icons
  :if (display-graphic-p))

;; global set keys

(global-set-key "\C-l" 'goto-line)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
;;(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-unset-key (kbd "C-z"))

(fset 'yes-or-no-p 'y-or-n-p)

;; aliases
(defalias 'select-all 'mark-whole-buffer)

;; orgmode

(defun custom/f-org-config ()
  ;; remove emphasis markers
  (setq org-hide-emphasis-markers t)

  ;; don't ask y/n when exec babel code
  (setq org-confirm-babel-evaluate nil)

  ;; disable auto-identation
  (setq org-indent-mode -1)
  (setq org-adapt-indentation nil)

  ;; source code blocks identation (left padding)
  (setq org-edit-src-content-indentation 2)
  (setq org-src-preserve-indentation nil)

  ;; babel configuration
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (python . t)))

  ;; org-tempo, structured templates, "<el + tab" to expand template
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist
               '("sh" . "src shell"))

  (add-to-list 'org-structure-template-alist
               '("el" . "src emacs-lisp"))

  (add-to-list 'org-structure-template-alist
               '("py" . "src python"))

  (add-to-list 'org-structure-template-alist
               '("nn" . "src text :tangle no")))

(defun custom/f-org-hook ()
  ;; look
  (fci-mode 0)
  (linum-mode 0)

  ;; content padding
  (visual-fill-column-mode)

  ;; display inline images
  (org-display-inline-images)

  ;; break column at 80
  (setq fill-column 80)
  (auto-fill-mode)

  ;; prettify symbols
  (setq-default prettify-symbols-alist
                '(("#+begin_src" . "")
                  ("#+begin_src emacs-lisp" . "")
                  ("#+begin_src text :tangle no" . "")
                  ("#+end_src" . "")))
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (prettify-symbols-mode))

(defun custom/f-org-face-block()
  (interactive)
  (save-excursion)
  (custom-set-faces
   '(org-block-begin-line
     ((t (
          ;;:underline "#A7A6AA"
          ;;:foreground "#008ED1"
          :background "#1e1e1e"
          :extend t))))
   '(org-block
     ((t (
          :background "#1e1e1e"
          :extend t))))
   '(org-block-end-line
     ((t (
          ;;:overline "#A7A6AA"
          ;;:foreground "#008ED1"
          :background "#1e1e1e"
          :extend t))))
   ))

(use-package org
  :defer t
  :config
  (custom/f-org-config)
  (custom/f-org-face-block)
  :hook (org-mode . custom/f-org-hook))

(use-package visual-fill-column
  :defer t
  :hook
  (lambda ()
    ((setq visual-fill-column-width 80)
     (setq visual-fill-column-center-text nil)
     (visual-fill-column-mode 1))))

(use-package org-auto-tangle
  :defer t
  :after org
  :hook (org-mode . org-auto-tangle-mode))

(use-package term
  :defer t
  :init
  (setq-default shell-file-name "/bin/bash") ;; doesn't work with term
  :commands term
  :config
  (fci-mode 0)
  (linum-mode 0)
  (hl-line-mode 0))

(defun termc ()
  (interactive)
  (save-excursion)
  (term "/bin/bash"))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-startup-banner
        (concat (file-name-directory (file-truename user-init-file))
                "emacs.dashboard.33.png"))
  (setq dashboard-banner-logo-title "")
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5))))

;;(with-current-buffer "*scratch*" (goto-char (point-max)) (insert (format "\ndd = %s" buffer-file-name)))

(use-package fill-column-indicator
  :defer t
  :config
  (setq fci-rule-column 79)
  (setq fci-rule-width 1)
  (setq fci-rule-color "grey22"))

(use-package whitespace
  :defer t
  :custom
  (setq whitespace-line-column 80)
  :config
  (setq whitespace-style '(face lines-tail)))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package multiple-cursors
  :defer t
  :bind (("C-c m c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-word-like-this)))

;; on the treemacs buffer
;; C-c p is the prefix command.
(use-package treemacs
  :defer t
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
    (set-face-attribute face nil :family custom/v-font-fam :height custom/v-font-ht)))

(use-package no-littering
  :defer nil
  :init
  (setq user-emacs-directory "~/.cache/emacs")
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package elfeed
  :defer t
  :config
  (setq elfeed-feeds
        (quote
         (("https://news.ycombinator.com/rss" tech hackernews)
          ("https://blog.tartanllama.xyz/feed.xml" programming cpp)
          ("https://linuxnewbieguide.org/feed/" tech linux)))))

;; ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 ("<tab>" . ivy-partial) ;; partial search
	 ("<ret>" . ivy-alt-done) ;; done with <ret>
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :defer nil
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-e" . 'counsel-minubuffer-history))
  :config
  ;; will avoid start search with ^
  ;; (setq ivy-initial-inputs-alist nil) 
  (fci-mode 0))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects/")
    (setq projectile-project-search-path '("~/projects/")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package magit
  :defer t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-prefer-capf t)
  (setq lsp-completion-provider :capf)
  (setq lsp-completion-enable t))
  ;;:hook (rust-mode . lsp))

(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-c" . rust-run))
  :config
  (add-hook 'rust-mode-hook #'lsp)
  :hook
  (setq indent-tabs-mode nil))

;; set bold off EVERYWHERE but orgmode
;;(set-face-bold-p 'bold nil) ;; disable bold fonts

(defun custom/f-is-org-face (face)
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

(mapc (lambda (face)(custom/f-is-org-face face)) (face-list))

;; enable fci, linenum, hl and whitespace for prog
;; modes only

(defun custom/prog-mode-hooks ()
  (fci-mode 1)
  (linum-mode 1)
  (hl-line-mode 1)
  (whitespace-mode 1))

  (add-hook 'prog-mode-hook 'custom/prog-mode-hooks)
  (add-hook 'text-mode-hook (lambda () (hl-line-mode 1)))
  (add-hook 'org-mode-hook (lambda () (hl-line-mode 1)))

(setq gc-cons-threshold (* 2 1000 1000))

;; eof
;; below this line, there's pure garbage
