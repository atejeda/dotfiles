;; generated from emacs.org

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
  visual-fill-column))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

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
(global-hl-line-mode 1)
(define-key read-expression-map (kbd "TAB") #'lisp-complete-symbol)

(require 'powerline)
(powerline-default-theme)
(setq powerline-arrow-shape 'arrow)

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

(global-set-key "\C-l" 'goto-line)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-unset-key (kbd "C-z"))
(fset 'yes-or-no-p 'y-or-n-p)

(defalias 'select-all 'mark-whole-buffer)

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
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
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
  (progn
    (setq
     treemacs-no-png-images t))
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

(defun custom/org-mode-visual-fill ()
  (setq visual-fill-column-width 80
        visual-fill-column-center-text t
        fci-mode 0
        )
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook
  (org-mode . custom/org-mode-visual-fill))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "")
                                       ("#+END_SRC" . "-")
                                       ("#+begin_src" . "")
                                       ("#+end_src" . "-")))
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

(setq elfeed-feeds (quote
  (("https://news.ycombinator.com/rss" tech hackernews)
   ("https://blog.tartanllama.xyz/feed.xml" programming cpp)
   ("https://linuxnewbieguide.org/feed/" tech linux))))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minubuffer-history))
  :config
  ;; avoid start search with ^
  (setq ivy-initial-inputs-alist nil))

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

(use-package rust-mode
  :ensure t)

(dev-mode)

;; eof
;; below this line, there's pure garbage
