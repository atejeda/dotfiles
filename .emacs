

;; melpa and package stuff

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

;; '' install orgmode, orginit file.
;; compnay mode, ivy... rust...ooooo
;; evil mode

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; use-package
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; look and feel

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

;; bindings

(global-set-key "\C-l" 'goto-line)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; global keysets

(global-unset-key (kbd "C-z"))
(fset 'yes-or-no-p 'y-or-n-p)

;; paren

(show-paren-mode 1)
(setq show-paren-delay 0)

;; tabs and identation

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default py-indent-offset 4)
(setq standard-indent 4)
(setq c-default-style "linux" c-basic-offset 4)
(setq scroll-step 1)
(setq make-backup-files nil)
(setq auto-fill-mode 1)
(setq next-line-add-newlines nil)

;; multiple cursors

(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-word-like-this)

;; powerline

(require 'powerline)
(powerline-default-theme)
(setq powerline-arrow-shape 'arrow)

;; neotree

;;(require 'neotree)
;; (global-set-key [f8] 'neotree-toggle)
;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;; ;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
;; (eval-after-load "neotree"
;;   '(add-to-list 'window-size-change-functions
;;                 (lambda (frame)
;;                   (let ((neo-window (neo-global--get-window)))
;;                     (unless (null neo-window)
;;                       (setq neo-window-width (window-width neo-window)))))))
;;(setq neo-window-fixed-size nil)

;; treemacs

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
    (set-face-attribute face nil :family "Courier New" :height 140))
  )

;; fill column indicator

(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-column 79)
(setq fci-rule-width 1)
(setq fci-rule-color "grey22")

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode 1)

;; def aliases and def commands

(defalias 'select-all 'mark-whole-buffer)

(defun dev-mode ()
  (interactive)
  (save-excursion
    (require 'doom-themes)
    (load-theme 'doom-one t)
    (text-scale-set 0)
    (cond
     ((eq system-type 'gnu/linux)
      ;; Courier New : 120, UbuntuMono : 120, Monospace : 100, Hack 100
      (set-face-attribute 'default nil :family "Hack" :height 100)
      (set-background-color "#1c2023")
      (set-foreground-color "#c7ccd1"))
     ((eq system-type 'darwin)
      (set-face-attribute 'default nil :family "Courier New" :height 140)
      (set-background-color "#121212")
      (set-foreground-color "#d8dee8")))))

(defun retro-mode ()
  (interactive)
  (save-excursion
    (require 'doom-themes)
    (load-theme 'doom-one-light t)
    ;;(set-face-font
    ;;  'default "-adobe-courier-medium-r-normal--17-*-100-100-m-100-iso8859-9")
    (set-face-font
      'default "-adobe-courier-medium-r-normal--14-*-75-75-m-90-iso8859-9")
    (set-background-color "#0000FF")
    (set-foreground-color "#FFFFFF")))
;;(global-set-key (kbd "C-c r m") 'retro-mode)

(defun presentation-mode ()
  (interactive)
  (save-excursion
    (dev-mode)
    (require 'doom-themes)
    (load-theme 'doom-one-light t)
    ;;(set-background-color "#FFFFFF")
    ;;(set-foreground-color "#000000")
    (text-scale-set 3)))

(defun headache-mode ()
  (interactive)
  (save-excursion
    (dev-mode)
    (text-scale-set 1)))

(defun presentation-mode ()
  (interactive)
  (save-excursion
    (dev-mode)
    (require 'doom-themes)
    (load-theme 'doom-one-light t)
    (text-scale-set 3)))

(dev-mode)

;; ;; orgmode-html stuff

;; ;; questions/14684263/how-to-org-mode-image-absolute-path-of-export-html
;; ;; questions/9807/org-mode-dont-change-relative-urls
;; (defun wvxvw/export-rel-url (path desc format)
;;   (cl-case format
;;     (html (format "<a href=\"%s\">%s</a>" path (or desc path)))
;;     (latex (format "\\href{%s}{%s}" path (or desc path)))
;;     (otherwise path)))

;; (eval-after-load "org"
;;   '(org-link-set-parameters "rel" :follow #'browse-url :export #'wvxvw/export-rel-url))

;; ;; org mode disable autoidentation (tree identation)
;; (setq org-adapt-identation nil)

;; ;; prevent to insert tabs/spaces after src lines and titles
;; (setq org-src-preserve-indentation t)

;; ;; ??
;; (setq org-src-tab-acts-natively nil)

;; orgmode

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)

;; structured templates for org-mode

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; rss feeds

(setq elfeed-feeds (quote
                    (("https://news.ycombinator.com/rss" tech hackernews)
                     ("https://blog.tartanllama.xyz/feed.xml" programming cpp)
                     ("https://linuxnewbieguide.org/feed/" tech linux))))
;;(setq browse-url-browser-function 'eww-browse-url)

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which key, C-h
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; ivy-rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minubuffer-history))
  :config
  ;; avoid start search with ^
  (setq ivy-initial-inputs-alist nil))

;; lsp-mode
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

;; rust
(use-package rust-mode
  :ensure t)

;; lsp-rust configuration
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-rls/
;; rustup update
;; rustup component add rls rust-analysis rust-src

;; C-x C-e : to evaluate regions
;; C-c C-c : in org mode, C-s ', C-s C-s
;; C-h f : it can describe a function

;; C-c - : org mode mini editor






;; eof

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(visual-fill-mode visual-fill-column visual-fill use-package orgmode doom-themes lsp-mode multiple-cursors all-the-icons powerline neotree fill-column-indicator bazel groovy-mode yaml yaml-mode haskell-mode elfeed json-mode rainbow-delimiters which-key ivy ivy-rich counsel treemacs)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
