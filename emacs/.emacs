;; packages ...
;; doom
;; doom-themes
;; multiple-cursors
;; autopair
;; M-x all-the-icons-install-fonts
;; jedi-direxes
;; go-mode
;; compnay-go
;; go-flycheck
;; fill-column-indicator

;; melpa stuff
(require 'package)
(add-to-list 'package-archives
             (cons "mel pa" "https://melpa.org/packages/") t)
(package-initialize)

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
;;(global-display-line-numbers-mode)
(define-key read-expression-map (kbd "TAB") #'lisp-complete-symbol)

;; doom-themes
(require 'doom-themes)
(load-theme 'doom-one t)

;; font
(cond
 ((eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :family "Monospace" :height 90))
 ((eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Monaco" :height 100)))
;; (set-face-attribute 'default nil :height 100)

(set-background-color "black")

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

;; parens
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
;; (setq powerline-color1 "grey22")
;; (setq powerline-color2 "grey40")

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
(eval-after-load "neotree"
  '(add-to-list 'window-size-change-functions
                (lambda (frame)
                  (let ((neo-window (neo-global--get-window)))
                    (unless (null neo-window)
                      (setq neo-window-width (window-width neo-window)))))))
(setq neo-window-fixed-size nil)

;; autopair
(require 'autopair)
(autopair-global-mode) 

;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;; go
;; https://github.com/nsf/gocode
(require 'company)
(require 'company-go)
(add-hook 'go-mode-hook (lambda ()
                          (setq-default)
                          (setq tab-width 4)
                          (setq standard-indent 4)
                          (setq indent-tabs-mode nil)
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

;; flycheck
(require 'flycheck)
(global-flycheck-mode)

;; https://www.emacswiki.org/emacs/FillColumnIndicator
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

;; https://seagle0128.github.io/doom-modeline/