

;; melpa and package stuff

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages '(
  doom-themes
  multiple-cursors
  all-the-icons
  powerline
  neotree
  fill-column-indicator
  bazel
  groovy-mode
  yaml
  yaml-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

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

;; FillColumnIndicator

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

(dev-mode)

;; orgmode-html stuff

;; questions/14684263/how-to-org-mode-image-absolute-path-of-export-html
;; questions/9807/org-mode-dont-change-relative-urls
(defun wvxvw/export-rel-url (path desc format)
  (cl-case format
    (html (format "<a href=\"%s\">%s</a>" path (or desc path)))
    (latex (format "\\href{%s}{%s}" path (or desc path)))
    (otherwise path)))

(eval-after-load "org"
  '(org-link-set-parameters "rel" :follow #'browse-url :export #'wvxvw/export-rel-url))


;;(set-face-font
;;  'default "-adobe-courier-medium-r-normal--14-*-75-75-m-90-iso8859-9")

;;(x-select-font nil t)
;;(dolist (font (x-list-fonts "*")) 
;;  (insert (format "%s\n" font)))

;; emacs tabs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode yaml groovy-mode bazel doom-themes multiple-cursors all-the-icons powerline neotree fill-column-indicator)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
