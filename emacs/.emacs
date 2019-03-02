;; packages
;; M-x package-refresh-contents RET
;; M-x package-install RET org RET
;; M-x package-install RET auto-complete RET
;; M-x package-install RET autopair RET
;; M-x package-install RET dired-k RET
;; M-x package-install RET neotree RET
;; M-x package-install RET all-the-icons
;; M-x all-the-icons-install-fonts
;; M-x package-install RET smart-mode-line RET
;; M-x package-install RET powerline RET
;; M-x package-install RET smart-mode-line-powerline-theme RET
;; M-x package-install RET spaceline RET
;; M-x package-install RET spaceline-all-the-icons RET
;; M-x package-install RET fill-column-indicator RET
;; M-x package-install multiple-cursors
;; M-x package-install one-themes
;; M-x package-install whitespace

;; look and feel

(display-time)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(column-number-mode 1)
(line-number-mode 1)
(global-linum-mode -1)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

(load-theme 'wombat 1)
(set-face-attribute 'fringe nil :background nil)

(cond
  ((eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :family "Monospace" :height 90))
  ((eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Monaco" :height 100)))

(if (display-graphic-p)
    (progn
      (setq color "gray7")
      (set-background-color color)
      (add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (display-graphic-p frame)
              (with-selected-frame frame
                (set-background-color color))))))
    (progn
      (setq color "color-232")
      (set-background-color color)
      (add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (display-graphic-p frame)
              (with-selected-frame frame
                (set-background-color color))))))
)

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

;; backup

(setq make-backup-files t)
(setq version-control t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
(setq backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 3
      kept-old-versions 2)

;; custom keys

(global-set-key "\C-l" 'goto-line)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Have TAB expand in M-:
(define-key read-expression-map (kbd "TAB") #'lisp-complete-symbol)

;; melpa
;; M-x package-refresh-contents
(require 'package)
(package-initialize)

;; packages configurations

;; lines

;;(sml/setup)
;;(setq sml/theme 'dark)

(require 'powerline)
(powerline-default-theme)
(setq powerline-arrow-shape 'arrow)
(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")

(require 'all-the-icons)

;; org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-support-shift-select 'always)

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

;; multiple cursors
;; http://emacsrocks.com/e13.html
(require 'multiple-cursors)

;; whitespace
;; https://www.emacswiki.org/emacs/WhiteSpace
(require 'whitespace)
(setq whitespace-style '(face tabs empty lines-tail trailing))
(global-whitespace-mode t)
(setq r (whitespace-looking-back whitespace-empty-at-eob-regexp (+ 1 whitespace-point)))
;;(setq whitespace-newline nil)
(setq whitespace-empty-at-bob-regexp nil)
(setq whitespace-empty-at-eob-regexp nil)

;; fill column indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-column 79)
(setq fci-rule-width 1)
(setq fci-rule-color "white")