;; look and feel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(display-time)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(column-number-mode 1)
(line-number-mode 1)
(global-linum-mode -1)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(savehist-mode 1)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; (setq mouse-wheel-progressive-speed nil)
;; (setq mouse-wheel-follow-mouse 't)
;; (setq scroll-step 5)
(define-key read-expression-map (kbd "TAB") #'lisp-complete-symbol)

;; theme

(load-theme 'wombat 1)
(set-face-attribute 'fringe nil :background nil)

(defun custom-configure-frame (frame)
  (modify-frame-parameters frame '((vertical-scroll-bars . nil)
                                   (horizontal-scroll-bars . nil)
                                   (toggle-scroll-bar))))
(add-hook 'after-make-frame-functions 'custom-configure-frame)
(if (display-graphic-p)
    (progn
      (setq color "gray7")
      (set-background-color color)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (when (display-graphic-p frame)
                    (with-selected-frame frame
                      (set-background-color color)
                      (set-face-attribute 'fringe nil :background nil))))))
  (progn
    (setq color "color-232")
    (set-background-color color)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (when (display-graphic-p frame)
                  (with-selected-frame frame
                    (set-background-color color)
                    (set-face-attribute 'fringe nil :background nil))))))
  )

;; font

(cond
 ((eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :family "Monospace" :height 80))
 ((eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Monaco" :height 90)))

;; editor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; some bindings
(global-set-key "\C-l" 'goto-line)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; parens
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match-face "#aaaaaa")
(set-face-attribute 'show-paren-match-face nil
		    :weight 'bold :underline nil :overline nil :slant 'normal)
(set-face-foreground 'show-paren-mismatch-face "red")
(set-face-attribute 'show-paren-mismatch-face nil
                    :weight 'bold :underline t :overline nil :slant 'normal)

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
;;(setq create-lockfiles nil)

;; packages ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;(add-to-list 'load-path "~/.emacs.d")
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("elpa.gnu.org" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa.org" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("stable.melpa.org" . "http://stable.melpa.org/packages/") t)

;; packages configuration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; projectile
(require 'projectile)
(projectile-global-mode)

;; auto-complete
;;(require 'auto-complete-config)
;;(ac-config-default)
;;(setq ac-show-menu-immediately-on-auto-complete t)

;; company
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  (lambda ()
    (set-face-attribute
     'company-preview
     nil
     :background (face-attribute 'company-preview-common :background))))
(custom-set-faces
 '(company-preview
   ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-selection
   ((t (:background "steelblue" :foreground "white"))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection))))
 '(company-scrollbar-fg
   ((t :background "lightgray")))
 '(company-scrollbar-bg
   ((t :background "lightgray")))
 )

;; jedi
;; http://tkf.github.io/emacs-jedi/latest/
(require 'jedi)
(add-to-list 'ac-sources 'ac-source-jedi-direct)

(defun readpp (filepath)
  (with-temp-buffer
    (insert-file-contents filepath)
    (split-string (buffer-string) "\n" t)))

(defun pythonizer ()
  'jedi-setup
  (setq jedi:server-args
        '("--sys-path" "/home/valiant/Desktop/package1"))
  )

(add-hook 'python-mode-hook 'pythonizer )
(setq jedi:complete-on-dot t)
;; M-x jedi:install-server RET

;; whitespace
(setq whitespace-style '(face tabs empty lines-tail trailing))
(global-whitespace-mode t)
(setq r (whitespace-looking-back whitespace-empty-at-eob-regexp (+ 1 whitespace-point)))
;;(setq whitespace-newline nil)
(setq whitespace-empty-at-bob-regexp nil)
(setq whitespace-empty-at-eob-regexp nil)
(setq whitespace-tab nil)

;;fill-column-indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-column 79)
(setq fci-rule-width 1)
(setq fci-rule-color "grey22")

(defvar-local company-fci-mode-on-p nil)
(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))
(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))
(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

;; org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-support-shift-select 'always)

(require 'powerline)
(powerline-default-theme)
(setq powerline-arrow-shape 'arrow)
(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")

;; initializations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(add-hook
 'after-init-hook
 '(lambda ()
    ;; package initializations
    ))
