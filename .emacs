;; -*- coding: utf-8 -*-
;; atejeda
 
;;;;
;; Tabs and identation
;;;;
(setq-default c-basic-offset 4) ;; c style
(setq-default py-indent-offset 4) ;; python
(setq sgml-basic-offset 4) ;; html mode
(setq ruby-indent-level 2) ;; ruby indentation
(setq scala-indent-level 4) ;; scala indentation
(setq standard-indent 4) ;; change indent from 2 to 4 
(setq c-default-style "linux" c-basic-offset 4) ;; all c-based code - not gnu style
(setq scroll-step 1) ;; scroll line by line
(setq-default indent-tabs-mode nil) ;;  no tabs char in files
(setq make-backup-files nil) ;; no backup
(setq auto-fill-mode 1)
(setq next-line-add-newlines nil)
 
 
;;;;
;; General
;;;;
(setq dabbrev-case-replace nil) ;; expanding words dynamically
(define-key global-map [f9] 'bookmark-jump) ;; bookmark set
(define-key global-map [f10] 'bookmark-set) ;; bookmark jump
(setq bookmark-save-flag 1) ;; save bookmark at any bookmark-action
 
;;;;
;; Backup
;;;;
(setq make-backup-files t) ;; Enable backup files.
(setq version-control t) ;; Enable versioning with default values
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/")))) ;; Save all backup file in this directory.
(setq backup-by-copying t    ;; Don't delink hardlinks
      delete-old-versions t  ;; Clean up the backups
      version-control t      ;; Use version numbers on backups,
      kept-new-versions 3    ;; keep some new versions
      kept-old-versions 2)   ;;; and some old ones, too
 
;;;;
;; Theme
;;;;
(global-font-lock-mode 1)
(set-cursor-color "red")
(set-mouse-color "goldenrod")
(set-background-color "black") ;; Set emacs background color
(set-foreground-color "white") ;; Set emacs foreground color
(set-border-color "green") ;; Set emacs border color
(set-face-attribute 'default nil :height 100)
(display-time)
(line-number-mode 1)
(column-number-mode 1)
 
;;;;
;; Frame Configuration
;;;;
(setq default-frame-alist
      (append default-frame-alist
              '((foreground-color . "white")
                (background-color . "black")
                (cursor-color . "red")
                (set-mouse-color "goldenrod")
                )))
 
;;;;
;; Custom Keys
;;;;
(global-set-key "\C-l" 'goto-line)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Have TAB expand in M-:
(define-key read-expression-map (kbd "TAB") #'lisp-complete-symbol)
 
;; if the font is found, use it.
;;(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  ;;'(column-number-mode t)
  ;;'(display-time-mode t))
 ;;)
;;(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 ;;'(default ((t (:inherit nil 
 ;;    :stipple nil 
 ;;    :background "black" 
 ;;    :foreground "white" 
 ;;    :inverse-video nil :box nil 
 ;;    :strike-through nil 
 ;;    :overline nil 
 ;;    :underline nil 
 ;;    :slant normal 
 ;;    :weight normal 
 ;;    :height 70 
 ;;    :width normal 
 ;;    :foundry "unknown" 
 ;;    :family "Monospace")))))
 
;;;;
;; no categorisable settings
;;;;
(defun with-no-warnings (&rest body)

;; Theme for fedora using monaco font
(car (last body)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(display-time-mode t)
 '(font-use-system-font t)
 '(size-indication-mode t))

;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(default ((t (:family "Monaco" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
