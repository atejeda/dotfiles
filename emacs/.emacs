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

;; theme

(menu-bar-mode -1) 
(tool-bar-mode -1) 
(toggle-scroll-bar -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (whitespace-cleanup-mode fill-column-indicator spaceline-all-the-icons sr-speedbar smart-mode-line-powerline-theme org neotree dired-k autopair auto-complete all-the-icons)))
 '(send-mail-function (quote smtpmail-send-it))
 '(tool-bar-mode nil))

(set-border-color "green") ;; Set emacs border color
(display-time)
(line-number-mode 1)
(column-number-mode 1)
;;(global-linum-mode t)

(setq default-frame-alist
      (append default-frame-alist
              '((foreground-color . "white")
                (background-color . "black")
                (cursor-color . "red")
                (set-mouse-color "goldenrod")
                )))

;;(if (display-graphic-p)
;;    (setq initial-frame-alist
;;         '(
;;            (tool-bar-lines . 1)
;;            (width . 106)
;;           (height . 60)
;;            (background-color . "#131415")
;;            (left . 50)
;;            (top . 50)))
;;  (setq initial-frame-alist '( (tool-bar-lines . 1))))
;;(setq default-frame-alist initial-frame-alist)

;; faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monospace" :slant normal :weight normal :height 90 :width normal)))))

(if (eq system-type 'darwin)
    (custom-set-faces
     '(default ((t (:family "Monaco" :slant normal :weight normal :height 100 :width normal)))))
  )

;; tabs and identation

(setq-default indent-tabs-mode nil) ;;  no tabs char in files
(setq-default c-basic-offset 4) ;; c style
(setq-default py-indent-offset 4) ;; python
(setq standard-indent 4) ;; change indent from 2 to 4 
(setq c-default-style "linux" c-basic-offset 4) ;; all c-based code - not gnu style
(setq scroll-step 1) ;; scroll line by line
(setq make-backup-files nil) ;; no backup
(setq auto-fill-mode 1)
(setq next-line-add-newlines nil)

;; backup

(setq make-backup-files t) ;; Enable backup files.
(setq version-control t) ;; Enable versioning with default values
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/")))) ;; Save all backup file in this directory.
(setq backup-by-copying t    ;; Don't delink hardlinks
      delete-old-versions t  ;; Clean up the backups
      version-control t      ;; Use version numbers on backups,
      kept-new-versions 3    ;; keep some new versions
      kept-old-versions 2)   ;;; and some old ones, too

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
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; packages configurations

; lines

(sml/setup)
(setq sml/theme 'dark)

(require 'powerline)
(powerline-default-theme)
(setq powerline-arrow-shape 'arrow)
;;(setq powerline-color1 "grey22")
;;(setq powerline-color2 "grey40")

(require 'all-the-icons)

; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

; org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Set the neo-window-width to the current width of the
;; neotree window, to trick neotree into resetting the
;; width back to the actual window width.
;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
(eval-after-load "neotree"
  '(add-to-list 'window-size-change-functions
                (lambda (frame)
                  (let ((neo-window (neo-global--get-window)))
                    (unless (null neo-window)
                      (setq neo-window-width (window-width neo-window)))))))

; neotree
(setq neo-window-fixed-size nil)

; one dark pro background color
;;(load-theme 'one-dark t)
(set-background-color "#101113") ; supposed to be 141718

; multiple cursors
; http://emacsrocks.com/e13.html
(require 'multiple-cursors)

(require 'whitespace)
 (setq whitespace-style '(face empty tabs lines-tail trailing))
 (global-whitespace-mode t)
