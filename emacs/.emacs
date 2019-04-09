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
  (set-face-attribute 'default nil :family "Monospace" :height 90))
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
;;(set-face-background 'show-paren-match-face "#aaaaaa")
;;(set-face-attribute 'show-paren-match-face nil
;;		    :weight 'bold :underline nil :overline nil :slant 'normal)
;;(set-face-foreground 'show-paren-mismatch-face "red")
;;(set-face-attribute 'show-paren-mismatch-face nil
;;                    :weight 'bold :underline t :overline nil :slant 'normal)

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
;; (require 'package)
;; (package-initialize)
;; ;; (add-to-list 'package-archives
;; ;;              '("melpa" . "http://melpa.milkbox.net/packages/"))
;; ;; (add-to-list 'package-archives
;; ;;              '("elpa.gnu.org" . "https://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa.org" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("stable.melpa.org" . "http://stable.melpa.org/packages/") t)


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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-scrollbar-bg ((t :background "lightgray")))
 '(company-scrollbar-fg ((t :background "lightgray")))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))

;; jedi
;; http://tkf.github.io/emacs-jedi/latest/
;; (require 'jedi)
;; (add-to-list 'ac-sources 'ac-source-jedi-direct)

;; (defun readpp (filepath)
;;   (with-temp-buffer
;;     (insert-file-contents filepath)
;;     (split-string (buffer-string) "\n" t)))

;; (defun pythonizer ()
;;   ;;'jedi-setup
;;   ;;(setq jedi:server-args
;;   ;;      '("--sys-path" "/home/valiant/Desktop/package1"))
;;   (message "Reverting `%s'..." (buffer-name))
;;   )

;;(add-hook 'python-mode-hook 'pythonizer )
;;(setq jedi:complete-on-dot t)
;; M-x jedi:install-server RET

;; whitespace
;; (setq whitespace-style '(face tabs empty lines-tail trailing))
;; (global-whitespace-mode t)
;; (setq r (whitespace-looking-back whitespace-empty-at-eob-regexp (+ 1 whitespace-point)))
;; ;;(setq whitespace-newline nil)
;; (setq whitespace-empty-at-bob-regexp nil)
;; (setq whitespace-empty-at-eob-regexp nil)
;; (setq whitespace-tab nil)

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

;; powerline
(require 'powerline)
(powerline-default-theme)
(setq powerline-arrow-shape 'arrow)
(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")

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

;; flycheck
;;(package-install 'flycheck)
;;(global-flycheck-mode)

;; initializations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(add-hook
 'after-init-hook
 '(lambda ()
    ;; package initializations
    ))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun python-load-environment ()
  (interactive)

  (defun build-path-from-list (&rest relatives)
    (let (absolute)
      (dolist (relative (butlast relatives))
        (setq absolute (concat absolute (file-name-as-directory relative))))
      (setq absolute (concat absolute (car (last relatives))))
      absolute
      )
    )
  
  (require 'company-jedi)
  
  (add-to-list 'company-backends 'company-jedi)
  (autoload 'jedi:setup "jedi" nil t)
  
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)
  (setq jedi:get-in-function-call--d 0)
  (setq jedi:doc-mode 'rst-mode)
  
  (setq jedi-server-path (build-path-from-list user-emacs-directory "jedi-server"))
  (setq jedi-server-path (expand-file-name jedi-server-path))
  (setq jedi-server-script (build-path-from-list jedi-server-path "jediepcserver.py"))
  (setq jedi-server-bin (build-path-from-list jedi-server-path "bin" "jediepcserver"))
  
  ;; install jedi server on custom location (if doesn't exists)
  (unless (file-exists-p jedi-server-script)
    (with-temp-buffer
      (setq install-command (format "pip install --upgrade -t %s %s"
                                    jedi-server-path
                                    jedi:source-dir))
      (shell-command install-command "*Messages*" "*Messages*")
      (message "jedi server ... installing")
      (message "jedi server ... installed")
      )
    )

  ;; update pythonpath
  (setenv "PYTHONPATH"
          (concat jedi-server-path
                  (concat path-separator (getenv "PYTHONPATH"))))
  
  (setq python-sys-paths '())

  ;; from .projectile, custom paths takes precedence
  (setq projectile-file-path (locate-dominating-file default-directory ".projectile"))
  (when projectile-file-path
    (with-temp-buffer
      (setq projectile-file-path (expand-file-name projectile-file-path))
      (cd projectile-file-path)
      (setq projectile-file (build-path-from-list projectile-file-path ".projectile"))
      (setq projectile-file (expand-file-name projectile-file))
      (insert-file-contents projectile-file)
      (let ((paths (split-string (buffer-string) "\n" t)))
        (dolist (path paths)
          (setq path (expand-file-name path))
          (message "Appended projectile file to jedi:server-args --sys.path %s" path)
          (setq python-sys-paths (append python-sys-paths '("--sys-path") (list path)))
          )
        )
      )
    )

  ;; from pythonpath
  (let ((pythonpath (parse-colon-path (getenv "PYTHONPATH"))))
    (when pythonpath
      (dolist (path pythonpath)
        (setq path (expand-file-name path))
        (message "Appended from pythonpath to jedi:server-args --sys.path %s" path)
        (setq python-sys-paths (append python-sys-paths '("--sys-path") (list path)))
        )
      )
    )

  ;; from python sys.path
  (with-temp-buffer
    (shell-command "python -c \"import sys; print(':'.join(sys.path)[1:])\"" t "*Messages*")
    (setq sys-paths (parse-colon-path (buffer-string)))
    (dolist (path sys-paths)
      (message "Appended from python sys.path to jedi:server-args --sys.path %s" path)
      (setq python-sys-paths (append python-sys-paths '("--sys-path") (list path)))
      )
    )

  ;; jedi-server
  (message "Appended from jedi-server to jedi:server-args --sys.path %s" jedi-server-path)
  (setq python-sys-paths (append python-sys-paths '("--sys-path") (list jedi-server-path)))

  ;; add paths
  (setq jedi:server-args python-sys-paths)

  ;; configure server
  (message "jedi server ... starting from %s" jedi-server-bin)
  (setq jedi:server-command (list "python" jedi-server-bin))

  ;; key bindings
  (local-set-key (kbd "C-s d") 'jedi:show-doc)
  (local-set-key (kbd "C-c t") 'jedi:complete)
  (local-set-key (kbd "C-c .") 'jedi:goto-definition)
  )

(add-hook 'python-mode-hook 'python-load-environment)
