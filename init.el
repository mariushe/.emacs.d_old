

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; ref: https://github.com/magnars/.emacs.d

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; ref: http://clojure-doc.org/articles/tutorials/emacs.html
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

 (add-to-list 'package-archives 
              '("melpa" . "http://melpa.milkbox.net/packages/"))

(add-to-list 'package-archives 
             '("org" . "http://orgmode.org/elpa/") 
             t)

(package-initialize)

(defvar my-packages '(better-defaults
                      clojure-mode
                      clojure-test-mode
                      cider
                      paredit
                      rainbow-delimiters
                      git-timemachine
                      magit
                      git-commit-mode
                      gitconfig-mode
                      gitignore-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; My own
(setq is-mac (equal system-type 'darwin))
(when is-mac (require 'mac))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.

;; Accept theme to run lisp code
 '(custom-safe-themes (quote ("9527feeeec43970b1d725bdc04e97eb2b03b15be982ac50089ad223d3c6f2920" default)))
;; Makes emacs run fullscreen
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
;;

;;;; CLOJURE MODE

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)

;;;;;;;;;;;;;;;;;

 ;; If there is more than one, they won't work right.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; KEY-BININGS
(require 'find-file-in-project)
;; Find file in project
(global-set-key (kbd "C-x o") 'find-file-in-project)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status)
(autoload 'magit-status "magit")

;; load-magit-log-when-committing-mode
(define-minor-mode load-magit-log-when-committing-mode
  "dummy")

;; the hook
(defun show-magit-log-hook ()
  (cd "..")
  (magit-log)
  (switch-to-buffer-other-window "COMMIT_EDITMSG"))

;; add the hook
(add-hook 'load-magit-log-when-committing-mode-hook 'show-magit-log-hook)

;; load the mode for commit message
(add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . load-magit-log-when-committing-mode))
;;(eval-after-load 'magit '(require 'setup-magit))

;; Move windows, even in org-mode
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)

;; Deletes file as well
(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key "\C-p" 'grunt)

(setq grunt-cmd "grunt make-js --config ~/grunt.conf")
 

(defun grunt ()
  "Run grunt"
  (interactive)
  (let* ((grunt-buffer (get-buffer-create "*grunt*"))
        (result (call-process-shell-command grunt-cmd nil grunt-buffer t))
        (output (with-current-buffer grunt-buffer (buffer-string))))
    (cond ((zerop result)
           (message "Grunt completed without errors"))
          (t
           (message nil)
           (split-window-vertically)
           (set-window-buffer (next-window) grunt-buffer)))))

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(require 'appearance)

