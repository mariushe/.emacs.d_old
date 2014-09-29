

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Add information about which column the cursor is currently on in the mode line
(column-number-mode)

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
                      gitignore-mode
                      butler))

(require 'butler)

(require 'nexus)

(require 'org-trello)



(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; My own

(require 'personal-settings)

(global-set-key (kbd "C-x C-j") 'butler-status)

(global-set-key (kbd "C-x C-n") 'nexus-search-keyword)



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

;; Move windows, even in org-mode
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)

;; Enlarge window horizontally
(global-set-key (kbd "C-x 9") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x 8") 'shrink-window-horizontally)

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

;; Agressive Auto-indentation
(defun endless/indent-defun ()
  "Indent current defun."
  (interactive)
  (let ((l (save-excursion (beginning-of-defun 1) (point)))
        (r (save-excursion (end-of-defun 1) (point))))
    (indent-region l r)))

(defun endless/activate-aggressive-indent ()
  "Locally add `endless/indent-defun' to `post-command-hook'."
  (add-hook 'post-command-hook
            #'endless/indent-defun nil 'local))

(add-hook 'clojure-mode-hook
          #'endless/activate-aggressive-indent)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(require 'appearance)

