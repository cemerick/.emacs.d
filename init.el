;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.


;; Initializing ELPA (package.el)

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("tromey" . "http://tromey.com/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))


;; Refresh the packages descriptions
;; (unless package-archive-contents 
;;   (package-refresh-contents))

;; making sure we're using the repo-installed org-mode instead of the built-in one

(add-to-list 'load-path "~/.emacs.d/el-get/package/elpa/org-20130311")
(add-to-list 'load-path "~/.emacs.d/el-get/package/elpa/org-plus-contrib-20130311")

(setq package-load-list '(all))     ;; List of packages to load
(unless (package-installed-p 'org)  ;; Make sure the Org package is
  (package-install 'org))           ;; installed, install it if not
(package-initialize)          

;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files

(require 'org-install)
(require 'ob-tangle)

;; load up all literate org-mode files in this directory

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-velocity)))
 '(org-velocity-bucket "/Users/danielszmulewicz/Dropbox/notes/cheat-sheet.org"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
