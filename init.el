;;; init.el --- Where all the magic begins

;; Cask initialization (if required files are found)
;; Pallet's main job, though, is to add and delete package references from your Cask file as you install and delete them using the built-in Emacs package management system. It does this automatically and silently.
  (let 
      ((cask-file "~/.cask/cask.el")
       (cask-spec "~/.emacs.d/Cask"))
    (when (and (file-exists-p cask-file) (file-exists-p cask-spec))
      (require 'cask cask-file)
      (cask-initialize)
      (require 'pallet)))
     
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.

(require 'org-install)
(require 'ob-tangle)

;; load up all literate org-mode files in this directory

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

;;; init.el ends here
