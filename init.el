;; load theme
(load-theme 'tango-dark t)

;;disable menu bar
(menu-bar-mode 0)

;; Recent file list
(require 'recentf)
(recentf-mode 1)

;; Winner mode
(winner-mode 1)

;; Enable ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; make recentf use ido
(defun ido-recentf-open () 
  "Use `ido-completing-read' to \\[find-file] a recent file" 
  (interactive) 
  (if (find-file (ido-completing-read "Find recent file: " recentf-list)) 
      (message "Opening file...") 
    (message "Aborting")))


;; El-get installation routine

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless 
    (require 'el-get nil t) 
  (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el" 
		(lambda (s) 
		  (let (el-get-master-branch)
		  (goto-char (point-max)) 
		  (eval-print-last-sexp)))))

;; El-get distributed setup
;; local sources
(if (not (string-match "netbsd" system-configuration)) 
    (setq el-get-sources '((:name magit 
				  :after (global-set-key (kbd "C-x C-o") 'magit-status)) 
			   (:name elisp-format 
				  :features elisp-format))))

(setq my-packages (append '(el-get ack switch-window yasnippet ruby-compilation ruby-end ruby-mode
				   auto-complete auto-complete-emacs-lisp auto-complete-yasnippet
				   anything anything-rcodetools emacs-w3m) 
			  (mapcar 'el-get-source-name el-get-sources)))


;; El-get cleanup function
(defun el-get-cleanup (packages) 
  "Removes packages absent in the list argument function 'packages. Useful, for example, when we want to remove all packages not explicitly declared in the emacs init file." 
  (let* ((packages-to-keep (el-get-dependencies (mapcar 'el-get-as-symbol packages))) 
	 (packages-to-remove (set-difference (mapcar 'el-get-as-symbol
						     (el-get-list-package-names-with-status
						      "installed")) packages-to-keep))) 
    (mapc 'el-get-remove packages-to-remove)))
(el-get-cleanup my-packages)
(el-get 'sync my-packages)
