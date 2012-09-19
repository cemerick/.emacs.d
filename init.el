;;disable menu bar
(menu-bar-mode 0)

;; Recent file list
(require 'recentf) 
(recentf-mode 1)

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

;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration) 
    (set-face-font 'default "Monaco-13"))


;; El-get installation routine

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless 
    (require 'el-get nil t) 
  (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el" 
		(lambda (s) 
		  (goto-char (point-max)) 
		  (eval-print-last-sexp))))

(el-get 'sync)

;; El-get distributed setup
;; local sources
(if (not (string-match "netbsd" system-configuration)) 
    (setq el-get-sources '((:name magit 
			      :after (global-set-key (kbd "C-x C-z") 'magit-status))
		       (:name elisp-format)
)))

(setq my-packages (append '(el-get switch-window yasnippet ruby-compilation ruby-electric ruby-end
				   ruby-mode ruby-test ruby-test-mode) 
			  (mapcar 'el-get-source-name el-get-sources)))
(el-get 'sync my-packages)
