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

;; enable windmove
 (when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))

;;fix the bug that shift-up doesn't send the right escape sequence in term
(if (equal "xterm-256color" (tty-type)) (define-key input-decode-map "\e[1;2A" [S-up]))

;; turn visual mode for text files
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

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

;; canonical list
(setq my-packages (append '(el-get ack yasnippet ruby-compilation ruby-end ruby-mode
				   auto-complete auto-complete-emacs-lisp auto-complete-yasnippet
				   anything anything-rcodetools emacs-w3m yaml-mode windcycle) 
			  (mapcar 'el-get-source-name el-get-sources)))

(el-get-cleanup my-packages)
(el-get 'sync my-packages)
