;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; tabs
(setq tab-width 2)
(setq tab-stop-list (number-sequence 2 200 2))
(setq indent-tabs-mode nil)

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

;;fix the bug that shift-up doesn't send the right escape sequence in terminal
(if (equal "xterm-256color" (tty-type)) (define-key input-decode-map "\e[1;2A" [S-up]))
;; to make windmove work in tmux
(if (equal "screen-256color" (tty-type)) 
    (progn
    (define-key input-decode-map "\e[1;2D" [S-left])  
    (define-key input-decode-map (kbd "M-[ 1 ; 2 C") [S-right])  
    (define-key input-decode-map (kbd "M-[ 1 ; 2 B")[S-down])  
    (define-key input-decode-map "\e[1;2A" [S-up])  
    (define-key input-decode-map "\e[1;6A" [S-C-up])
    (define-key input-decode-map "\e[1;6B" [S-C-down])
    )
)

;; enable windmove
 (when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))

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

;; org-babel setup

(when (locate-file "ob" load-path load-suffixes)
					   (require 'ob)
					   (require 'ob-tangle)
					   (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

					   (org-babel-do-load-languages
					    'org-babel-load-languages
					    '((emacs-lisp . t)
					      (clojure . t)
					      (js . t)
					      (ruby . t)))


					   (defun org-babel-execute:clojure (body params)
					     "Evaluate a block of Clojure code with Babel."
					     (let* ((result (nrepl-send-string-sync body (nrepl-current-ns)))
						    (value (plist-get result :value))
						    (out (plist-get result :stdout))
						    (out (when out
							   (if (string= "\n" (substring out -1))
							       (substring out 0 -1)
							     out)))
						    (stdout (when out
							      (mapconcat (lambda (line)
									   (concat ";; " line))
									 (split-string out "\n")
									 "\n"))))
					       (concat stdout
						       (when (and stdout (not (string= "\n" (substring stdout -1))))
							 "\n")
						       ";;=> " value)))

					   (provide 'ob-clojure)

					   (setq org-src-fontify-natively t)
					   (setq org-confirm-babel-evaluate nil))

;; setup package (works with el-get-build-elpa-whatever)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("tromey" . "http://tromey.com/elpa/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; El-get distributed setup
;; local sources
(setq el-get-sources '((:name magit 
			      :after (global-set-key (kbd "C-x C-o") 'magit-status))
		       (:name enh-ruby-mode
			      :after (when (string= system-name "ma.sdf.org") (setq enh-ruby-program "ruby193")))
		       (:name elisp-format 
			      :features elisp-format)))
		
;; canonical list
(setq my-packages (append '(el-get ack yasnippet ruby-compilation enh-ruby-mode color-theme-solarized
				   auto-complete auto-complete-emacs-lisp auto-complete-yasnippet
				   anything anything-rcodetools emacs-w3m yaml-mode windcycle
				   go-mode 
				   coffee-mode markdown-mode less-css-mode scss-mode mustache-mode
				   clojure-mode clojurescript-mode midje-mode nrepl htmlize paredit kibit-mode
				   epresent org-html5presentation org-impress-js org-s5
				   ) 
			  (mapcar 'el-get-source-name el-get-sources)))
(el-get-cleanup my-packages)
(el-get 'sync my-packages)

## temporary until recipe merge
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))

;; turn on paredit for clojure and Lisp
(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))

;; load theme
(add-to-list 'custom-theme-load-path "el-get/color-theme-solarized")
(load-theme 'solarized-dark t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

