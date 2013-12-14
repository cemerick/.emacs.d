<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Customizations</a></li>
<li><a href="#sec-2">2. Backup and autosave</a></li>
<li><a href="#sec-3">3. UTF-8</a></li>
<li><a href="#sec-4">4. Tabs</a></li>
<li><a href="#sec-5">5. Menu bar</a></li>
<li><a href="#sec-6">6. Winner mode</a></li>
<li><a href="#sec-7">7. Windmove</a></li>
<li><a href="#sec-8">8. Recent file list</a></li>
<li><a href="#sec-9">9. Ido</a>
<ul>
<li><a href="#sec-9-1">9.1. recentf</a></li>
</ul>
</li>
<li><a href="#sec-10">10. Uniquify</a></li>
<li><a href="#sec-11">11. Spelling</a></li>
<li><a href="#sec-12">12. Key remappings</a></li>
<li><a href="#sec-13">13. Misc.</a></li>
<li><a href="#sec-14">14. Color theme</a></li>
<li><a href="#sec-15">15. Splash screen</a></li>
<li><a href="#sec-16">16. Mac OS X customizations</a></li>
<li><a href="#sec-17">17. Text-files hooks</a></li>
<li><a href="#sec-18">18. Deft</a></li>
<li><a href="#sec-19">19. Org-mode</a>
<ul>
<li><a href="#sec-19-1">19.1. Location of default notes files</a></li>
<li><a href="#sec-19-2">19.2. Capture templates</a></li>
<li><a href="#sec-19-3">19.3. Project configuration</a></li>
<li><a href="#sec-19-4">19.4. Org-babel</a></li>
</ul>
</li>
<li><a href="#sec-20">20. Ctags</a></li>
<li><a href="#sec-21">21. Paredit</a></li>
<li><a href="#sec-22">22. Pretty Lambda</a></li>
<li><a href="#sec-23">23. Slime</a></li>
<li><a href="#sec-24">24. Clojure</a>
<ul>
<li><a href="#sec-24-1">24.1. nrepl</a></li>
<li><a href="#sec-24-2">24.2. rainbow delimiters</a></li>
<li><a href="#sec-24-3">24.3. pretty lambda and co</a></li>
</ul>
</li>
<li><a href="#sec-25">25. w3m</a></li>
<li><a href="#sec-26">26. mu4e</a></li>
<li><a href="#sec-27">27. ElDOC</a></li>
<li><a href="#sec-28">28. Terminal hotkey</a></li>
<li><a href="#sec-29">29. Flycheck</a></li>
<li><a href="#sec-30">30. nyan-mode</a></li>
<li><a href="#sec-31">31. web-mode</a></li>
<li><a href="#sec-32">32. elisp</a>
<ul>
<li><a href="#sec-32-1">32.1. elisp-slime-nav</a></li>
<li><a href="#sec-32-2">32.2. elisp-format</a></li>
</ul>
</li>
<li><a href="#sec-33">33. Global keys</a></li>
</ul>
</div>
</div>

# Customizations

We want to save our customizations in a dedicated file. We will check
the existence of such a file so as not to cause errors on a vanilla
emacs installation. Please create it yourself if you don't want your
customizations to be saved in the init file. (default behavior)

    (let 
        ((customization-file "~/.emacs.d/emacs-custom.el"))
      (when (file-exists-p customization-file)
        (setq custom-file customization-file)
        (load custom-file)))

# Backup and autosave

Keep Backup and Auto-save Files Out of the Way
<http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/>

    ;; store all backup and autosave files in the tmp dir
    (setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

# UTF-8

We want to have default utf-8 everything.

    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)

# Tabs

Two-width tab stops, like I'm used to

    (setq tab-width 2)
    (setq tab-stop-list (number-sequence 2 200 2))
    (setq indent-tabs-mode nil)

# Menu bar

Disable menu-bar, invoke if necessary with \`menu-bar-mode

    (menu-bar-mode -1)

# Winner mode

    (winner-mode 1)

# Windmove

    (when (fboundp 'windmove-default-keybindings)
         (windmove-default-keybindings))

Make windmove work in org-mode:

    (add-hook 'org-shiftup-final-hook 'windmove-up)
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right)

# Recent file list

    (require 'recentf)
    (recentf-mode 1)

# Ido

    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t)
    (ido-mode 1)

## recentf

Make recentf use ido

    
    (defun ido-recentf-open () 
      "Use `ido-completing-read' to \\[find-file] a recent file" 
      (interactive) 
      (if (find-file (ido-completing-read "Find recent file: " recentf-list)) 
          (message "Opening file...") 
        (message "Aborting")))

# Uniquify

    (require 'uniquify)
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)

# Spelling

    (setq ispell-program-name "aspell")
    (setq ispell-list-command "list")

# Key remappings

Fix the bug that shift-up doesn't send the right escape sequence in terminal

    (if (equal "xterm-256color" (tty-type)) (define-key input-decode-map "\e[1;2A" [S-up]))

To make windmove work in tmux

    
    (if (equal "screen-256color" (tty-type)) 
        (progn
        (define-key input-decode-map "\e[1;2D" [S-left])  
        (define-key input-decode-map (kbd "M-[ 1 ; 2 C") [S-right])  
        (define-key input-decode-map (kbd "M-[ 1 ; 2 B")[S-down])  
        (define-key input-decode-map "\e[1;2A" [S-up])  
        (define-key input-decode-map "\e[1;6A" [S-C-up])
        (define-key input-decode-map "\e[1;6B" [S-C-down])))

Not sure it is needed.

    (if (equal "daniels-imac.local" (system-name))
        (add-hook 'comint-mode-hook
                  (lambda ()               
                    (define-key comint-mode-map (kbd "M-n") 'comint-next-input))))

# Misc.

    (add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
    (when (string= system-name "ma.sdf.org") (setq enh-ruby-program "ruby193"))

    (add-hook 'css-mode-hook 'rainbow-mode)

# Color theme

    (load-theme 'zenburn t)

# Splash screen

    (setq inhibit-splash-screen t)

# Mac OS X customizations

Clipboard and kill ring

    (when (eq system-type 'darwin)
      (progn
        (turn-on-pbcopy)
        (setq default-frame-alist '((height . 44) (width . 120) (font . "Menlo-14") (top . 20) (left . 200)))
        (setq initial-frame-alist '((top . 10) (left . 30)))))

[Braces and square braces in emacs](http://stackoverflow.com/questions/3376863/unable-to-type-braces-and-square-braces-in-emacs)

    (setq mac-right-option-modifier nil
          mac-option-key-is-meta t
          x-select-enable-clipboard t)

    
    (defun reset-ui (&optional frame)
      (if frame
          (select-frame frame))
      (interactive)
      (smex-initialize)
      (load-theme 'solarized-dark t)
      (delete-other-windows)
      (set-cursor-color "deeppink")
      (set-face-background 'modeline-inactive "gray10")
      (if (window-system)
          (cond
           ((= 1050 (display-pixel-height)); 22" display
            (set-frame-size (selected-frame) 163 71)
            (set-frame-position (selected-frame) 0 -1050))
           ((= 1200 (display-pixel-height)); 24" display
            (set-frame-size (selected-frame) 163 76)
            (set-frame-position (selected-frame) 0 -1200))
           (t ; laptop runs 1440x900
            (set-frame-size (selected-frame) 163 53)
            (set-frame-position (selected-frame) 0 0))))
      (split-window-horizontally))
    
    ;;(add-hook 'after-make-frame-functions 'reset-ui)

# Text-files hooks

Turn visual mode for text files

    (add-hook 'text-mode-hook 'turn-on-visual-line-mode)

# Deft

    (setq deft-directory "~/Dropbox/notes")
    (setq deft-extension "org")
    (setq deft-text-mode 'org-mode)

# Org-mode

## Location of default notes files

    (let ((destination (if (file-exists-p "~/Dropbox")
                           "~/Dropbox/notes.org"
                         "~/notes.org")))
      (setq org-default-notes-file destination))    

## Capture templates

    
    (require 'org-element)
    
    (defun pn-get-headline ()
      (let* ((headlines (org-map-entries '(org-element-property :title (org-element-at-point)) t 'file)) 
             (headline (car headlines)) 
             (listoftags (org-map-entries '(org-element-property :tags (org-element-at-point)) t 'file))
             (tags (car listoftags)))
        (org-capture-put :title headline)
        (org-capture-put :tags tags)
        headline))
    
    (defun pn-filename_from_title ()
      (replace-regexp-in-string " " "-" (pn-get-headline)))
    
    (defun matching-post (title)
      (directory-files (pn-get-property :publishing-directory) nil (concat "[0-9]\\{4\\}-[0-9]+-[0-9]+-" title ".html")))
    
    (defun pn-postp (title)
      (matching-post title))
    
    (defun pn-date-from-file (title)
      (substring (car (matching-post title)) 0 10))
    
    (defun get-date (title)
      (if (pn-postp title)
          (pn-date-from-file title)
        (format-time-string "%Y-%m-%d")))
    
    (defun pn-capture-blog-path ()
      (let ((name (pn-filename_from_title)))
        (expand-file-name (format "%s-%s.org"
                                  (get-date name)
                                  name) "~/Dropbox/notes/blog")))
    
    (setq org-capture-templates  
    
          '(         
            ("b" 
             "Org to Blog entry" 
             plain 
             (file (pn-capture-blog-path)) 
             "#+BEGIN_HTML\n---\ntitle: %(org-capture-get :title)\nlayout: post\ntags: %(mapconcat 'identity (org-capture-get :tags) \" \")\n---\n#+END_HTML\n\n%F"
             :immediate-finish t
             :kill-buffer t
             )
    
            ("t" 
             "Todo" 
             entry 
             (file+headline "" "Task")
             "* TODO %?\n  %i\n  %a")
    
            ("i"
             "Idea")
    
            ("ia"
             "app idea"
             entry
             (file+headline "~/Dropbox/notes/ideas.org" "App ideas")
             "* %^{App idea (title)}\n %? \n%u"
             )
    
            ("ib"
             "blog idea"
             entry
             (file+headline "~/Dropbox/notes/ideas.org" "Blog ideas")
             "* %^{Blog idea (title)}\n %? \n%u"
             )
    
            ("it"
             "T-shirt idea"
             entry
             (file+headline "~/Dropbox/notes/ideas.org" "T-shirt slogans")
             "* %^{T-shirt slogan (title)}\n %? \n%u"
             )
    
            ("j" 
             "Journal" 
             entry (file+datetree "")             
             "* %?\nEntered on %U\n  %i\n  %a"))) 
    
    (setq org-capture-templates-contexts
          '(("b" ((in-mode . "org-mode")))))

## Project configuration

Publishing is configured almost entirely through setting the value of one variable, called \`org-publish-project-alist

    
    (setq org-publish-project-alist
          '(
            ("org-perfumed-nightmare"
             :base-directory "~/Dropbox/notes/blog"
             :publishing-directory "~/Documents/danielsz.github.io/_posts"
             :publishing-function org-html-publish-to-html
             :preparation-function (lambda () (mapcar 'pn-expand-blog-file (pn-select-blog-files)))
             :completion-function pn-delete-blog-files
             :table-of-contents nil
             :html-extension "html"
             :body-only t 
             :exclude "\\^\\([0-9]\\{4\\}-[0-9]+-[0-9]+\\)"
             ))
          )

These are my helper functions for the above project. One-click exporting to jekyll.

    
    (defun pn-get-property (prop)
      (plist-get (cdr (assoc "org-perfumed-nightmare" org-publish-project-alist)) prop))
    
    (defun pn-select-blog-files ()
      (directory-files (pn-get-property :base-directory) t "\\([0-9]\\{4\\}-[0-9]+-[0-9]+\\)"))
    
    (defun pn-delete-blog-files ()
      (mapcar (lambda (file)
                (kill-buffer (find-buffer-visiting file))
                (delete-file file)) (pn-select-blog-files))
      ) 
    (defun chomp (str)
      "Chomp leading and trailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)
    
    (defun pn-delete-line ()
      (delete-region (point) (progn (forward-line -1) (point))))
    
    (defun pn-expand-blog-file (file)
      (with-current-buffer (find-file-noselect file)
        (end-of-buffer)
        (beginning-of-line)
        (let ((root-file (chomp (thing-at-point 'line))))
          (pn-delete-line)
          (insert-file-contents root-file)
          (delete-region (point) (line-end-position)))))

Interactive function to enable the 1-click custom export command in Emacs:

    
    (defun org-export-blog ()
      "1-click blog publishing"
      (interactive)
      (org-capture nil "b")
      (org-publish "org-perfumed-nightmare"))

## Org-babel

org-babel setup

    
    (when (locate-file "ob" load-path load-suffixes)
      (require 'ob)
      (require 'ob-tangle)
    
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (clojure . t)
         (js . t)
         (ruby . t)))
    
      (setq org-src-fontify-natively t)
      (setq org-confirm-babel-evaluate nil))

# Ctags

Find root (replace eproject-root): cd "$(git rev-parse &#x2013;show-toplevel)"

    (defun build-ctags ()
      (interactive)
      (message "building project tags")
      (let ((root (eproject-root)))
        (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root)))
      (visit-project-tags)
      (message "tags built successfully"))
    
    (defun visit-project-tags ()
      (interactive)
      (let ((tags-file (concat (eproject-root) "TAGS")))
        (visit-tags-table tags-file)
        (message (concat "Loaded " tags-file))))

# Paredit

    (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
    (add-hook 'clojure-mode-hook          #'enable-paredit-mode)
    (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)

# Pretty Lambda

    (pretty-lambda-for-modes)

# Slime

If there is a slime helper in quicklisp directory, assume a clozure installation

    (let 
         ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
      (when (file-exists-p slime-helper)
        (load slime-helper)
        (setq inferior-lisp-program "ccl64")))

Open the hyperspec with w3m. \`C-c C-d h\`

    (setq browse-url-browser-function '(("hyperspec" . w3m-browse-url)
                                        ("." . browse-url-default-macosx-browser)))

# Clojure

## nrepl

      ;  (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
      ;  (add-hook 'nrepl-interaction-mode-hook (lambda () (require 'nrepl-ritz)))
      ;  (add-hook 'nrepl-repl-mode-hook 'subword-mode)
    (add-hook 'cider-repl-mode-hook 'subword-mode)
      ;  (add-hook 'nrepl-repl-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
    (setq nrepl-hide-special-buffers t)
    ;(setq nrepl-popup-stacktraces-in-repl t)
    (setq cider-popup-stacktraces nil)                                        
    (setq cider-auto-select-error-buffer t)
    (setq nrepl-buffer-name-show-port t)
    ;(define-key nrepl-repl-mode-map (kbd "C-c C-i") 'nrepl-inspect)
    ;(add-to-list 'same-window-buffer-names "*nrepl*")

I was experimenting with integrated tools.namespace reloading in elisp as well, and I found a slightly nicer way to send commands to nrepl:

    (defun nrepl-reset ()
        (interactive)
        (nrepl-interactive-eval "(user/reset)"))

## rainbow delimiters

    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

## pretty lambda and co

    (eval-after-load 'clojure-mode
      '(font-lock-add-keywords
        'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                         (0 (progn (compose-region (match-beginning 1)
                                                   (match-end 1) "λ")
                                   nil))))))
    
    (eval-after-load 'clojure-mode
      '(font-lock-add-keywords
        'clojure-mode `(("\\(#\\)("
                         (0 (progn (compose-region (match-beginning 1)
                                                   (match-end 1) "ƒ")
                                   nil))))))
    
    (eval-after-load 'clojure-mode
      '(font-lock-add-keywords
        'clojure-mode `(("\\(#\\){"
                         (0 (progn (compose-region (match-beginning 1)
                                                   (match-end 1) "∈")
                                   nil))))))

# w3m

    (setq w3m-coding-system 'utf-8
              w3m-file-coding-system 'utf-8
              w3m-file-name-coding-system 'utf-8
              w3m-input-coding-system 'utf-8
              w3m-output-coding-system 'utf-8
              w3m-terminal-coding-system 'utf-8)

# mu4e

    (when (require 'mu4e nil t)
      (setq 
       mu4e-maildir (expand-file-name "~/mail")
       mu4e-mu-binary "/usr/local/bin/mu"
       ;; below are the defaults; if they do not exist yet, mu4e offers to
       ;; create them. they can also functions; see their docstrings.
       ;; (setq mu4e-sent-folder   "/sent")
       ;; (setq mu4e-drafts-folder "/drafts")
       ;; (setq mu4e-trash-folder  "/trash")
       ;;mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
       mu4e-get-mail-command "true"
       mu4e-update-interval 300)             ;; update every 5 minutes
      )
    
    ;; something about ourselves
    (setq
     user-mail-address "daniel.szmulewicz@gmail.com"
     user-full-name  "Daniel Szmulewicz"
     message-signature
     (concat
      "http://danielsz.github.io\n"))
    
    ;;for emacs-24 you can use: 
    (setq send-mail-function 'smtpmail-send-it
          smtpmail-stream-type 'starttls
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587
          ;; To allow for queuing, you need to tell smtpmail where you want to
          ;; store the queued messages. For example:
          smtpmail-queue-mail nil  ;; start in non-queuing mode
          smtpmail-queue-dir "~/mail/queue/cur"
          )
    
    ;; attempt to show images when viewing messages
    (setq
     mu4e-view-show-images t
     mu4e-view-image-max-width 800)

# ElDOC

    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

# Terminal hotkey

    
    (defun toggle-terminal ()
      (interactive)
      (if (string= "eshell-mode" (eval 'major-mode))
          (winner-undo)
        (progn (delete-other-windows) (eshell))))

# Flycheck

    (add-hook 'after-init-hook #'global-flycheck-mode)

# nyan-mode

    
    (add-hook 'after-init-hook
              (lambda ()
                (if (display-graphic-p)
                    (nyan-mode t)
                  (nyan-mode -1)))) 
    
    
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (if (display-graphic-p frame)
                    (nyan-mode t)
                  (nyan-mode -1)))) 

# web-mode

    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    
    
    (defun web-mode-hook ()
      "Hooks for Web mode."
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
    )
    (add-hook 'web-mode-hook  'web-mode-hook)

# elisp

## elisp-slime-nav

    (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
      (add-hook hook 'turn-on-elisp-slime-nav-mode))

## elisp-format

elisp-format is a handy mode enabling source code formating of elisp code.
It's unavailable on the repos, so here is how to install it manually (commented out for now) 

    ;;(add-to-list 'load-path (expand-file-name "~/elisp"))
    ;;(require 'elisp-format)

# Global keys

    ;;ace-jump-mode
    (eval-after-load "org"
            '(define-key org-mode-map "\C-c " 'nil)) ; unmap key, was org-table-blank-field
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
    ;;org-capture
    (global-set-key [f6] 'org-capture)
    ;;deft
    (global-set-key [f8] 'deft)
    ;;org-velocity
    (global-set-key (kbd "C-c v") 'helm-projectile)
    ;;magit
    (global-set-key (kbd "C-x C-o") 'magit-status)
    ;;;Smex is a M-x enhancement for Emacs. Built on top of IDO, it provides a convenient interface to your recently and most frequently used commands.
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
    ;; This is your old M-x.
    (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
    ;;Get rid of `find-file-read-only' and replace it with something more useful.
    (global-set-key (kbd "C-x C-r") 'ido-recentf-open)
    ;;helm mini
    (global-set-key (kbd "C-c h") 'helm-mini)
    ;;magit-status
    (global-set-key (kbd "C-x g") 'magit-status)
    ;; slime-selector
    (global-set-key [f5] 'slime-selector)
    ;; terminal visor
    (global-set-key (kbd "C-c t") 'toggle-terminal)
