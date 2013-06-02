Table of Contents
-----------------

-   [1 Saving Customizations](#sec-1)
-   [2 Backup and autosave](#sec-2)
-   [3 UTF-8](#sec-3)
-   [4 Tabs](#sec-4)
-   [5 Color theme](#sec-5)
-   [6 Graphic display](#sec-6)
-   [7 Winner mode](#sec-7)
-   [8 Windmove](#sec-8)
-   [9 Recent file list](#sec-9)
-   [10 Ido](#sec-10)
    -   [10.1 recentf](#sec-10-1)

-   [11 Key remappings](#sec-11)
-   [12 El-get](#sec-12)
    -   [12.1 Installation routine](#sec-12-1)
    -   [12.2 Recipe setup](#sec-12-2)
    -   [12.3 User setup](#sec-12-3)

-   [13 Text-files hooks](#sec-13)
-   [14 Deft](#sec-14)
-   [15 Org-mode](#sec-15)
    -   [15.1 Location of default notes files](#sec-15-1)
    -   [15.2 Capture templates](#sec-15-2)
    -   [15.3 Project configuration](#sec-15-3)
    -   [15.4 Org-babel](#sec-15-4)

-   [16 Ctags](#sec-16)
-   [17 Repos](#sec-17)
-   [18 Paredit](#sec-18)
-   [19 Slime](#sec-19)
-   [20 w3m](#sec-20)
-   [21 Mac OS X customizations](#sec-21)
-   [22 Global keys](#sec-22)

1 Saving Customizations
-----------------------

We want to save our customizations in a dedicated file. We will check
the existence of such a file so as not to cause errors on a vanilla
emacs installation. Please create it yourself if you don't want your
customizations to be saved in the init file. (default behavior)

~~~~ {.src .src-emacs-lisp}
(let 
    ((customization-file "~/.emacs.d/emacs-custom.el"))
  (when (file-exists-p customization-file)
    (setq custom-file customization-file)
    (load custom-file)))
~~~~

2 Backup and autosave
---------------------

Keep Backup and Auto-save Files Out of the Way
[http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/](http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/)

~~~~ {.src .src-emacs-lisp}
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
~~~~

3 UTF-8
-------

We want to have default utf-8 everything.

~~~~ {.src .src-emacs-lisp}
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
~~~~

4 Tabs
------

Two-width tab stops, like I'm used to

~~~~ {.src .src-emacs-lisp}
(setq tab-width 2)
(setq tab-stop-list (number-sequence 2 200 2))
(setq indent-tabs-mode nil)
~~~~

5 Color theme
-------------

~~~~ {.src .src-emacs-lisp}
(load-theme 'adwaita t)
~~~~

6 Graphic display
-----------------

Disable menu-bar in terminal, enable in graphic display

~~~~ {.src .src-emacs-lisp}
(if (display-graphic-p)
     (progn
       (set-frame-size (selected-frame) 90 34)
       (menu-bar-mode t))
    (menu-bar-mode 0))
~~~~

7 Winner mode
-------------

~~~~ {.src .src-emacs-lisp}
(winner-mode 1)
~~~~

8 Windmove
----------

~~~~ {.src .src-emacs-lisp}
(when (fboundp 'windmove-default-keybindings)
     (windmove-default-keybindings))
~~~~

Make windmove work in org-mode:

~~~~ {.src .src-emacs-lisp}
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
~~~~

9 Recent file list
------------------

~~~~ {.src .src-emacs-lisp}
(require 'recentf)
(recentf-mode 1)
~~~~

10 Ido
------

~~~~ {.src .src-emacs-lisp}
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
~~~~

### 10.1 recentf

Make recentf use ido

~~~~ {.src .src-emacs-lisp}
(defun ido-recentf-open () 
  "Use `ido-completing-read' to \\[find-file] a recent file" 
  (interactive) 
  (if (find-file (ido-completing-read "Find recent file: " recentf-list)) 
      (message "Opening file...") 
    (message "Aborting")))
~~~~

11 Key remappings
-----------------

Fix the bug that shift-up doesn't send the right escape sequence in
terminal

~~~~ {.src .src-emacs-lisp}
(if (equal "xterm-256color" (tty-type)) (define-key input-decode-map "\e[1;2A" [S-up]))
~~~~

To make windmove work in tmux

~~~~ {.src .src-emacs-lisp}
(if (equal "screen-256color" (tty-type)) 
    (progn
    (define-key input-decode-map "\e[1;2D" [S-left])  
    (define-key input-decode-map (kbd "M-[ 1 ; 2 C") [S-right])  
    (define-key input-decode-map (kbd "M-[ 1 ; 2 B")[S-down])  
    (define-key input-decode-map "\e[1;2A" [S-up])  
    (define-key input-decode-map "\e[1;6A" [S-C-up])
    (define-key input-decode-map "\e[1;6B" [S-C-down])))
~~~~

Not sure it is needed.

~~~~ {.src .src-emacs-lisp}
(if (equal "daniels-imac.local" (system-name))
    (add-hook 'comint-mode-hook
              (lambda ()               
                (define-key comint-mode-map (kbd "M-n") 'comint-next-input))))
~~~~

12 El-get
---------

### 12.1 Installation routine

~~~~ {.src .src-emacs-lisp}
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless 
    (require 'el-get nil t) 
  (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el" 
  (lambda (s) 
    (let (el-get-master-branch)
      (goto-char (point-max)) 
        (eval-print-last-sexp)))))
~~~~

### 12.2 Recipe setup

Canonical list of packages

~~~~ {.src .src-emacs-lisp}
(setq my-packages (append '(el-get smex magit helm dash
                            zenburn-theme solarized-theme 
                            inf-ruby rspec-mode rbenv
                            emacs-w3m yaml-mode 
                            windcycle pbcopy ace-jump-mode
                            flymake flymake-ruby
                            coffee-mode markdown-mode scss-mode mustache-mode
                            clojure-mode midje-mode nrepl htmlize paredit kibit-mode
                            epresent org-html5presentation org-impress-js org-s5
                            o-blog deft palimpsest-mode org-jekyll) 
                          (mapcar 'el-get-source-name el-get-sources)))
(el-get-cleanup my-packages)
(el-get 'sync my-packages)
~~~~

### 12.3 User setup

~~~~ {.src .src-emacs-lisp}
(setq el-get-sources '(
                       (:name ruby-mode
                        :after (when (string= system-name "ma.sdf.org") (setq enh-ruby-program "ruby193")))
                       (:name elisp-format 
                        :features elisp-format)))
~~~~

13 Text-files hooks
-------------------

Turn visual mode for text files

~~~~ {.src .src-emacs-lisp}
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
~~~~

14 Deft
-------

~~~~ {.src .src-emacs-lisp}
(setq deft-directory "~/Dropbox/notes")
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
~~~~

15 Org-mode
-----------

### 15.1 Location of default notes files

~~~~ {.src .src-emacs-lisp}
(let ((destination (if (file-exists-p "~/Dropbox")
                       "~/Dropbox/notes.org"
                     "~/notes.org")))
  (setq org-default-notes-file destination))    
~~~~

### 15.2 Capture templates

~~~~ {.src .src-emacs-lisp}
(require 'org-element)

(defun pn-get-headline ()
  (let* ((headlines (org-map-entries '(org-element-property :title (org-element-at-point)) t 'file)) (headline (car headlines)))
    (org-capture-put :title headline)
     headline))

(defun pn-filename_from_title ()
  (replace-regexp-in-string " " "-" (pn-get-headline)))

(defun pn-capture-blog-path ()
  (let ((name (pn-filename_from_title)))
    (expand-file-name (format "%s-%s.org"
                              (format-time-string "%Y-%m-%d")
                              name) "~/Dropbox/notes/blog")))

(setq org-capture-templates  

      '(
        ("n" 
         "New blog entry" 
         plain 
         (file (pn-capture-blog-path)) 
         "#+BEGIN_HTML\n---\ntitle:\nlayout: post\ntags:\n - blog\n---\n#+END_HTML\n\n"
         :immediate-finish t
         )

        ("b" 
         "Org to Blog entry" 
         plain 
         (file (pn-capture-blog-path)) 
         "#+BEGIN_HTML\n---\ntitle: %(org-capture-get :title)\nlayout: post\ntags: %^{Tags (separated by spaces)}\n---\n#+END_HTML\n\n%F"
         :immediate-finish t
         :kill-buffer t
         )

        ("t" 
         "Todo" 
         entry 
         (file+headline "" "Task")
         "* TODO %?\n  %i\n  %a")

        ("j" 
         "Journal" 
         entry (file+datetree "")             
     "* %?\nEntered on %U\n  %i\n  %a"))) 

  (setq org-capture-templates-contexts
        '(("b" ((in-mode . "org-mode")))))
~~~~

### 15.3 Project configuration

Publishing is configured almost entirely through setting the value of
one variable, called \`org-publish-project-alist

~~~~ {.src .src-emacs-lisp}
(setq org-publish-project-alist
      '(
        ("org-perfumed-nightmare"
         :base-directory "~/Dropbox/notes/blog"
         :publishing-directory "~/Documents/danielsz.github.io/_posts"
         :publishing-function org-publish-org-to-html
         :preparation-function (lambda () (mapcar 'pn-expand-blog-file (pn-select-blog-files)))
         :completion-function pn-delete-blog-files
         :table-of-contents nil
         :html-extension "html"
         :body-only t 
         :exclude "\\^\\([0-9]\\{4\\}-[0-9]+-[0-9]+\\)"
         ))
      )
~~~~

These are my helper functions for the above project. One-click exporting
to jekyll.

~~~~ {.src .src-emacs-lisp}
(defun pn-select-blog-files ()
  (directory-files "~/Dropbox/notes/blog" t "\\([0-9]\\{4\\}-[0-9]+-[0-9]+\\)"))

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
      (save-buffer))))
~~~~

Interactive function to enable the 1-click custom export command in
Emacs:

~~~~ {.src .src-emacs-lisp}
(require 'org-publish)

  (defun org-export-blog ()
    "1-click blog publishing"
    (interactive)
    (org-capture nil "b")
    (org-publish "org-perfumed-nightmare"))
~~~~

### 15.4 Org-babel

org-babel setup

~~~~ {.src .src-emacs-lisp}
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
~~~~

16 Ctags
--------

Find root (replace eproject-root): cd "\$(git rev-parse –show-toplevel)"

~~~~ {.src .src-emacs-lisp}
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
~~~~

17 Repos
--------

~~~~ {.src .src-emacs-lisp}
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("tromey" . "http://tromey.com/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
~~~~

18 Paredit
----------

~~~~ {.src .src-emacs-lisp}
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
~~~~

19 Slime
--------

If there is a slime helper in quicklisp directory, assume a clozure
installation

~~~~ {.src .src-emacs-lisp}
(let 
     ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-exists-p slime-helper)
    (load slime-helper)
    (setq inferior-lisp-program "ccl")))
~~~~

20 w3m
------

~~~~ {.src .src-emacs-lisp}
(setq w3m-coding-system 'utf-8
          w3m-file-coding-system 'utf-8
          w3m-file-name-coding-system 'utf-8
          w3m-input-coding-system 'utf-8
          w3m-output-coding-system 'utf-8
          w3m-terminal-coding-system 'utf-8)
~~~~

21 Mac OS X customizations
--------------------------

Clipboard and kill ring

~~~~ {.src .src-emacs-lisp}
(when (eq system-type 'darwin)
  (progn
    (turn-on-pbcopy)))  
~~~~

22 Global keys
--------------

~~~~ {.src .src-emacs-lisp}
;;ace-jump-mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;;deft
(global-set-key [f8] 'deft)
;;org-velocity
(global-set-key (kbd "C-c v") 'org-velocity-read)
;;magit
(global-set-key (kbd "C-x C-o") 'magit-status)
;;;Smex is a M-x enhancement for Emacs. Built on top of IDO, it provides a convenient interface to your recently and most frequently used commands.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;;Get rid of `find-file-read-only' and replace it with something more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
;;hell mini
(global-set-key (kbd "C-c h") 'helm-mini)
~~~~

Date: 2013-06-02T09:12+0300

Author: Daniel Szmulewicz

[Org](http://orgmode.org) version 7.9.3f with
[Emacs](http://www.gnu.org/software/emacs/) version 24

[Validate XHTML 1.0](http://validator.w3.org/check?uri=referer)
