;; =============================================================================

;; Configure paths
(setq path-init "~/config/emacs/.emacs.d/")
(setq path-prog "~/Desktop/Prog/")
(setq path-config "~/config/")
(setq path-org "~/org/")

(defun rel-init (x)
  "Get a path relative to .emacs.d editable config"
  (concat (file-name-as-directory path-init) x))

(defun rel-prog (x)
  "Get a path relative to .emacs.d editable config"
  (concat (file-name-as-directory path-prog) x))

(defun rel-org (x)
  "Get a path relative to .emacs.d editable config"
  (concat (file-name-as-directory path-org) x))

;; =============================================================================

;; Variable to determine if exwm should be enabled
(setq enable-exwm t)

;; =============================================================================

;; Set UTF-8 for env
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; =============================================================================

;; Set back-up files path
(setq backup-directory-alist `(("." . "~/.saves/")))

;; =============================================================================

;; Don't Go where the mouse follows
(setq mouse-autoselect-window nil)

;; =============================================================================

;; Dont use a custom file
;; Customize stuff by yourself
(setq-default custom-file nil)

;; =============================================================================

;; Load path for elisp files
(add-to-list 'load-path (rel-init "elisp"))

;; =============================================================================

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode)
(blink-cursor-mode 1)
(global-hl-line-mode 1)
(display-time-mode 1)                ;; Display time
(display-battery-mode 1)             ;; Display battery

;; =============================================================================

;; Prompt before kill
(setq confirm-kill-emacs 'yes-or-no-p)

;; =============================================================================

;; Conservative scrolling
(setq scroll-preserve-screen-position 'always)

;; =============================================================================

;; Ignore ding
(setq ring-bell-function 'ignore)

;; =============================================================================

;;disable splash screen and scratch message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; =============================================================================

;; Auth keys sources
(setq auth-sources '("~/.authinfo"))

;; =============================================================================

;; Set column length to 80
(setq-default fill-column 80)

;; =============================================================================

;; Onsave hook, remove spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; =============================================================================

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; =============================================================================

;; Key bindings
(global-set-key (kbd "M-o") 'other-window)
(define-key isearch-mode-map "\C-n" 'isearch-repeat-forward)
(define-key isearch-mode-map "\C-p" 'isearch-repeat-backward)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward)
(global-set-key (kbd "C-x C-b") 'buffer-list-switch)

;; Translate ESC to C-c, We have no use for ESC
(define-key key-translation-map (kbd "ESC") (kbd "C-c"))

;; =============================================================================

;; Use Alt+Arrow to jump to different windows
;; This does not work well with exwm
(windmove-default-keybindings 'meta)

;; =============================================================================

;; Default white space to match anything
(setq search-whitespace-regexp ".*?")

;; Use isearch-del-char instead of isearch-delete-char
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;; =============================================================================

;; Hilight matching parenthesis
(progn
  (setq show-paren-style 'parenthesis)
  (show-paren-mode 1))

;; =============================================================================

;; Goto program dir when you type "prog"
;; Mostly used in eshell
(defalias 'prog (lambda () (cd path-prog)))

;; =============================================================================

(defun nushell ()
  "Start a nu process in ansi-term"
  (interactive)
  (ansi-term "nu"))

;; Configure terminal for unicode and set nu
(use-package term
  :init
  (defun setup-term ()
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (defun paste-in-term ()
    (interactive)
    (term-send-raw-string (current-kill 0)))
  :bind (:map term-raw-map
	      ("M-o" . other-window)
	      ("C-y" . paste-in-term))
  :hook ((term-exec . setup-term)))

;; =============================================================================

;; Configure proced-narrow
(use-package proced-narrow
  :bind (:map proced-mode-map
	      ("/" . proced-narrow)))

;; =============================================================================

;; Configure nix-dienv
(use-package direnv
  :config
  (direnv-mode))

;; =============================================================================

;; Configure vue-mode
(use-package vue-mode)

;; =============================================================================

;; Configure elm-mode
(use-package elm-mode)

;; =============================================================================

;; Configure nix-mode
(use-package nix-mode)

;; =============================================================================

;; Configure helm
(use-package helm
  :demand t
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("M-/" . helm-dabbrev)
	 ("C-x b" . helm-mini)
	 ("C-c h" . helm-command-prefix)
	 ("C-c g" . helm-google-suggest))
  :custom (helm-split-window-in-side-p t)
  :config (helm-mode 1))

; :bind cannot be used as eshell has a bug
(use-package helm-eshell
  :init
  (defun setup-eshell-env ()
    "Set up the eshell environment"
    (define-key eshell-mode-map (kbd "M-/") 'helm-eshell-history))
  :hook ((eshell-mode . setup-eshell-env)))

;; =============================================================================

;; Blog settings
(use-package org-static-blog
  :init
  (setq header_ "
<link rel=\"stylesheet\"
      type=\"text/css\"
      href=\"https://gongzhitaao.org/orgcss/org.css\"/>
")
  (setq preamble_ "
<div style=\"display: flex; justify-content: space-between; width: 100%\">
  <div>٩(^‿^)۶</div>
  <div><a href=\"./index.html\">Home</a></div>
  <div><a href=\"./about.html\">About</a></div>
  <div><a href=\"./archive.html\">Archive</a></div>
  <div><a href=\"./rss.xml\">RSS</a></div>
</div>
")
  (setq postamble_ "
<small>
Site built by
<a href=\"https://github.com/bastibe/org-static-blog\">org-static-blog</a>
& <a href=\"https://github.com/gongzhitaao/orgcss\">orgcss</a>.
The source code is
<a href=\"https://github.com/adithyaov/adithyaov.github.io\">
available on Github</a>.
</small>
")
  :custom
  (org-static-blog-publish-title "Adithya Obilisetty")
  (org-static-blog-publish-url "./")
  (org-static-blog-publish-directory (rel-prog "blog/"))
  (org-static-blog-posts-directory (rel-prog "blog/posts/"))
  (org-static-blog-drafts-directory (rel-prog "blog/drafts/"))
  (org-static-blog-enable-tags nil)
  (org-export-with-toc t)
  (org-export-with-section-numbers t)
  (org-static-blog-use-preview t)
  :config
  (setq org-static-blog-page-header header_)
  (setq org-static-blog-page-preamble preamble_)
  (setq org-static-blog-page-postamble postamble_))

;; =============================================================================

;; Configure doom-modeline
(use-package doom-modeline
  :config
  (doom-modeline-init)
  (doom-modeline-mode 1))

;; =============================================================================

;; Configure forge
(use-package forge)

;; =============================================================================

;; Fira code font
(use-package frame
  :config
  (set-frame-font "Fira Code" nil t))

;; =============================================================================

;; Fira code font
(use-package fira-code-mode
  :config
  (global-fira-code-mode))

;; =============================================================================

;; Configure ox-reveal
(use-package ox-reveal)

;; =============================================================================

;; Configure org
(use-package org
  :demand t
  :init
  (setq tasks-file (rel-org "tasks.org"))
  :bind (("C-c c" . org-capture)
	 :map org-mode-map
	 ("<C-return>" . er/expand-region))
  :custom
  ;; Org mode work flow - Kanban style
  (org-todo-keywords
   '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "|" "DONE" "ARCHIVED")))
  ;; Setting Colours (faces) for todo states to give clearer view of work
  (org-todo-keyword-faces
   '(("TODO" . org-warning)
     ("DOING" . "yellow")
     ("BLOCKED" . "red")
     ("REVIEW" . "orange")
     ("DONE" . "green")
     ("ARCHIVED" .  "blue")))
  :config
  (setq org-capture-templates
	'(("t" "Todo" entry
	   (file+headline tasks-file "Tasks")
	   "* TODO %?\n %i\n %a")
	  ("j" "Journal" entry
	   (file+olp+datetree "~/org/journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a"))))

(defun tasks ()
  "Open the tasks file."
  (interactive)
  (find-file tasks-file))

;; =============================================================================

;; Hilight text that extends beyond a certain column
(use-package column-enforce-mode
  :config
  (global-column-enforce-mode t))

;; =============================================================================

;; Git prompt in eshell
(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'robbyrussell))

;; =============================================================================

;; Configure theme
(use-package doom-themes
  :config
  (load-theme 'doom-palenight t))

;; =============================================================================

(use-package faces
  :custom-face
  (mode-line-inactive ((t (:background "#232635" :foreground "#676E95" :box nil)))))

;; =============================================================================

(use-package highlight-function-calls
  :custom-face
  (highlight-function-calls-face ((t (:weight bold)))))

;; =============================================================================

;; Configure impatient-mode
;; Look at markdown in a clean format
(use-package impatient-mode
  :config
  (defun markdown-html (buffer)
    (princ
     (with-current-buffer buffer
              (format "<!DOCTYPE html><html><title>Impatient Markdown</title>\
<xmp theme=\"united\" style=\"display:none;\"> %s  </xmp>\
<script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>"
		      (buffer-substring-no-properties (point-min) (point-max))))
     (current-buffer))))

;; =============================================================================

;; Configure expand-region
(use-package expand-region
  :bind (("C-=" . er/expand-region)
	 ("<C-return>" . er/expand-region)
	 ("C--" . er/contract-region))
  :config
  (pending-delete-mode t))

;; =============================================================================

;; Configure magit
(use-package magit
  :bind (("C-x g" . magit-status)))

;; =============================================================================

;; Configure haskell-mode
(use-package haskell-mode
  :custom
  (haskell-tags-on-save nil)
  (tags-revert-without-query t)
  (haskell-compile-cabal-build-command "cabal v2-build")
  :hook
  ((haskell-mode . turn-on-haskell-indent)))

;; =============================================================================

;; Configure projectile
(use-package projectile
  :demand t
  :custom
  (projectile-completion-system 'helm)
  (projectile-project-search-path `(,path-prog))
  :config
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map)))

;; =============================================================================

;; Configure helm-projectile
(use-package helm-projectile
  :after (projectile)
  :config
  (helm-projectile-on))

;; =============================================================================

;; Configure multiple-cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)))

;; =============================================================================

;; Configure avy
(use-package avy
  :custom
  (avy-keys '(?a ?s ?d ?f ?q ?w ?e ?r ?n ?m ?j ?k ?l ?o ?p))
  (avy-background t)
  (avy-orders-alist '((avy-goto-word-0 . avy-order-closest)
			   (avy-goto-word-1 . avy-order-closest)))
  (avy-all-windows nil)
  :bind
  (("M-RET" . avy-goto-word-0)))

;; =============================================================================

;; Configure hydra
(use-package hydra
  :config
  (global-set-key
   (kbd "C-c m")
   (defhydra hydra-movement (:body-pre (set-mark-command nil))
     "Hydra navigation"
     ("f"   forward-char)
     ("F"   forward-word)
     ("b"   backward-char)
     ("B"   backward-word)
     ("p"   previous-line)
     ("n"   next-line)
     ("P"   backward-paragraph)
     ("N"   forward-paragraph)
     ("a"   smarter-move-point)
     ("e"   move-end-of-line)
     ("q"   nil)
     ("v"   scroll-up-command)
     ("V"   scroll-down-command)
     ("m"   avy-goto-word-0 :color blue))))

;; =============================================================================

;; Configure highlight-function-calls
;; Highlight emacs function calls
(use-package highlight-function-calls
  :hook ((emacs-lisp-mode . highlight-function-calls-mode)))

;; =============================================================================

;; Handy function to open my init file
(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file (rel-init "init.el")))

;; =============================================================================

;; Use C-a to move to beginning of line and first indentation
(defun smarter-move-point ()
    "Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line"
    (interactive)

    (let ((orig-point (point)))
      (back-to-indentation)
      (if (= orig-point (point))
	  (move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'smarter-move-point)

;; =============================================================================

;; Open buffer list/switch
(defun buffer-list-switch ()
  "Switch to buffer list and activate the window"
  (interactive)
  (list-buffers)
  (select-window (get-buffer-window "*Buffer List*" 0)))

;; =============================================================================

;; Haskell auto show core script
(defun produce-core (file-path &optional ghc-path args)
  (let* ((check-nil (lambda (x y) (if x x y)))
	 (gp (funcall check-nil ghc-path "ghc"))
	 (aa (funcall check-nil args ""))
	 (cmd (concat gp " " file-path " -ddump-simpl -ddump-to-file " aa))
	 (so (shell-command-to-string cmd))
	 (res
	  (with-temp-buffer
	    (insert-file-contents
	     (concat (substring file-path 0 -2) "dump-simpl") nil)
	    (buffer-string)))
	 (cb (current-buffer)))
    (progn (with-help-window "*core*" (princ res))
	   (switch-to-buffer "*core*")
	   (haskell-mode)
	   (switch-to-buffer cb))))

(defun current-core ()
  (interactive)
  (produce-core (buffer-file-name) "ghc" "-dsuppress-all -O2"))

;; =============================================================================

;; cabal check open-repl/close
(defvar cabalcc-target)

(defun cabalcc (main &rest args)
  (interactive "srepl: ")
  (let* ((cmd (lambda (x) (concat "(echo :q | cabal repl " x  ")" )))
	 (and-cmd (lambda (x) (concat " && " (funcall cmd x))))
	 (result (funcall cmd main))
	 (full-cmd
	  (dolist (element args result)
	    (setq result (concat result (funcall and-cmd element))))))
    (projectile-with-default-dir (projectile-project-root)
      (compile full-cmd))))

(defun set-cabalcc-target (target)
  (interactive "starget: ")
  (setq cabalcc-target target))

(defun cabalcc-target ()
  (interactive)
  (cabalcc cabalcc-target))

(define-key haskell-mode-map (kbd "C-c C-c") 'cabalcc-target)

;; =============================================================================

;; Custom function to delete blank lines & spaces, Thanks ergoemacs!
(defun xah-delete-blank-lines ()
    "Delete all newline around cursor.

URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2018-04-02"
    (interactive)
    (let ($p3 $p4)
      (skip-chars-backward "\n")
      (setq $p3 (point))
      (skip-chars-forward "\n")
      (setq $p4 (point))
      (delete-region $p3 $p4)))

(defun xah-shrink-whitespaces ()
    "Remove whitespaces around cursor to just one, or none.

Shrink any neighboring space tab newline characters to 1 or none.
If cursor neighbor has space/tab, toggle between 1 or 0 space.
If cursor neighbor are newline, shrink them to just 1.
If already has just 1 whitespace, delete it.

URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2018-04-02T14:38:04-07:00"
    (interactive)
    (let* (
	   ($eol-count 0)
	   ($p0 (point))
	   $p1 ; whitespace begin
	   $p2 ; whitespace end
	   ($charBefore (char-before))
	   ($charAfter (char-after ))
	   ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9)))
	   $just-1-space-p
	   )
      (skip-chars-backward " \n\t")
      (setq $p1 (point))
      (goto-char $p0)
      (skip-chars-forward " \n\t")
      (setq $p2 (point))
      (goto-char $p1)
      (while (search-forward "\n" $p2 t )
	(setq $eol-count (1+ $eol-count)))
      (setq $just-1-space-p (eq (- $p2 $p1) 1))
      (goto-char $p0)
      (cond
       ((eq $eol-count 0)
	(if $just-1-space-p
	    (delete-horizontal-space)
	  (progn (delete-horizontal-space)
		 (insert " "))))
       ((eq $eol-count 1)
	(if $space-neighbor-p
	    (delete-horizontal-space)
	  (progn (xah-delete-blank-lines) (insert " "))))
       ((eq $eol-count 2)
	(if $space-neighbor-p
	    (delete-horizontal-space)
	  (progn
	    (xah-delete-blank-lines)
	    (insert "\n"))))
       ((> $eol-count 2)
	(if $space-neighbor-p
	    (delete-horizontal-space)
	  (progn
	    (goto-char $p2)
	    (search-backward "\n" )
	    (delete-region $p1 (point))
	    (insert "\n"))))
       (t (progn
	    (message "nothing done. logic error 40873. shouldn't reach here" ))))))

(global-set-key (kbd "M-d") 'xah-shrink-whitespaces)

;; =============================================================================

;; Scroll by min(Paragraph, Half screen)

(defun is-empty-line ()
  "Check if the current line is empty"
  (if (eq (line-beginning-position) (line-end-position)) t))

(defun smart-jump (arg)
  "Go to current position + till. If a newline if found, stop"
  (or arg (setq arg 50))
  (setq avg-num-line-chars (* arg 45))
  (if (> arg 0)
      (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
	    (t (re-search-forward "^\n" (+ (point) avg-num-line-chars) 'end))))
  (if (< arg 0)
      (cond ((looking-at "\\s)") (forward-char 1) (backward-list 1))
	    (t (re-search-backward "^\n" (+ (point) avg-num-line-chars) 'end)))))

(global-set-key (kbd "M-n") (lambda () (interactive) (smart-jump 20)))
(global-set-key (kbd "M-p") (lambda () (interactive) (smart-jump -20)))

;; =============================================================================

;; Configure hindent
(use-package hindent
  :demand t
  :hook ((haskell-mode . hindent-mode))
  :config
  ;; Try to work with both, hindent and CPP
  (setq alist-haskell-cpp
	'(("INLINE_LATE" . "INLINE [0]")
	  ("INLINE_NORMAL" . "INLINE [1]")
	  ("INLINE_EARLY" . "INLINE [2]")))
  (defun haskell-desugar-cpp-decl (assoc-arr)
    (interactive)
    (let ((start-end (hindent-decl-points)))
      (when start-end
	(let ((beg (car start-end))
	      (end (cdr start-end)))
	  (dolist (elem assoc-arr)
	    (replace-string (car elem) (cdr elem) nil beg end))))))
  (defun haskell-sugar-cpp-decl (assoc-arr)
    (interactive)
    (let ((start-end (hindent-decl-points)))
      (when start-end
	(let ((beg (car start-end))
	      (end (cdr start-end)))
	  (dolist (elem assoc-arr)
	    (replace-string (cdr elem) (car elem) nil beg end))))))
  (defun hindent-reformat-decl-cpp (&optional assoc-arr)
    (interactive)
    (progn
      (unless assoc-arr (setq assoc-arr alist-haskell-cpp))
      (haskell-desugar-cpp-decl assoc-arr)
      (hindent-reformat-decl)
      (haskell-sugar-cpp-decl assoc-arr)))
  (defun hindent-reformat-decl-or-fill-cpp (justify)
    (interactive (progn
		   (barf-if-buffer-read-only)
		   (list (if current-prefix-arg 'full))))
    (if (hindent-in-comment)
	(fill-paragraph justify t)
      (progn
	  (setq move-point (point))
	  (hindent-reformat-decl-cpp)
	  (goto-char move-point))))
  :bind
  (:map hindent-mode-map
	([remap fill-paragraph] . hindent-reformat-decl-or-fill-cpp)))

;; =============================================================================

;; Configure ghcid
(use-package ghcid
  :after (projectile)
  :config
  (require 's)
  (defun set-default-target ()
    "Set a default ghcid-target"
    (setq ghcid-target
	  (concat "lib:"
		  (-last 's-present? (s-split "/" (projectile-project-root))))))
  (defun ghcid-projectile ()
    "Start a ghcid process in a new window. Kills any existing sessions.

The process will be started in the directory of the buffer where
you ran this command from."
    (interactive)
    (set-default-target)
    (ghcid-start (projectile-project-root))))

;; =============================================================================

(use-package exwm-config :demand t)

;; Configure exwm
(use-package exwm
  :after (exwm-config)
  :init
  (defun rename-workspace-buffer ()
      "Rename buffer according to exwm-class-name"
    (exwm-workspace-rename-buffer exwm-class-name))
  :demand t
  :custom
  (exwm-workspace-number 2)
  (exwm-input-simulation-keys
   '(([?\C-b] . [left])
     ([?\C-f] . [right])
     ([?\C-p] . [up])
     ([?\C-n] . [down])
     ([?\M-f] . [C-right])
     ([?\M-b] . [C-left])
     ([?\C-a] . [home])
     ([?\C-e] . [end])
     ([?\M-p] . [prior])
     ([?\M-n] . [next])
     ([?\C-d] . [delete])
     ([?\C-k] . [S-end C-x])
     ([?\C-y] . [C-v])
     ([?\M-w] . [C-c])
     ([?\C-s] . [C-f])
     ([?\C-g] . [escape])))
  :config
  (exwm-input-set-key (kbd "C-c o") #'exwm-workspace-switch)
  (exwm-input-set-key  (kbd "M-o") #'other-window)
  (push (kbd "<escape>") exwm-input-prefix-keys)
  (if enable-exwm
      (exwm-enable))
  :hook ((exwm-update-class . rename-workspace-buffer)))

;; =============================================================================
