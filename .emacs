;; -*- Emacs-Lisp -*- ~*~ UTF-8 ~*~

;; Allow custom packages to be installed
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(package-initialize)


;; A helper function to install a package if it isn't already
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
	   (package-refresh-contents)
           (package-install package)
         package)))
   packages))


;; Define preferred color theme
;; Possible options:
;; adwaita 	deeper-blue 	dichromacy 	leuven		light-blue
;; manoj-dark	misterioso	tango		tango-dark 	tsdh-dark
;; sdh-light 	wheatgrass	whiteboard 	wombat
(load-theme 'tsdh-dark)


;; Highlight numeric literals
(ensure-package-installed 'highlight-numbers)
(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)


;; Make mouse scrolling less jerky
(setq mouse-wheel-scroll-amount '(1))
(dolist (modifier '("" "S-" "C-" "M-"))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key
       (read-kbd-macro
	(concat "<" modifier multiple "wheel-" direction ">"))
       'ignore
       ) ) ) )


;; Make [home] behave similar to Sublime
;; @TODO [S-home] doesn't highlight from the current position to home
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)


;; Set rulers at columns 72 and 80
;; @TODO


;; Display line numbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))


;; Make both the line and column appear
(column-number-mode)


;; Don't show the welcome screen
(setq inhibit-startup-screen t)


;; Hide the GUI toolbar
(tool-bar-mode -1)


;; [C-backspace] deletes a word or whitespace up to line start
(defun line-up-to-point-is-blank ()
  "Check if the line up to the current point matches /^[[:blank:]]*/"
  (save-excursion
    (let ((old-point (point))
	  (old-mark (copy-marker (mark-marker))))
      (set-mark old-point)
      (beginning-of-line)
      (let ((return (equal
		     (re-search-forward "^[[:blank:]]*" (region-end) t)
		     old-point)))
	(deactivate-mark)
	return))))

(defun kill-whitespace-or-word ()
  (interactive)
  (if (line-up-to-point-is-blank)
      (delete-indentation)
    (backward-kill-word 1)))

(global-set-key  [C-backspace]
		 'kill-whitespace-or-word)


;; Highlight enclosing brackets
(ensure-package-installed 'highlight-parentheses)
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda nil (highlight-parentheses-mode t)))

(setq highlight-parentheses-colors '(nil "tan2" "tan3" "tan4"))
(setq highlight-parentheses-background-colors '("tan4", nil, nil, nil))
(global-highlight-parentheses-mode t)


;; Enable multiple cursors
(ensure-package-installed 'multiple-cursors)
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-d") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-d") 'mc/mark-all-like-this)


;; Require a newline at the end of every buffer
(setq-default require-final-newline t)


;; Use a bar cursor
(setq-default cursor-type 'bar)
(set-cursor-color "#ffffff")


;; Set up ElDoc
(ensure-package-installed 'eldoc)
(require 'eldoc)
(if (version< "24.4.0" emacs-version)
    (progn (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
     (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
     (add-hook 'ielm-mode-hook 'eldoc-mode))
  (progn (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
   (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
   (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))
  )


;; Allow multicolumn
(ensure-package-installed 'multicolumn)
(require 'multicolumn)
(multicolumn-global-mode 1)
(setq multicolumn-resize-frome-default-width 85)


;; Set up a python environment
(ensure-package-installed 'elpy)
(require 'elpy)
(elpy-enable)
(setq elpy-rpc-virtualenv-path 'current)
(add-hook 'elpy-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook
		      'elpy-format-code nil t)))


;; Set up a C/C++ environment
;; clang-format
(ensure-package-installed 'clang-format)
(require 'clang-format)
(setq clang-format-fallback-style "mozilla")
(setq clang-format-style "file")
(defun clang-format-save-hook ()
  "Create a buffer-local save hook"
  (add-hook 'before-save-hook
	    (lambda ()
	      (when (locate-dominating-file "." ".clang-format")
		(clang-format-buffer))
	      ;; Continue to save
	      nil)
	    nil
	    ;; Buffer-local hook
	    t))
(add-hook 'c-mode-hook (lambda () (clang-format-save-hook)))
(add-hook 'c++-mode-hook (lambda () (clang-format-save-hook)))

;; ElDoc
(ensure-package-installed 'c-eldoc)
(require 'c-eldoc)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
(defvar c-eldoc-includes
  "`pkg-config gtk+-3.0 --cflags` \
   -I./ \
   -I../ \
   ")


;; GLSL
;; glsl-mode
(ensure-package-installed 'glsl-mode)
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;; clang-format
(add-hook 'glsl-mode-hook (lambda () (clang-format-save-hook)))


;; Rust
(ensure-package-installed 'rust-mode)
(require 'rust-mode)

;; Enforce spaces instead of tabs
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; rustfmt
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook (lambda () (prettify-symbols-mode)))

;; key binds
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
(define-key rust-mode-map (kbd "C-c C-l") 'rust-run-clippy)
(define-key rust-mode-map (kbd "C-C C-t") 'rust-test)
