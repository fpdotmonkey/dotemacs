;; -*- Emacs-Lisp -*- ~*~ UTF-8 ~*~

;; Allow custom packages to be installed
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))

;; for installing things if they haven't already
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  )

;; for installing from source
(use-package "quelpa" :ensure t)
(use-package quelpa-use-package
  :ensure t
  :config (quelpa-use-package-activate-advice))


;; Scimax
(if (file-exists-p "~/.emacs.d/scimax/init.el")
    (load "~/.emacs.d/scimax/init.el")
  (display-warning :warning (message "install scimax with `bash -c \"$(curl -fsSL https://raw.githubusercontent.com/jkitchin/scimax/master/install-scimax-linux.sh)\"`")))


;; Define preferred color theme
;; Possible options:
;; adwaita 	deeper-blue 	dichromacy 	leuven		light-blue
;; manoj-dark	misterioso	tango		tango-dark 	tsdh-dark
;; sdh-light 	wheatgrass	whiteboard 	wombat
;;; setting the theme messes with org-mode stuff, will need to investigate
;; (load-theme 'wombat)


;; fira code font
(if (find-font (font-spec :name "Fira Code-11"))
    (progn
      (set-face-attribute 'default nil :font "Fira Code-11")
      (set-frame-font "Fira Code-11" nil t))
  (display-warning :warning (message "install Fira Code https://github.com/tonsky/FiraCode")))
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                          '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                            ;; =:= =!=
                            ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                            ;; ;; ;;;
                            (";" (rx (+ ";")))
                            ;; && &&&
                            ("&" (rx (+ "&")))
                            ;; !! !!! !. !: !!. != !== !~
                            ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                            ;; ?? ??? ?:  ?=  ?.
                            ("?" (rx (or ":" "=" "\." (+ "?"))))
                            ;; %% %%%
                            ("%" (rx (+ "%")))
                            ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                            ;; |->>-||-<<-| |- |== ||=||
                            ;; |==>>==<<==<=>==//==/=!==:===>
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "=" ))))
                            ;; \\ \\\ \/
                            ("\\" (rx (or "/" (+ "\\"))))
                            ;; ++ +++ ++++ +>
                            ("+" (rx (or ">" (+ "+"))))
                            ;; :: ::: :::: :> :< := :// ::=
                            (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!" "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ;; *> */ *)  ** *** ****
                            ("*" (rx (or ">" "/" ")" (+ "*"))))
                            ;; www wwww
                            ("w" (rx (+ "w")))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>>
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
					 (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; Fira code: 0xFF 0x12
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            ;; Fira code:
                            "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                            ;; The few not covered by the regexps.
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))


;; packages from git
(use-package image-roll
  :quelpa (image-roll
	   :fetcher github
	   :repo "dalanicolai/image-roll.el"))
(use-package pdf-tools
  :quelpa (pdf-tools
	   :fetcher github
	   :repo "dalanicolai/pdf-tools"
	   :branch "pdf-roll"
	   :files ("lisp/*.el"
		   "README"
		   ("build" "Makefile")
		   ("build" "server")
		   (:exclude "lisp/tablist.el" "lisp/tablist-filter.el"))))

(use-package epc)


;; Highlight numeric literals
(use-package "highlight-numbers")
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
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line.

On visually wrapped lines, move the point first to the beginning of the visual line, and on next invocation to the indentation."
  (interactive)
  (let ((oldpos (point)))
    (if (< (window-total-width) (current-column))
	(progn (call-interactively 'beginning-of-visual-line)
	       (and (= oldpos (point))
		    (call-interactively 'back-to-indentation)))
      (call-interactively 'back-to-indentation)
      (and (= oldpos (point))
	   (call-interactively 'beginning-of-line)))))
(defun shift-smart-beginning-of-line ()
  (interactive)
  (setq this-command-keys-shift-translated t)
  (smart-beginning-of-line))
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "S-<home>") 'shift-smart-beginning-of-line)
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
(use-package "highlight-parentheses")
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda nil (highlight-parentheses-mode t)))

(setq highlight-parentheses-colors '(nil "tan2" "tan3" "tan4"))
(setq highlight-parentheses-background-colors '("tan4", nil, nil, nil))
(global-highlight-parentheses-mode t)


;; Enable multiple cursors
(use-package "multiple-cursors")
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
(use-package "eldoc")
(require 'eldoc)
(if (version< "24.4.0" emacs-version)
    (progn (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
	   (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
	   (add-hook 'ielm-mode-hook 'eldoc-mode))
  (progn (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
	 (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
	 (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))
  )


;; git
(use-package "magit")
(require 'magit)
(use-package "git-modes")
(require 'git-modes)


;; Set up a python environment
(use-package "elpy")
(require 'elpy)
(elpy-enable)
(setq elpy-rpc-virtualenv-path 'current)
(add-hook 'elpy-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook
		      'elpy-format-code nil t)))


;; Set up a C/C++ environment
;; clang-format
(use-package "clang-format")
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
(use-package "c-eldoc")
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
(use-package "glsl-mode")
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;; clang-format
(add-hook 'glsl-mode-hook (lambda () (clang-format-save-hook)))


;; Rust
(use-package "rust-mode")
(require 'rust-mode)
(use-package "rustic")

;; Enforce spaces instead of tabs
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; rustfmt
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook (lambda () (prettify-symbols-mode)))

;; lsp
(add-hook 'rust-mode-hook 'lsp-deferred)

;; key binds
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
(define-key rust-mode-map (kbd "C-c C-l") 'rust-run-clippy)
(define-key rust-mode-map (kbd "C-C C-t") 'rust-test)


;; haskell
(use-package haskell-mode)

;; org-mode stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (python . t)
     (shell . t)
     (matlab . t)
     (sqlite . t)
     (ruby . t)
     (perl . t)
     (org . t)
     (dot . t)
     (plantuml . t)
     (R . t)
     (fortran . t)
     (C . t)))
 '(package-selected-packages
   '(glsl-mode rust-mode multiple-cursors multicolumn lsp-mode highlight-parentheses highlight-numbers elpy clang-format c-eldoc)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
