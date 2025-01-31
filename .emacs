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


;; fira code font
(if (find-font (font-spec :name "Fira Code-11"))
    (progn
      (set-face-attribute 'default nil :font "Fira Code-11")
      (set-frame-font "Fira Code-11" nil t))
  (display-warning :warning (message "install Fira Code https://github.com/tonsky/FiraCode")))
(use-package ligature
  :config
  ;; Enable in every major mode
  (ligature-set-ligatures 't '("www" "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"))
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
                            ;; ++ +++ ++++ +> +a +A
                            ("+" (rx (or ">" (+ "+") (in "alpha"))))
                            ;; :: ::: :::: :> :< := :// ::= :a :A
                            (":" (rx (or ">" "<" "=" "//" ":=" (+ ":") (in "alpha"))))
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!" "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-<->>-<<-| -a -O
                            ("-" (rx (or (+ (or ">" "<" "|" "~" "-")) (in "alpha"))))
                            ;; *> */ *)  ** *** **** *a *A
                            ("*" (rx (or ">" "/" ")" (+ "*") (in "alpha"))))
                            ;; www.
                            ("w" (rx "ww"))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>> f::<T>::G h::<T>()
                            (">" (rx
				  (or
				   (or (>= 3 ":") (and ":" (not ":")) (and ":" eol))
				   (+ (or ">" "<" "|" "/" "=" "-")))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
					 (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; Fira code: 0xFF 0x12
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            ;; The few not covered by the regexps.
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  (ligature-set-ligatures 'text-mode
			  '(
			    ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!" "-"  "/" "|" "="))))
			    ("-" (rx (or (+ (or ">" "<" "|" "~" "-")) (in "alpha"))))))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))


;; Scimax
(if (file-exists-p "~/.emacs.d/scimax/init.el")
    (load "~/.emacs.d/scimax/init.el")
  (display-warning :warning (message "install scimax with `bash -c \"$(curl -fsSL https://raw.githubusercontent.com/jkitchin/scimax/master/install-scimax-linux.sh)\"`")))
(setq scimax-dir "~/.emacs.d/scimax")
(add-to-list 'load-path "~/.emacs.d/scimax")

(setq org-src-block-faces
      '(("emacs-lisp" (:extend t))
	("sh" (:extend t))
	("python" (:extend t))))


;; Define preferred color theme
;; Possible options:
;; adwaita 	deeper-blue 	dichromacy 	leuven		light-blue
;; manoj-dark	misterioso	tango		tango-dark 	tsdh-dark
;; sdh-light 	wheatgrass	whiteboard 	wombat
;;; setting the theme messes with org-mode stuff, will need to investigate
(setq dark-theme nil)
(load-theme 'dichromacy)


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
(if (version<= "29.0.0" emacs-version)
    (progn (pixel-scroll-precision-mode)
	   (setq pixel-scroll-precision-large-scroll-height 35.0))
  (setq mouse-wheel-scroll-amount '(1))
  (dolist (modifier '("" "S-" "C-" "M-"))
    (dolist (multiple '("" "double-" "triple-"))
      (dolist (direction '("right" "left"))
	(global-set-key
	 (read-kbd-macro
	  (concat "<" modifier multiple "wheel-" direction ">"))
	 'ignore)))))


;; Make [home] behave similar to Sublime
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line.

On visually wrapped lines, move the point first to the beginning of the visual line, and on next invocation to the indentation."
  (interactive)
  (let ((oldpos (point)))
    (if (< (window-max-chars-per-line) (current-column))
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

(defun smart-end-of-line ()
  "Move point to the end of the visual line or the end of the line"
  (interactive)
  (let ((oldpos (point)))
    (call-interactively 'end-of-visual-line)
    (and (= oldpos (point))
	 (call-interactively 'end-of-line))))
(global-set-key (kbd "<end>") 'smart-end-of-line)


;; Set rulers at columns 72 and 80
;; @TODO


;; Display line numbers
(when (version<= "26.0.50" emacs-version)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode))


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


;; counsel
(setq counsel-find-file-ignore-regexp "\\`[#~]|\\.~undo-tree~\\'")


;; git
(use-package "magit")
(require 'magit)
(use-package "git-modes")
(require 'git-modes)


;; python
(use-package pyvenv :ensure t)
(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode))
(setq python-black-extra-args '("--line-length" "79"))


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
(use-package rustic)

;; rustfmt
(setq rustic-format-trigger 'on-save)

(use-package rmsbolt)


;; haskell
(use-package haskell-mode)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package lsp-haskell
  :ensure t
  :config
  (setq lsp-haskell-process-path-hie "ghcide")
  (setq lsp-haskell-process-args-hie '()))


;; Golang
(use-package go-mode)


;; better HTML
(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;; LaTeX and Tectonic
(setq
 org-latex-pdf-process
 '("tectonic -X compile --outdir=%o -Z shell-escape -Z continue-on-errors %f"))
(add-to-list 'org-preview-latex-process-alist
	     '(tectonic :programs ("tectonic" "convert") 
			:description "pdf > png"
			:message "you need install the programs: tectonic and imagemagick."
			:image-input-type "pdf" 
			:image-output-type "png"
			:image-size-adjust (2 . 2) 
			:latex-compiler
			("tectonic -Z shell-escape-cwd=%o --outfmt pdf --outdir %o %f")
			:image-converter
			("magick convert -density %D -trim -antialias %f -quality 300 %O")))
(setq org-preview-latex-default-process 'tectonic)
(setq org-latex-title-command "\\maketitle")


;; org-mode x hugo
(use-package ox-hugo :ensure t :after ox)


;; org-link youtube
(org-link-set-parameters "yt"
			 :follow #'org-yt-follow
			 :export #'org-yt-export)

(defun org-yt-follow (video arg)
  "Open the YouTube video in a browser"
  (browse-url (format "https://youtu.be/%s" video) arg))

(defun org-yt-export (video description format _)
  "Export a YouTube video link from Org Files."
  (pcase format
    ;; www-flavored
    (`html (format yt-iframe-format video (or description "")))
    (`md (format yt-iframe-format video (or description "")))
    ;; TeX-flavored
    (`latex (format "\\href{%s}{%s}" video (or description video)))
    (`texinfo (format "@uref{%s,%s}" video (or description video)))
    ;; Text-flavored
    (t (format "%s (http://youtu.be/%s)" description video))))

(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe"
	  " width=560 height=315"
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))


;; org-mode stuff
(setq org-default-notes-file (concat org-directory "~/notes.org"))

(add-to-list 'org-agenda-files "~/org-agenda/")


;; org-ref
(setq bibtex-completion-bibliography '("~/org-agenda/bib/references.bib")
      bibtex-completion-library-path '("~/org-agenda/bib/pdf/")
      bibtex-completion-notes-path "~/org-agenda/bib/notes/"
      bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

      bibtex-completion-additional-search-fields '(keywords)
      bibtex-completion-display-formats
      '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	(inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	(incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	(inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	(t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
      bibtex-completion-pdf-open-function
      (lambda (fpath)
	;; TODO: use `open` for Darwin and `cmd /c start` for Windows
	;; TODO: test if both of ^these are correct
	(call-process "xdg-open" nil 0 nil fpath)))

(require 'bibtex)

(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(define-key bibtex-mode-map (kbd "C-s-b") 'org-ref-bibtex-hydra/body)

(require 'org-ref)
(require 'org-ref-ivy)

(setq org-capture-templates
      '(("w" "Work" entry
	 (file+headline "~/org-agenda/General.org" "Work")
	 "%i\n%a\n%?")
	("k" "Kylän Keittiö" entry
	 (file+headline "~/org-agenda/General.org" "Kylän Keittiö")
	 "%i\n%a\n%?")))

(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
;; C-c ] => insert a citation
;; C-u C-c ] => insert a cross-reference
;; C-u C-u C-c ] => insert a label

;; org generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/home/fp/org-agenda/General.org" "/home/fp/projects/lemmi-drivetrain/doc/idetc-cie.org" "/home/fp/org-agenda/Getting Started with Orgzly.org" "/home/fp/org-agenda/Yana videos.org" "/home/fp/org-agenda/birthdays.org"))
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
