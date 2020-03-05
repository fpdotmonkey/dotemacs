
(require 'cl)
(require 'compile)
(setq exec-path (cons "~/bin" exec-path))

(load-theme 'misterioso)
(if (eq system-type 'darwin)
    (progn
      (setq mac-option-modifier 'meta)
      (setq mac-command-modifier 'meta)
      (set-default-font "Menlo-12")))

(add-to-list 'load-path "~/projects/emacs-stuff/")
(add-to-list 'load-path "~/projects/Matlab Packages/emacs")
(load-library "matlab-load")
(add-to-list 'load-path "~/projects/dart-mode")
(add-to-list 'load-path "~/projects/flutter")

(add-to-list 'load-path "~/projects/dart-mode/")
(add-to-list 'load-path "~/projects/dash.el/")
(add-to-list 'load-path "~/projects/s.el/")
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
(autoload 'dart-mode "dart-mode")

;; (use-package flutter
;;              :after dart-mode
;;              :bind (:map dart-mode-map
;;                          ("C-M-x" . #'flutter-run-or-hot-reload))
;;              :custom
;;              (flutter-sdk-path "~/projects/flutter"))

(add-to-list 'load-path "~/projects/emacs-stuff/julia-emacs")
(require 'julia-mode)

;; Make mouse wheel / trackpad scrolling less jerky
(setq mouse-wheel-scroll-amount '(1))
(dolist (modifier '("" "S-" "C-" "M-"))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" modifier multiple "wheel-" direction ">")) 'ignore))))
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Make default frame height be the screen height
;; @FIX make 
;(add-to-list 'default-frame-alist '(height . 50))

(global-font-lock-mode t)
(setq transient-mark-mode t)
(setq require-final-newline t)
(setq-default indent-tabs-mode nil)

(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key "\M-g" 'beginning-of-buffer)
(global-set-key "\M-G" 'end-of-buffer)
(global-set-key "\C-p" 'goto-line)
(global-set-key "\C-f" 'clang-format-region)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rr 'replace-regexp)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages)
  )

;; make the git dang text wrap nicely
(global-visual-line-mode t)

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'magit
                          'color-theme
                          'markdown-mode
                          'clang-format) 

;; activate installed packages
(package-initialize)

(setq ispell-program-name "/usr/local/bin/aspell")

(setq-default c-default-style "stroustrup"
              tab-stop 4
              tab-width 4
              innamespace 0
              c-basic-offset 4)
(c-set-offset 'innamespace 0)

; style I want to use in c++ mode
(c-add-style "conduce-cxx" 
	     '("stroustrup"
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 4)            ; indent by four spaces
               (tab-stop . 4)
               (tab-width . 4)
	       (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
				   (brace-list-open . 0)
                   (innamespace . 0)))))

(defun conduce-c++-mode-hook ()
  (c-set-style "conduce-cxx")        ; use my-style defined above
  (auto-fill-mode))

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

; CLI matlab from the shell:
; /Applications/MATLAB_R2015b.app/bin/matlab -nodesktop
;
; elisp setup for matlab-mode:
(setq matlab-shell-command "/Applications/MATLAB_R2015b.app/bin/matlab")
(setq matlab-shell-command-switches (list "-nodesktop"))
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")

(add-hook 'c++-mode-hook 'conduce-c++-mode-hook)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(global-set-key [C-f] 'clang-format-region)


(x-focus-frame nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(safe-local-variable-values (quote ((c-default-style . "linux") (tab-stop . 8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
