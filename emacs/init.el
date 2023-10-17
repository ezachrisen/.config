;; init.el --- Espen's emacs settings -*- lexical-binding: t; coding: utf-8 -*-

;;; Commentary:

;; This is the one and only startup file.

;;; Code:

;; -------------------------------------------------- INIT.EL
(setq debug-on-error t)
(setq warning-minimum-level :error) ; quiet, unless it's bad


;; Enable quick access to init.el
(defun load-init()
  "Load the init file for editing."
  (interactive)
  (find-file "~/.config/emacs/init.el"))


(global-set-key (kbd "C-c C-i") 'load-init)


;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

(add-to-list 'load-path "~/.config/emacs/lisp")


(defvar language-server "eglot")

;; ---------------------------------------------------------------------- STARTUP

(setq
 inhibit-startup-message t				; Don't show the welcome screen
 initial-scratch-message ""				; Initial content of the *scratch* buffer
 inhibit-startup-echo-area-message ""	; No system info in echo area
 ring-bell-function 'ignore				; Be silent
 make-backup-files nil					; Don't make backup files
 auto-save-default nil					; Don't make #autosave# files
 scroll-step 1							; When moving off screen, scroll smoothly
 create-lockfiles nil					; Don't create .#filename.txt files
 confirm-kill-emacs  #'yes-or-no-p      ; Confirm to quit
 transient-mark-mode t)					; Highlight the region


;; location where we want Emacs to write customizations, so they don't clutter
;; init.el.
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror)


(defun display-startup-time ()
  "Print a message after startup."
  (message "Emacs loaded in %s with %d garbage collections and %d colors."
		   (format "%.2f seconds"
				   (float-time
					(time-subtract after-init-time before-init-time)))
		   gcs-done
		   (tty-display-color-cells)
		   ))

(add-hook 'emacs-startup-hook #'display-startup-time)


;;;--------------------------------------------------------------------- ENVIRONMENT

;; Ensure that Emacs has the same env variables as my shell
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :config (exec-path-from-shell-initialize)))






;; ---------------------------------------------------------------------- NATIVE COMPILATION

(if (not (version< emacs-version "28.0"))
	(setq package-native-compile t))



;;; --------------------------------------------------------------------- PACKAGE MANAGEMENT

										; use-package bootstrap; installs use-package if not already installed
;(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")) ;
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)


;; Install apps into the OS in a use-package declaration with the
;; :ensure-system-package keyword.
(use-package use-package-ensure-system-package
  :ensure t)





;;; --------------------------------------------------------------------- MISC

(savehist-mode t)                     ; Keep minibuffer history
(fset 'yes-or-no-p 'y-or-n-p)         ; Allow 'y' or 'no' answers

(setq read-buffer-completion-ignore-case t)


;;; --------------------------------------------------------------------- PARENS

(show-paren-mode t)               ; Show matching parens
(setq show-paren-style 'parenthesis ; Show paren only
	  show-paren-delay 0.03
	  show-paren-highlight-openparen t
	  show-paren-when-point-in-periphery t
	  show-paren-context-when-offscreen t
)

(setq blink-matching-paren t)      ; Show off-screen match in echo area



;; ---------------------------------------------------------------------- BACKUPS

;; Save a backup of a file each time it is saved.
(use-package backup-each-save
  :ensure t
  :init
  (setq backup-each-save-mirror-location "~/.backups-per-save"))

(add-hook 'after-save-hook 'backup-each-save)


;;; --------------------------------------------------------------------- APPEARANCE

(setq custom--inhibit-theme-enable nil) ; take effect immediately

;; Make the vertical separator between buffers a thin vertical line
(let ((display-table (or standard-display-table (make-display-table))))
  (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│))
  (setq standard-display-table display-table))


(defadvice load-theme (before theme-dont-propagate activate)
  "Reset the current when switching; prevents carry-over."
  (mapc #'disable-theme custom-enabled-themes))

(use-package gruvbox-theme
  :ensure t
  :defer t)

(use-package nord-theme
  :defer t
  :ensure t)

(use-package leuven-theme
  :ensure t
  :defer t
  )

(use-package material-theme
  :ensure t
  :defer t)

(use-package borland-blue-theme
  :ensure t
  :defer t)


(defun custom-theme-colors(theme &rest args)
  "Modifications to THEME, activate if ARGS is true."
  (if args
	  (cond ((string= theme "gruvbox-dark-soft")
			 (custom-theme-set-faces
			  'gruvbox-dark-soft
			  `(underline ((t (:underline nil))))
			  `(line-number ((t (:background unspecified :foreground "#7c6f64"))))
			  `(isearch ((t (:background "#fabd2f" :foreground "#000000"))))
			  `(isearch-lazy-highlight ((t (:background "#66999D" :foreground "#ffffff"))))
			  `(mode-line ((t (:background "#343130"))))))

			((string= theme "gruvbox-dark-medium")
			 'gruvbox-dark-medium
			 (custom-theme-set-faces
			  'gruvbox-dark-medium
			  `(underline ((t (:underline nil))))
			  `(line-number ((t (:background unspecified :foreground "#7c6f64"))))
			  `(isearch ((t (:background "#fabd2f" :foreground "#000000"))))
			  `(mode-line ((t (:background "#343130"))))
			  `(lazy-highlight ((t (:background "#66999D" :foreground "#ffffff"))))))
			((string= theme "material-light")
			 'material-light
			 (custom-theme-set-faces
			  'material-light
			  `(underline ((t (:underline nil))))
			  `(vertical-border ((t (:background "#FAFAFA" :foreground "gray80"))))
			  `(isearch ((t (:background "#fabd2f" :foreground "#000000"))))
			  `(lazy-highlight ((t (:background "#66999D" :foreground "#ffffff"))))))
			((string= theme "leuven")
			 'leuven
			 (custom-theme-set-faces
			  'leuven
			  `(underline ((t (:underline nil))))
			  `(vertical-border ((t (:background "#FFFFFF" :foreground "gray80"))))
			  `(isearch ((t (:background "#fabd2f" :foreground "#000000"))))
			  `(lazy-highlight ((t (:background "#66999D" :foreground "#ffffff"))))))
			((string= theme "nord")
			 (custom-theme-set-faces
			  'nord
			  `(underline ((t (:underline nil))))
			  `(isearch ((t (:background "#fabd2f" :foreground "#000000"))))
			  `(lazy-highlight ((t (:background "#66999D" :foreground "#ffffff"))))
			  `(line-number ((t (:background unspecified :foreground "#4C566A")))))))))


(advice-add 'load-theme :after #'custom-theme-colors)

(load-theme 'gruvbox-dark-medium t)



										; GUI appearance
(if window-system
	(progn
	  (setq-default cursor-type '(bar . 1))
	  (set-frame-font "Roboto Mono 13" nil t)
	  (tool-bar-mode -1)
	  (scroll-bar-mode -1))
  (progn
	(menu-bar-mode -1)))




;; Define a list of left/right margin pairs and cycle through them.
(let ((margins '((0 . 0) (1 . 0) (5 . 0) (20 . 20) (30 . 30) (50 . 50)))
	  (idx 0))
  (defun wide-margins(&optional arg)
	"Cycle through sets of margin widths of the current buffer for a more pleasant look.
With prefix ARG, apply to all windows, except special ones that contain the '*' character."
	(interactive "P")
	(setq idx (1+ idx))
	(when (>= idx (length margins))
	  (setq idx 0))
	(if arg (dolist (w (window-list))
			  (when (not (string-match-p (regexp-quotex "*") (buffer-name (window-buffer w))))
				(set-window-margins w
									(car (nth idx margins)) (cdr (nth idx margins)))))
	  (set-window-margins (get-buffer-window)
						  (car (nth idx margins)) (cdr (nth idx margins))))))

;;; --------------------------------------------------------------------- LINE NUMBERS

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(global-set-key (kbd "C-c l") 'cycle-line-numbers)

(defun cycle-line-numbers()
  "Cycle through no line numbers, absolute line numbers and relative line numbers."
  (interactive)
  (cond
   ((equal display-line-numbers t)
	(setq display-line-numbers 'relative))
   ((equal display-line-numbers 'relative)
	(setq display-line-numbers nil))
   ((equal display-line-numbers nil)
	(setq display-line-numbers t))))

;; (defun cycle-line-numbers()
;;   "Cycle through no line numbers, absolute line numbers and relative line numbers."
;;   (interactive)
;;   (cond 
;;    ((equal display-line-numbers t)
;; 	(setq display-line-numbers 'relative))
;;    ((equal display-line-numbers 'relative)
;; 	(setq display-line-numbers nil))
;;    ((equal display-line-numbers nil)
;; 	(setq display-line-numbers t))))
   
  



;;; --------------------------------------------------------------------- MOVEMENT

(global-set-key (kbd "M-g")   'goto-line)
(global-set-key (kbd "C-c o") 'occur)			 ; All in buffer
(global-set-key (kbd "C-c *") 'occur-word)		 ; Word at point in buffer
(global-set-key (kbd "M-f")   'forward-to-word)	 ; Go to beginning of word
(global-set-key (kbd "M-b")   'backward-to-word) ; Go back to beginning of word
;(global-set-key (kbd "C-c r l") 'list-registers)
(setq set-mark-command-repeat-pop t)             ; C-u C-SPC C-SPC cycles positions
(setq global-mark-ring-max 16)                   ; Max positions in global mark ring
(save-place-mode 1)                              ; Return to last position on file open

(defun occur-word()
  "List all occurences of current word in buffer."
  (interactive)
  (occur (current-word)))


;; Ace Window lets you jump to any window on the screen.
(use-package ace-window
  :ensure t
  :defer t
  :bind (("M-j" . ace-window))
  :config
  (setq aw-dispatch-always t)
  :custom-face
  (aw-background-face ((t (:background unspecified :foreground "#585858"))))
  (aw-leading-char-face ((t (:background unspecified  :foreground "#ff0000")))))


;; Beacon helps you find your cursor
(use-package beacon
  :ensure t
  :defer t
  :config
  (beacon-mode)
  (setq beacon-blink-delay 0.2)
  (setq beacon-blink-duration 0.7)
  (setq beacon-color 0.1)
  (setq beacon-size 50)
  (setq beacon-blink-when-buffer-changes nil)
  (setq beacon-blink-when-window-changes nil)
  (setq beacon-blink-when-point-moves-horizontally nil)
  (setq beacon-blink-when-point-moves-vertically nil)
  (setq beacon-blink-when-window-scrolls nil)
  :bind
  ("M-SPC" . beacon-blink))




;;; --------------------------------------------------------------------- EDITING

(setq-default 
 tab-width 2
 fill-column 90)			; Fill column; show with cheat sheet
(delete-selection-mode 1)				; Type over selection to replace it
(put 'downcase-region 'disabled nil)	; Allow downcase/upcase
(put 'upcase-region 'disabled nil)

(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to  to autoindent.")


(defun open-next-line (arg)
  "Move to the next line and then opens a line.
With ARG, opens ARG lines.
See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent
	(indent-according-to-mode)))

(defun open-previous-line (arg)
  "Open a new line before the current one.  With ARG, open ARG lines.
See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
	(indent-according-to-mode)))



(defun aya-open-line ()
  "Call `open-line', unless there are abbrevs or snippets at point.
In that case expand them.  If there's a snippet expansion in progress,
move to the next field. Call `open-line' if nothing else applies."
  (interactive)
  (cond ((expand-abbrev))

        ((and (fboundp 'yas-active-snippets) (yas-active-snippets))
         (yas-next-field-or-maybe-expand))

        ((ignore-errors
           (yas-expand)))

        (t
         (open-previous-line 1))))


;; (global-set-key (kbd "C-o") 'open-previous-line)
(global-set-key (kbd "M-o") 'open-next-line)
(global-set-key (kbd "C-o") 'aya-open-line) 
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-a") 'beginning-of-buffer)
;;(global-set-key (kbd "C-x C-e") 'end-of-buffer)


;; required to make the meta-arrow keys work in some terminals
(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])


;; Drag lines or regions with key strokes.
(use-package drag-stuff
  :ensure t
  :defer t
  :config (drag-stuff-global-mode)
  :bind
  ([(meta up)] . drag-stuff-up)       ; meta-up drags line or region up
  ([(meta down)] . drag-stuff-down))  ; meta-dn drags line or region down


(defun copy-whole-line (arg)
  "Copy lines (as many as prefix ARG) in the kill ring.
Works in read-only buffers."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
				  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))



(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
			   (`(,beg . ,end) (get-positions-of-line-or-region))
			   (region (buffer-substring-no-properties beg end)))
	(dotimes (_i arg)
	  (goto-char end)
	  (newline)
	  (insert region)
	  (setq end (point)))
	(goto-char (+ origin (* (length region) arg) arg))))

(defun get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
	(if (and mark-active (> (point) (mark)))
		(exchange-point-and-mark))
	(setq beg (line-beginning-position))
	(if mark-active
		(exchange-point-and-mark))
	(setq end (line-end-position))
	(cons beg end)))

(global-set-key (kbd "M-\"") 'duplicate-current-line-or-region)

(defun my-copy-or-kill-region (arg)
  "Copy or kill the region depending on the prefix arg."
  (interactive "P")
  (if arg
      (kill-region (region-beginning) (region-end))
    (kill-ring-save (region-beginning) (region-end))))

(global-set-key (kbd "M-w") 'my-copy-or-kill-region)


;; Misc editing commands
(global-set-key (kbd "M-k")     'kill-whole-line)
(global-set-key (kbd "M-l")     'copy-whole-line)
(global-set-key (kbd "M-Z")		'zap-to-char)
(global-set-key (kbd "M-z")		'zap-up-to-char)
(global-set-key (kbd "C-c a")   'align-regexp)

;; Join next line to this one. With universal argument,
;; joins this line with the one above.
(global-set-key (kbd "C-j")		'join-line)



(use-package expand-region
  :ensure t
  :defer t
  :bind ("C-\\" . er/expand-region))




;;; --------------------------------------------------------------------- YANKING


(defun yank-before()
  "Open a new line before this line and yank."
  (interactive)
  (open-previous-line 1)
  (yank))

(defun yank-after()
  "Open a new line after this line and yank."
  (interactive)
  (open-next-line 1)
  (yank))

(global-set-key (kbd "C-c y b") 'yank-before)
(global-set-key (kbd "C-c y a") 'yank-after)






;;; --------------------------------------------------------------------- MOUSE

;; Mouse wheel settings help improve mouse scrolling.
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(0.05))

;; XTerm mode enables mouse use in some terminals.
;; Toggle it on and off to enable "normal" use of the mouse, to select for
;; system clipboard copy, for example.
(xterm-mouse-mode 1)
(global-set-key (kbd "C-c k") 'xterm-mouse-mode)




;;; --------------------------------------------------------------------- COMMENTS

(defun comment-or-uncomment-line-or-region (arg)
  "Comments or uncomments the current line or region. 
With ARG, toggles the comment on that many lines."
  (interactive "P")
  (if arg
	  (comment-line arg)
	(if (region-active-p)
		(comment-or-uncomment-region (region-beginning) (region-end))
	  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))


(global-set-key (kbd "M-;") 'comment-or-uncomment-line-or-region)
(global-set-key (kbd "C-x ;") 'comment-line) ; the default binding, C-x C-; doesn't work in terminal



;;; --------------------------------------------------------------------- SEARCHING

										; Search and Replace
(setq case-fold-search t)             ; Case-insensitive searches
(setq query-replace-highlight t)      ; Highlight matches during replacement

										; In incremental search, run occur with the last search string
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

										; At then end of C-s, leave point at the beginning of the match, not the end
										; When searching for "begins with...", we usually want to be at the beginning of
										; the word, not at the end of it.
(add-hook 'isearch-mode-end-hook
		  #'endless/goto-match-beginning)

(defun endless/goto-match-beginning ()
  "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
  (when (and isearch-forward
			 (number-or-marker-p isearch-other-end)
			 (not mark-active)
			 (not isearch-mode-end-hook-quit))
	(goto-char isearch-other-end)))




;;; --------------------------------------------------------------------- HYDRA

;; Hydra combines displaying and invoking a list of commands.
;; I use it extensively for "cheat sheets" of commands.
(use-package hydra
  :ensure t)

;; ;; Enable a single key binding to call up mode-specific Hydra.
(use-package major-mode-hydra
  :ensure t
  :demand t
  ;; :bind
  ;; ("C-c c" . major-mode-hydra)
  :config
  (setq major-mode-hydra-separator "-")
  (setq major-mode-hydra-invisible-quit-key "q"))


(defun my-hydra-formatter(arg)
  "Replace the : after each key in ARG.  This cleans up the hydra display."
  (replace-regexp-in-string "\\(_._\\):" "\\1 " arg))



;;; --------------------------------------------------------------------- FLYCHECK

;; Flycheck provides on-the-fly syntax checking, displaying marks in the
;; margin where issues have been identified.
;; See https://www.flycheck.org.
(use-package flycheck
  :ensure t
  :config
  (setq flycheck-standard-error-navigation nil
		flycheck-indication-mode 'left-margin)
  :init (global-flycheck-mode)
  :bind (("M-n" . flycheck-next-error)
		 ("M-p" . flycheck-previous-error)
		 ("C-c e" . flycheck-list-errors)))


;; To tell flycheck where to find proto files, set flycheck-protoc-import-path.
;; See example .dir-locals.el at the end of this file.


(with-eval-after-load 'flycheck
  (defun my/set-flycheck-margins ()
    "Adjust margins and fringes when using flycheck."
    (flycheck-define-error-level 'error
      :severity 100
      :compilation-level 2
      :overlay-category 'flycheck-error-overlay
      :fringe-face 'flycheck-fringe-error
      :error-list-face 'flycheck-error-list-error)
    (setq left-fringe-width 16 right-fringe-width 8
		  left-margin-width 1 right-margin-width 0)
    (flycheck-refresh-fringes-and-margins))
  (add-hook 'flycheck-mode-hook #'my/set-flycheck-margins)
  
  ;; Use a fancy character when indicating a flycheck message
  ;; in the fringe.

  (flycheck-redefine-standard-error-levels "✱"))



;;; --------------------------------------------------------------------- FLYSPELL

;; Flyspell uses Ispell to check words as you type.

(global-set-key (kbd "C-c z") 'flyspell-goto-next-error)
(setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))



;;; --------------------------------------------------------------------- LSP

(when (string= language-server "lsp") 

(use-package lsp-ui
  :ensure t
  :config 
  (setq lsp-ui-doc-enable nil
		lsp-ui-doc-show-with-cursor nil))

(use-package lsp-mode
  :ensure t
  :defer t
  :ensure-system-package (gopls . "go install golang.org/x/tools/gopls@latest")
  :commands (lsp lsp-deferred)
  :bind (("C-c x"	. 'lsp-find-references)
		 ("C-c d"	. 'lsp-describe-thing-at-point)
		 ("C-c i"   . 'lsp-ui-imenu)
		 ("C-c r"   . 'lsp-rename)		 
		 ("M-'"		. 'xref-find-definitions-other-window))
  :config
  (setq lsp-enable-symbol-highlighting t		; highlight references of symbol at point
		lsp-lens-enable nil
										; lsp-completion-provider :none
		lsp-enable-links nil					; turn off links; they're ugly
		lsp-file-watch-threshold 5000			; max files to watch before warning
										; lsp-ui-doc-enable nil
										; lsp-diagnostic-package :auto
		eldoc-echo-area-use-multiline-p nil		; don't allow resizing echo area
		lsp-eldoc-render-all nil				; only show symbol info in echo area
										; lsp-headerline-breadcrumb-enable t		; show file / func at top of screen
		read-process-output-max (* 1024 1024)	; read large input from lsp provider
		lsp-log-io nil							; turn off lsp logging

										; Special setting for gopls: set the GOFLAGS environment variable.
										; If this is not set, gopls will not work properly in test files with build tags.
		lsp-go-env '((GOFLAGS . "-tags=integration,spanner")))
  ;; Hydra for common LSP tasks. See key binding under use-package lsp-mode.
  ;; (pretty-hydra-define hydra-lsp
  ;; 	(:title "LSP" :separator " " :formatter my-hydra-formatter :idle 0.5 :color blue)
  ;; 	("Info"
  ;; 	 (("d" lsp-describe-thing-at-point "describe thing" )
  ;; 	  ("c" lsp-find-references "find references")
  ;; 	  ("t" lsp-find-type-definition "find type def")
  ;; 	  ("x" xref-find-definitions-other-window "def other window")
  ;; 	  ("u" lsp-treemacs-symbols "treemacs symbols")
  ;; 	  ("g" lsp-ui-doc-glance "doc glance"))
  ;; 	 "Actions"
  ;; 	 (("a" lsp-execute-code-action "exec code action")
  ;; 	  ("r" lsp-rename "rename")
  ;; 	  ("s" lsp-toggle-symbol-highlight "symbol highlight"
  ;; 	   :toggle lsp-enable-symbol-highlighting :color red)
  ;; 	  ("i" lsp-ui-imenu "Imenu"))))

  :hook ((web . lsp)))

)

;;; --------------------------------------------------------------------- TREESITTER


(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))



;;; --------------------------------------------------------------------- EGLOT

(when (string= language-server "eglot")
  (use-package eglot 
  :bind (("C-c r"   . 'eglot-rename)		 
		 ("M-'"		. 'xref-find-definitions-other-window))
  :config
  (setq 
   eldoc-echo-area-use-multiline-p 1
   eglot-ignored-server-capabilities '( :documentHighlightProvider :codeLensProvider))))
	
;;   (setq eldoc-echo-area-use-multiline-p 1)
;;   (setq eglot-ignored-server-capabilities '( :documentHighlightProvider :codeLensProvider))
;;   (global-set-key (kbd "C-c r")  'eglot-rename)
;;   (global-set-key (kbd "M-'")  'xref-find-definitions-other-window)
;; )


;;; --------------------------------------------------------------------- SNIPPETS

;; YASnippet is a templating engine to enable snippets to be inserted with
;; intelligent placeholders.
(use-package yasnippet
  :ensure t
  :defer t
  :bind (("C-c y n" . 'yas-new-snippet)
		 ("C-c y c" . 'yas-expand)
		 ("C-c y l" . 'yas-describe-tables))
  :config
  (yas-global-mode 1)
  :init 
  (load "yasnippet.el") ;; to fix weird yas bug
  :hook (go-ts-mode . yas-minor-mode))




;;; --------------------------------------------------------------------- PROJECTILE

;; Projectile creates the concept of a "project", which other commands (such as
;; helm-projectile) can use to do a smarter job of finding files or text.
;; A project is typically defined with the presence of a Git repo.
(use-package projectile
  :ensure t
  :defer t
  :init
  (projectile-mode +1)
  :config
  ;; Ignore generated Go protobuf files
  (setq projectile-git-command "git ls-files -zco --exclude-standard ':!:*.pb.go'")
  (setq projectile-mode-line-function '(lambda ()
										 (format " [%s]" (projectile-project-name)))))

;; To tell projectile to ignore files and folders, set
;; projectile-globally-ignored-directories and projectile-globally-ignored-file-suffixes.
;; See sample .dir-locals.el at the end of this file.


;;; --------------------------------------------------------------------- HELM

;; Helm is a framework for incremental completions and narrowing of functions.
;; I use it for finding files and buffers, grepping through files, etc. 
(use-package helm
  :ensure t
  :defer t
  :config
  (setq helm-default-display-buffer-functions '(display-buffer-in-side-window))
  :bind (("C-c b" . 'helm-mini)
		 ("C-h SPC" . 'helm-all-mark-rings)))


;; Provides easy navigation within a project.
(use-package helm-projectile
  :ensure t
  :defer t
  :config
  (setq  projectile-indexing-method 'hybrid)
  :bind (("C-c p s" . 'helm-projectile-switch-project)
		 ("C-c m" . 'projectile-compile-project)
		 ("C-c C-f" . 'helm-projectile)))

										;		 ("C-c f" . 'helm-projectile)))


;; Provides grepping files within a project.
(use-package helm-git-grep
  :ensure t
  :defer t
  :config
  (setq helm-git-grep-pathspecs
		'("*.*"  ":!:*/vendor/*.*" ":!:*.pb.go"  ":!:*.pb.*.go" ":!:*_pb.*"))
  (setq helm-boring-buffer-regexp-list
		(append helm-boring-buffer-regexp-list
				(list (rx "*magit-") (rx "*Flycheck") (rx "*Messages")
					  (rx "*gopls") (rx "*lsp-") (rx "*Help"))))
  :bind (("C-c p g" . 'helm-git-grep)
		 ("C-c p *" . 'helm-git-grep-at-point)))


(advice-add 'helm-git-grep-args :override
			(lambda ()
			  "Override helm's git grep args in order to add '-untracked' to the args."
			  (delq nil
					(append
					 (list "--no-pager" "grep" "--null" "-n" "--no-color" "--untracked"
						   (if helm-git-grep-ignore-case "-i" nil)
						   (if helm-git-grep-wordgrep "-w" nil)
						   (helm-git-grep-showing-leading-and-trailing-lines-option))
					 (nbutlast
					  (apply 'append
							 (mapcar
							  (lambda (x) (list "-e" x "--and"))
							  (split-string helm-pattern " +" t))))
					 (helm-git-grep-pathspec-args)))))


;; Navigate symbols identified by LSP with Helm. 
;; Currently not bound to anything, but may be useful.
(use-package helm-lsp
  :ensure t
  :defer t
  :commands helm-lsp-workspace-symbol)






;;; --------------------------------------------------------------------- MAGIT

;; Magit is the Git utility for Emacs.
(use-package magit
  :ensure t
  :defer t
  :bind (("C-c g" . magit)))



(defun magit-diff-to-main()
  "Perform a diff of the current state vs main.
Represents a preview of what a pull request diff will look like."
  (interactive)
  (magit-diff-range "main"))

(global-set-key (kbd "C-c C-\\") 'magit-diff-to-main)




;;; --------------------------------------------------------------------- COMPANY


;; Company mode provides symbol completion. 
(use-package company
  :ensure t
  :defer t
  :bind (:map company-active-map
			  ("C-n" . company-select-next)
			  ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0			; display completions immediatley
		lsp-completion-provider :capf	; set company mode as the LSP completion provider
		global-company-mode 1			; enable always
		company-text-icons-mapping		; characters in completion popup
		'((array "a" font-lock-type-face)
		  (boolean "b" font-lock-builtin-face)
		  (class "" font-lock-type-face)
		  (color "#" success)
		  (constant "" font-lock-constant-face)
		  (constructor "c" font-lock-function-name-face)
		  (enum-member "e" font-lock-builtin-face)
		  (enum "e" font-lock-builtin-face)
		  (field "ﰠ" font-lock-variable-name-face)
		  (file "f" font-lock-string-face)
		  (folder "d" font-lock-doc-face)
		  (interface "" font-lock-type-face)
		  (keyword "k" font-lock-keyword-face)
		  (method "" font-lock-function-name-face)
		  (function "" font-lock-function-name-face)
		  (module "" font-lock-type-face)
		  (numeric "n" font-lock-builtin-face)
		  (operator "o" font-lock-comment-delimiter-face)
		  (property "p" font-lock-variable-name-face)
		  (reference "r" font-lock-doc-face)
		  (snippet "S" font-lock-string-face)
		  (string "s" font-lock-string-face)
		  (struct "" font-lock-variable-name-face)
		  (text "r" shadow)
		  (type-parameter "p" font-lock-type-face)
		  (unit "u" shadow)
		  (value "v" font-lock-builtin-face)
		  (variable "" font-lock-variable-name-face)
		  (t "." shadow))))


;; In LSP mode, set completion-at-point-functions.
(add-hook 'lsp-managed-mode-hook (lambda ()
								   (setq-local company-backends
											   '(company-capf company-yasnippet))))




;;; --------------------------------------------------------------------- TREEMACS

;; Treemacs displays a file browser.
(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-follow-after-init nil
		treemacs-width 35
		treemacs-indentation 2
		treemacs-recenter-after-file-follow nil
		treemacs-silent-refresh t
		treemacs-silent-filewatch t
		treemacs-sorting 'alphabetic-asc
		treemacs-show-hidden-files t
		treemacs-show-cursor nil
		treemacs-follow-mode nil
		treemacs-filewatch-mode nil
		treemacs-git-mode 'deferred
		treemacs-project-follow-mode t
		treemacs-is-never-other-window t
		treemacs-deferred-git-apply-delay      0.5
		treemacs-directory-name-transformer    #'identity
		treemacs-display-in-side-window        nil)
  (treemacs-fringe-indicator-mode 'always)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  :bind (:map global-map
			  ;; ("C-x x" . treemacs-add-and-display-current-project)
			  ("C-x x" . treemacs-select-window)
			  ("C-x t" . treemacs)))

(use-package treemacs-projectile
  :ensure t
  :defer t)

(with-eval-after-load 'treemacs
  (defun treemacs-custom-filter (file _)
	(or (s-ends-with? ".DS_Store" file)
		(s-ends-with? ".DS_Store" file)))
  (push #'treemacs-custom-filter treemacs-ignored-file-predicates))

(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; #799B8E

;;; --------------------------------------------------------------------- COMPILATION

(setq compilation-always-kill t			; Don't ask to kill the running process
	  compilation-context-lines t		; Show an arrow next to the current error
	  compilation-scroll-output nil)	; Stay at the top of the output window



(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-x /") 'next-error)
(global-set-key (kbd "C-x '") 'previous-error)


(global-set-key (kbd "C-c |") (lambda () 
								"Put compilation in the current window."
								(interactive) 
								(switch-to-buffer "*compilation*")))


(defun compile-wrap(cmd)
  "Run the compilation CMD without modifying the compile command for this buffer."
  (setq current-compile-command compile-command)
  (compile cmd)
  (setq compile-command current-compile-command))


(require 'ansi-color)

(defun colorize-compilation-buffer ()
  "Enable colors in compilation buffers."
  (read-only-mode -1)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode 1))

;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; (add-hook 'compilation-mode-hook
;; 		  (lambda ()
;; 			(font-lock-add-keywords nil
;; 									'((" "
;; 									   (0 '(face nil font-lock-face nil
;; 												 compilation-message nil help-echo nil mouse-face nil) t)))
;; 									'append)))



;; (defun my-next-error (&optional arg reset)
;;   "Better way of going to the next error in a compilation buffer.
;; ARG and RESET are ignored."
;;   (interactive "P")
;;   (let ((buffer (next-error-find-buffer t)))
;; 	(when buffer
;; 	  ;; We know here that next-error-function is a valid symbol we can funcall
;; 	  (switch-to-buffer-other-window buffer)
;; 	  (compilation-next-error 1)
;; 	  (compile-goto-error)
;; 	  )))

;; (global-set-key (kbd "C-x \\") 'my-next-error)



;;; --------------------------------------------------------------------- SERVERS

;; Servers are external processes run and controlled from within Emacs.
;; We use Prodigy to manage the services, with hydra and some custom functions
;; to rapidly start/stop and show/hide log output by name/tag. 
;; Services are defined either in code with the prodigy-define-service 
;; function, or by setting a variable called local-services, then calling 
;; define-local-services. See define-local-services for documentation on the 
;; format of the local-services variable. 

(use-package prodigy
  :defer t
  :ensure t)

(defun define-local-services()
  "Create services from the variable local-services.
The variable must be a list of cons cells, where the car is the
name and the cdr is a list of properties corresponding to the
property names below. It is useful to define multiple tags for
services to either start/stop them as a group or individually.

Example of a .dir-locals.el definition:

 (local-services . 
 								 ((:name \"gRPC Server\"
 												 :tags (g grpc a)
 												 :command\"make\"
 												 :cwd \"$HOME/code/xyz\"
 												 :args (\"start_grpc\") 
 												 :env ((\"CONSOLE_FORMAT\" \"yes\"))							
 									       :start-sequences ((a 1)))
 									(:name \"UI Server\"
 												 :tags (u ui a)
 												 :command \"make\"
 												 :cwd \"$HOME/code/xyz\"
 												 :args (\"start_ui\") 
 												 :env ((\"CONSOLE_FORMAT\" \"yes\"))
 									       :start-sequences ((a 2)))
 								 (:name \"Spanner Emulator\"
 												:tags (d)
 												:command \"docker\"
 												:cwd \"$HOME\"
 												:args (\"run\" \"-p\" \"9010:9010\" \"-p\" \"9020:9020\" \"gcr.io/cloud-spanner-emulator/emulator\") 
 												:env ((\"CONSOLE_FORMAT\" \"yes\")))))
"
  (interactive)
  (message "loading services")
  (when (not (boundp 'local-services))
	(hack-dir-local-variables-non-file-buffer))
  (when(boundp 'local-services) 
	(dolist (x local-services) 
	  (message "Defining service %s" (plist-get x :name))
	  (prodigy-define-service
		:name (plist-get x :name)
		:path (substitute-in-file-name (or (plist-get x :path)
										  (plist-get x :cwd)))
		:cwd (substitute-in-file-name (plist-get x :cwd))
		:command (plist-get x :command)
		:env (plist-get x :env)
		:args (plist-get x :args)
		:tags (plist-get x :tags)
		:sudo (if (string= (plist-get x :sudo) "t") t nil)
		:stop-signal (or (plist-get x :stop-signal)
						 'sigkill)
		:kill-process-buffer-on-stop (or (plist-get x :kill-process-buffer-on-stop)
										 "unless-visible")))
	(setq prodigy-services (sort prodigy-services (lambda (a b) (string< (upcase (plist-get a :name)) (upcase (plist-get b :name))))))

))

 
(defun restart-services(arg)
  "Start the services tagged with ARG or named ARG."
  (interactive "s(Re)start services with tag or name: ")   
  (do-with-services arg 'prodigy-restart-service 5 "Starting" "Started"))

(defun kill-services(arg)
  "Stop the services tagged with ARG or named ARG."
  (interactive "sKill services with tag or name: ") 
  (do-with-services arg (lambda(s)(prodigy-stop-service s t)) 0 "Killing" "Killed"))

(defun show-logs(arg)
  "Show logs for services tagged with ARG or named ARG.
If ARG is 'a', toggles the group of services tagged with a."
  (interactive "sShow logs for services with tag or name: ") 
  (setq slotnumber 1)
  (message "starting in slot %d" slotnumber)
  (setq slot 1)
	(do-with-services arg (lambda(s &rest rest) (display-service-log-in-left-side-slot s)) 0 "Showing" "Y"  ))

(defun display-service-log-in-left-side-slot (service)
  (message "display service %s in slot number %d" (plist-get service :name) slot)
  (let ((params `((side . left) (slot . ,slot))))
	(-if-let (buffer (get-buffer (prodigy-buffer-name service)))
		(progn
		  (display-buffer-in-side-window buffer params)		  
		  (setq slot (+ slot 1))))))

(defun hide-logs(arg)
  "Hide logs for services tagged with ARG or named ARG."
  (interactive "sHide logs for services with tag or name: ") 
  (do-with-services arg (lambda(s &rest rest)(-if-let (buffer (get-buffer (prodigy-buffer-name s)))
									  (delete-windows-on buffer))) 0 ))

(defun do-with-services(arg fun delay &optional start-msg finished-msg)
  "Apply the FUN to all Prodigy services that have a name or tag matching ARG.
FUN must take one argument, the prodigy service.
Waits DElAY seconds between applying FUN to each service. 
Displays START-MSG when FUN is applied, and FINISHED-MSG when FUN is done.
START-MSG and FINISHED-MSG default to 'Processing' and 'Finished' if not set."
  (dolist (s prodigy-services)
	(when (or (member (intern arg) (plist-get s :tags))
			  (eq arg (plist-get s :name))) 
	  (message "%s %s" (or start-msg "Processing") (plist-get s :name))
	  (funcall fun s (lambda() (message "%s %s" (or finished-msg "Finished") (plist-get s :name))))
	  (sit-for delay))))



(defhydra hydra-services (:color blue :idle 0.5)
  "
Services
   "
  ("q" nil "quit")
  ("r" restart-services  "(Re)start" :column "Control")
  ("a" ((lambda () (restart-services "a")))  "(Re)start a" )
  ("b" ((lambda () (restart-services "b")))  "(Re)start b" )
  ("k" kill-services "Kill")  
  ("l" show-logs "Show logs" :column "Logs")
  ("h" hide-logs "Hide logs")
  ("p" prodigy "List" :column "Processes")
  ("d" define-local-services "Load service definitions"))

(global-set-key (kbd "C-c s") 'hydra-services/body)


;;; --------------------------------------------------------------------- TEXT MODE

(add-hook 'text-mode-hook
		  (lambda ()
			(setq tab-width 4)
			(visual-line-mode)))




;;; --------------------------------------------------------------------- GO FUNCTIONS


(defmacro bool-prop (name default docstring)
  "Create a function and a boolean variable with NAME and initial value DEFAULT.
The function and the variable are given DOCSTRING."
  `(list 
	(defun ,(intern name) ()
	  ,docstring
	  (interactive)
	  (if ,(intern name)
		  (setq ,(intern name) nil)
		(setq ,(intern name) t)))
	(defvar ,(intern name) ,default ,docstring)))



(defmacro string-prop (name prompt default docstring)
  "Create a function and a string variable with NAME and initial value DEFAULT.
When NAME is called, it will read a value with PROMPT. The
function and the variable are given DOCSTRING."
  `(list 
	(defun ,(intern name) (arg)
	  ,docstring
	  (interactive (list (read-string ,prompt ,(intern name))))
	  (if (string= arg "")
		  (setq ,(intern name) nil)
		(setq ,(intern name) arg)))
	(defun ,(intern (format "%s-display" name)) ()
	  (concat ,prompt ,(intern name)))
	(defvar ,(intern name) ,default ,docstring)))


;; These are flags and settings we'll access with the Go mode hydra defined later.
(bool-prop "go-test-short" t "Test with the -short flag.")
(bool-prop "go-test-verbose" nil "Test with the -v flag.")
(bool-prop "go-test-package-only" t "Only test the package on saving.
If go-test-path is set, test the package in that path, otherwise
test the package of the current buffer. If nil, test the module
on save.")

(string-prop "go-test-timeout" "-timeout=" "" "Run go test with -timeout=ARG.")
(string-prop "go-test-run" "-run=" "" "Run go test with -run=ARG.")
(string-prop "go-test-tags" "-tags=" "" "Run go test with -tags=ARG.")
(string-prop "go-test-env"  "env=" "" "Set environment variables before running go test.")
(string-prop "go-test-path" "-path=" "" "Run package-level tests in this directory.")


(defun go-lint (&optional modulep)
  "Run the golangci-lint linter in a package (default) or the module if MODULEP is t."
  (interactive)
  (let((cmd (concat
			 (cond (modulep  (concat "cd " (go-module-dir) " && ")))
			 "golangci-lint run")))
	(message "Command = %s" cmd)
	(compile-wrap cmd)))


(defun go-module-name()
  "Determine the name of the current module."
  (interactive)
  (replace-regexp-in-string "\n$" ""  (shell-command-to-string "cat $(go env | grep GOMOD= | sed 's/GOMOD=\"//' | sed 's/\"//') | grep \"^module\" | sed 's/module //' | sed 's/\\n//'")))

(defun go-module-dir()
  "Determine the root directory of the current module."
  (interactive)
  (replace-regexp-in-string "\n$" ""  (shell-command-to-string "go env | grep GOMOD= | sed 's/GOMOD=\"//' | sed 's/\"//' | sed 's/go.mod//'")))

(defun go-test-coverage-in-emacs()
  "Generate test coverage output and display in a separate buffer."
  (interactive)
  (setq outputfile (make-temp-file "foo"))
  (shell-command (concat "go test -coverprofile=" outputfile))
  (go-coverage outputfile))

(defun go-open-alternate-file()
  "Open the test file for a go file and vice versa."
  (interactive)
  (if (string-match-p (regexp-quote "_test") buffer-file-name)
	  (find-file-other-window 
	   (concat (replace-regexp-in-string 
				(regexp-quote "_test") 
				"" 
				(file-name-sans-extension buffer-file-name))  ".go"))
	(find-file-other-window 
	 (concat (file-name-sans-extension buffer-file-name) "_test.go"))))

(defun go-generate-module()
  "Run go generate on the Go module."
  (interactive)
  (compile-wrap (concat "cd  "
						(go-module-dir)
						" && go generate")))


(defun go-test(&optional modulep)
  "Run go test on a package (default) or the module if MODULEP is t."
  (interactive)
  (let((cmd (concat
			 (cond ((or modulep (not go-test-package-only)) (concat "cd " (go-module-dir) " && "))
	 			   ((not (string= go-test-path ""))(concat "cd " go-test-path " && ")))
			 (concat " " go-test-env " ")
			 "go test "
			 (if (not (string= go-test-run "")) (concat " -run=" go-test-run " "))
			 (if (not (string= go-test-tags "")) (concat " -tags=" go-test-tags " "))
			 (if (not (string= go-test-timeout "")) (concat " -timeout=" go-test-timeout " "))
			 (if go-test-short '" -short ")
			 (if go-test-verbose	'" -v ")
			 (if (or modulep (not go-test-package-only)) "./..."))))
	(message "Command = %s" cmd)
	(compile-wrap cmd)))

(defun go-test-module-benchmarks()
  "Run benchmarks for the Go module."
  (interactive)
  (compile-wrap (concat "go test "
						" -bench=. "
						(go-module-name)
						" ./...")))



;; Go package site serves a web site for browsing go documentation for a module.
;; We start and manage the package site service with Prodigy.

(defvar go-package-site-dir nil
  "Directory where the Go pkgsite server should start.
Usually this is where go.mod is located. Set this explicitly, or load from a
.dir-locals.el file local to the project.")

;; We'll call this in the Go mode hook
(defun define-pkgsite-service(arg)
  "Define a Prodigy service to run a Go documentation server (pkgsite).
ARG is the full path to the directory where you want to run the
 service. See the go-package-site-dir variable."
  (prodigy-define-service
	:name "Go Package Site"
	:command "pkgsite"
	:args '("-list=false" "-http=localhost:7070")
	:cwd arg
	:tags '(work)
	:stop-signal 'sigkill
	:kill-process-buffer-on-stop t))


;;; --------------------------------------------------------------------- GO


;; Go mode setup

										;: shortcut to this location: gox

;; (use-package go-mode
;;   :ensure t
;;   :defer t
;;    :mode "\\(\\.go\\|go.mod\\|go.sum\\)\\'"
;;   ;;:mode "\\.go\\"
;;   :mode-hydra
;;   ((:title "Go" :separator " " :formatter my-hydra-formatter :idle 0.5)
;;    ("Testing"
;; 	(("m" ((lambda()(go-test t))) "module")
;; 	 ("t" go-test "package")
;; 	 ("o" go-test-package-only "package only on save" :toggle t)
;; 	 ("c" go-test-coverage-in-emacs "coverage")
;; 	 ("b" go-test-module-benchmarks "benchmarks")
;; 	 ("e" go-test-env (go-test-env-display))
;; 	 ("u" go-test-run (go-test-run-display))
;; 	 ("g" go-test-tags (go-test-tags-display))
;; 	 ("z" go-test-timeout (go-test-timeout-display))
;; 	 ("s" go-test-short "short" :toggle t)
;; 	 ("v" go-test-verbose "verbose" :toggle t)
;; 	 ("h" go-test-path (go-test-path-display))
;; 	 )
;; 	"Lint"
;; 	(("x" ((lambda()(go-lint t)))  "module")
;; 	 ("l" go-lint "package")
;; 	 )
;; 	"Navigation"
;; 	(("a" go-open-alternate-file "alternate file")
;; 	 ("f" helm-projectile "find file")
;; 	 ("i" lsp-ui-imenu "imenu"))
;; 	"Git"
;; 	(("d" magit-diff-to-main "diff to main"))
;; 	"Misc"
;; 	(("k" go-generate-module "go generate")
;; 	 ("r" lsp-rename "rename")
;; 	 )))
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.gohtml\\'" . html-mode))
;;   (setq gofmt-command "goimports")
;;   (add-hook 'go-mode-hook
;; 			(lambda ()	
;; 			  (message "loading go-mode hook")
;; 			  (common-go-setup))))


(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-hook 'go-ts-mode-hook 
		  (lambda()
			(common-go-setup)))


  (pretty-hydra-define hydra-go-test
	(:title "Go Test" :separator " " :formatter my-hydra-formatter :idle 0.1 :color blue)
	("Test"
	 (("m" ((lambda()(go-test t))) "module")
	 ("p" go-test "package")
	 ("o" go-test-package-only "package only on save" :toggle t)
	 ("c" go-test-coverage-in-emacs "coverage")
	 ("b" go-test-module-benchmarks "benchmarks")
	 ("e" go-test-env (go-test-env-display))
	 ("u" go-test-run (go-test-run-display))
	 ("g" go-test-tags (go-test-tags-display))
	 ("z" go-test-timeout (go-test-timeout-display))
	 ("s" go-test-short "short" :toggle t)
	 ("v" go-test-verbose "verbose" :toggle t)
	 ("h" go-test-path (go-test-path-display))
	 ("t" ((lambda()(interactive)(toggle-window "*compilation*"))) "toggle *compilation*")
	 )))

(global-set-key (kbd "C-c t") 'hydra-go-test/body)

(require 'gofmt)
(not go-package-site-dir)
(defun common-go-setup() 
  (hack-dir-local-variables-non-file-buffer)		 ; load .dir-locals.el
  (when (not go-package-site-dir)
	(setq go-package-site-dir (projectile-project-root)))
  (define-pkgsite-service go-package-site-dir)		 ; set dir for Go doc server
  (define-local-services)							 
  (if (not (string-match "go" compile-command))		 ; set default compile command
	  (set (make-local-variable 'compile-command)
		   "go build -v && go test -v -short && go vet"))
  (setq fill-column 90)
;;  (setq indent-tabs-mode nil) 
  (setq tab-width 2)
  ;;(setq indent-line-function 'insert-tab)
  (setq gofmt-command "goimports")
  (setq go-ts-mode-indent-offset 2)
  (subword-mode 1)
  (company-mode)
;;  (electric-pair-mode)
  (cond 
   ((string= language-server "lsp")
	(message "activating lsp in go mode")
	(lsp-deferred))
   ((string= language-server "eglot")
	(message "activating eglot in go mode")
	(eglot-ensure)))
  (add-hook 'before-save-hook 'gofmt-before-save nil 'make-it-local))
;  (add-hook 'after-save-hook 'go-test nil 'make-it-local))





;; --------------------------------------------------------------------- DART / FLUTTER

(use-package dart-mode
  :ensure t
  :defer t
  :hook ((dart-mode-hook . lsp)))


(use-package lsp-dart
  :ensure t)

;; --------------------------------------------------------------------- MISC LANGUAGES

(add-hook 'c-mode-common-hook
		  (lambda ()
			(message "Loaded C hook")
			(setq tab-width 4)
			(define-key c-mode-base-map (kbd "C-c C-g") 'compile)))




(add-hook 'html-mode-hook
		  (lambda ()
			(message "Loaded HTML hook")
			(local-unset-key (kbd "C-c <down>"))
			(local-unset-key (kbd "C-c <up>"))
			(local-unset-key (kbd "C-c C-i"))
			(local-unset-key (kbd "C-c <left>"))
			(local-unset-key (kbd "C-c <right>"))))


(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
			(message "Loaded lisp hook")
			(flycheck-mode)
			(electric-indent-mode -1)
			(setq tab-width 4)
			(setq mode-name "λ")
			))



;; (major-mode-hydra-define emacs-lisp-mode  (:formatter my-hydra-formatter)
;;   ("Eval"
;;    (("b" eval-buffer "buffer")
;; 	("e" eval-defun "defun")
;; 	("r" eval-region "region"))
;;    "REPL"
;;    (("I" ielm "ielm"))
;;    "Test"
;;    (("t" ert "prompt")
;; 	("T" (ert t) "all")
;; 	("F" (ert :failed) "failed"))
;;    "Doc"
;;    (("f" describe-function "function")
;; 	("v" describe-variable "variable")
;; 	("i" info-lookup-symbol "info lookup"))))


(use-package web-mode
  :defer t
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (add-to-list 'web-mode-comment-formats '("javascript" . "//" ))
  ;; Parser for svelte-check output
  (add-to-list 'compilation-error-regexp-alist   
			   '("^[0-9]* \\(ERROR\\|WARNING\\) \"\\([^\"]*\\)\" \\([0-9]*\\):\\([0-9]*\\) .*$" 2 3 4))
  :mode "\\(\\.svelte\\|.ts\\)\\'"
  )



(add-hook 'web-mode-hook #'lsp)
(setq web-mode-engines-alist
	  '(("svelte" . "\\.svelte\\'")))

(use-package terraform-mode
  :defer t
  :ensure t
  :mode "\\(\\.tf\\|tf.json\\)\\'"
  )


(use-package protobuf-mode
  :defer t
  :ensure t
  :mode "\\.proto\\'"
  :config (setq tab-width 4)
  (unbind-key "C-c C-c" protobuf-mode-map)
  :config
  (add-hook 'protobuf-mode-hook
			(lambda ()
			  (flyspell-prog-mode))))


(use-package yaml-mode
  :defer t
  :ensure t
  :mode "\\.yaml\\'")


(use-package dockerfile-mode
  :defer t
  :ensure t
  :mode "Dockerfile")


(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-port 7070)
  (setq httpd-root "/fakedir")
  (setq httpd-host "0.0.0.0"))


(use-package impatient-mode
  :defer t
  :ensure t
  :commands impatient-mode)

;;; --------------------------------------------------------------------- MARKDOWN MODE


(use-package markdown-mode
  :defer t
  :ensure t
  :init
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  :mode ("\\.\\(md\\|slide\\)\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :mode-hydra
  ((:title "Markdown" :formatter my-hydra-formatter)
   ("Preview"
	(("s" my-markdown-preview "preview server" :toggle markdown-preview-active-p)
	 ("u" show-markdown-preview-url "show preview URL")))
   )
  :config
  (unbind-key "C-c C-i" markdown-mode-map)
  (setq markdown-mode-map (make-sparse-keymap))
  (setq gfm-mode-map (make-sparse-keymap))
  (setq markdown-command "pandoc -t html5"))

(eval-after-load 'markdown-mode
  '  (setq markdown-mode-map (make-sparse-keymap)) )

(defun my-markdown-preview ()
  "Render the markdown buffer to HTML with a Github stylesheet.
The HTML is served on a local web server. Preview is rendered as
you type. Turn the server on and off if typing slows down."
  (interactive)
  (if (process-status "httpd")
	  (progn
		(message "Stopping Markdown preview server...")
		(httpd-stop)
		(impatient-mode)
		(setq markdown-preview-active-p nil))
	(progn
	  (message (concat "Starting Markdown preview server. Visit " (markdown-preview-url)))
	  (setq markdown-preview-active-p t)
	  (httpd-start)
	  (impatient-mode)
	  (imp-set-user-filter 'github-markdown-filter)
	  (when (eq system-type 'darwin)
		(imp-visit-buffer)))))


(defun github-markdown-filter (buffer)
  "Render the BUFFER in markdown with Github style sheet."
  (princ
   (with-temp-buffer
	 (let ((tmp (buffer-name)))
	   (set-buffer buffer)
	   (set-buffer (markdown tmp))
	   (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))


(defvar markdown-preview-active-p nil "Non-nil if the markdown preview server is running.
This variable is here just to inform the Hydra so it can display the status correctly.")

(defun get-ip-address (&optional dev)
  "Get the IP-address for device DEV (default: eth0) of the current machine."
  (let ((dev (if dev dev "eth0")))
	(format-network-address (car (network-interface-info dev)) t)))

(defun markdown-preview-url()
  "Derive the URL for the markdown preview for this buffer."
  (concat "http://" (get-ip-address) ":" 
		  (number-to-string httpd-port) "/imp/live/" (buffer-name) "/"))

(defun show-markdown-preview-url()
  "Display the markdown preview URL for this buffer so the user can copy it."
  (interactive)
  (message (markdown-preview-url)))

(defun markdown-preview-if-server-running()
  "If the markdown server is running, make the preview available for this buffer."
  (interactive)
  (if (process-status "httpd")
	  (progn
		(impatient-mode t)
		(imp-set-user-filter 'github-markdown-filter))
	(progn
	  (impatient-mode nil))))


;;; --------------------------------------------------------------------- WINDOWS

;; Enable desktop restore to work in terminal windows.
										; https://emacs.stackexchange.com/questions/19190/desktop-save-mode-fails-to-save-window-layout

(setq desktop-restore-forces-onscreen nil)
(add-hook 'desktop-after-read-hook
		  (lambda ()
			(frameset-restore
			 desktop-saved-frameset
			 :reuse-frames (eq desktop-restore-reuses-frames t)
			 :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
			 :force-display desktop-restore-in-current-display
			 :force-onscreen desktop-restore-forces-onscreen)))


(defun toggle-window (name)
  "Show or hide the window with NAME, such as '*Help*'."
  (interactive)
  (if (get-buffer-window name)
	  (quit-window nil (get-buffer-window name))
	(display-buffer name)))

(setq winner-dont-bind-my-keys t)
(winner-mode +1)

;; Easily navigate between windows
(use-package windmove
  :ensure t
  :bind (("C-c <up>"    . windmove-up)
		 ("C-c <down>"  . windmove-down)
		 ("C-c <left>"  . windmove-left)
		 ("C-c <right>" . windmove-right))
  :config
  (setq windmove-wrap-around t))

(global-set-key (kbd "C-c w v") 'move-window-vertical-right)
(global-set-key (kbd "C-c w r") 'rotate-windows)
(global-set-key (kbd "C-c w f") 'flip-windows)

;; Can't remember why I have this...
(defun move-window-vertical-right()
  "Close the current window, create a new vertical to the right of the current window,
and display the buffer."
  (interactive)
  (let ((this-win-file (buffer-file-name (window-buffer))))
	(delete-window)
	(select-window (split-window-horizontally))
	(find-file this-win-file)))


(defun rotate-windows ()
  "Toggle between side-by-side and above-and-below layout."
  (interactive)
  (if (= (count-windows) 2)
	  (let* ((this-win-buffer (window-buffer))
			 (next-win-buffer (window-buffer (next-window)))
			 (this-win-edges (window-edges (selected-window)))
			 (next-win-edges (window-edges (next-window)))
			 (this-win-2nd (not (and (<= (car this-win-edges)
										 (car next-win-edges))
									 (<= (cadr this-win-edges)
										 (cadr next-win-edges)))))
			 (splitter
			  (if (= (car this-win-edges)
					 (car (window-edges (next-window))))
				  'split-window-horizontally
				'split-window-vertically)))
		(delete-other-windows)
		(let ((first-win (selected-window)))
		  (funcall splitter)
		  (if this-win-2nd (other-window 1))
		  (set-window-buffer (selected-window) this-win-buffer)
		  (set-window-buffer (next-window) next-win-buffer)
		  (select-window first-win)
		  (if this-win-2nd (other-window 1))))))


(defun flip-windows ()
  "Swap places of two windows."
  (interactive)
  (cond ((not (> (count-windows)1))
		 (t
		  (setq i 1)
		  (setq numWindows (count-windows))
		  (while  (< i numWindows)
			(let* (
				   (w1 (elt (window-list) i))
				   (w2 (elt (window-list) (+ (% i numWindows) 1)))

				   (b1 (window-buffer w1))
				   (b2 (window-buffer w2))

				   (s1 (window-start w1))
				   (s2 (window-start w2))
				   )
			  (set-window-buffer w1  b2)
			  (set-window-buffer w2 b1)
			  (set-window-start w1 s2)
			  (set-window-start w2 s1)
			  (setq i (1+ i))))))))

;; Force new shells to open in current window
;; https://emacs.stackexchange.com/questions/44831/how-to-force-new-shell-to-appear-in-current-window
(add-to-list 'display-buffer-alist
			 '("^\\*shell\\*$" . (display-buffer-same-window)))


(defun enlarge-horizontally-percent(percent)
  "Widen/shrink the current window wider by PERCENT (as a decimal, e.g., .10).
If negative, shrink."
  (interactive)
  (enlarge-window-horizontally (round (* percent (window-width)))))


(defun enlarge-vertically-percent(percent)
  "Make the current window taller/shorter by PERCENT (as a decimal, e.g., .10).
If negative, make shorter."
  (interactive)
  (enlarge-window (round (* percent (window-height)))))


;; Hydra to toggle the display of specific windows.
(defhydra hydra-toggle (:color blue)
  "
Windows
   "
  ("q" nil "quit")
  ("u" winner-undo "undo" :column "Layout")
  ("r" winner-redo "redo")
  ("f" flip-windows "flip")
  ("r" rotate-windows "rotate")
  ("c" ((lambda() (interactive) (toggle-window "*compilation*"))) "compilation" :column "Compilation")
  ("m" ((lambda() (interactive) (toggle-window "*Messages*"))) "messages")
  ("h" ((lambda() (interactive) (toggle-window "*Help*"))) "help" :column "Help")

  ("b" ((lambda() (interactive) (toggle-window "*Backtrace*"))) "backtrace")
  ("w" ((lambda() (interactive) (toggle-window "*Warnings*"))) "warnings")
  ("o" ((lambda() (interactive) (toggle-window "*Occur*"))) "occur" :column "Search")
  ("l" ((lambda() (interactive) (toggle-window "*Output*"))) "output")
  ("d" ((lambda() (interactive) (toggle-window "*lsp-help*"))) "lsp-help")
  ("y" ((lambda() (interactive) (toggle-window "*YASnippet Tables*"))) "YASnippet List")
  ("d" toggle-window-dedicated "dedicated" :column "Other")
  ("_" shell-pop "terminal")
  ("e" treemacs "treemacs")
  )

(global-set-key (kbd "C-c w") 'hydra-toggle/body)


;;; --------------------------------------------------------------------- OS X

(when (eq system-type 'darwin)

  (defun pasteboard-copy()
	"Copy region to OS X system pasteboard."
	(interactive)
	(shell-command-on-region
	 (region-beginning) (region-end) "pbcopy"))


  (defun pasteboard-paste()
	"Paste from OS X system pasteboard via `pbpaste' to point."
	(interactive)
	(shell-command-on-region
	 (point) (if mark-active (mark) (point)) "pbpaste" nil t))

  (defun pasteboard-cut()
	"Cut region and put on OS X system pasteboard."
	(interactive)
	(pasteboard-copy)
	(delete-region (region-beginning) (region-end)))

  (global-set-key (kbd "C-M-y") 'pasteboard-paste)
  (global-set-key (kbd "C-M-w") 'pasteboard-copy)

  (defun region-as-rtf-to-clipboard()
	(interactive)
	(let* ((buf (get-buffer-create (concat "*" "xyz" "*"))))
	  (copy-to-buffer buf (region-beginning) (region-end))
	  (with-current-buffer buf
		(go-mode)
		(load-theme 'google t))))

  ) ;; end when mac




;;;--------------------------------------------------------------------- MODE-LINE

(column-number-mode t) ;; show the current number in the mode line

;; Diminish shortens or removes a mode's information from the mode line.
(use-package diminish
  :ensure t
  :config
  (eval-after-load "company"  '(diminish 'company-mode   " Comp"))
  (eval-after-load "yasnippet" '(diminish 'yas-minor-mode " Y"))
  (eval-after-load "eldoc" '(diminish 'eldoc-mode     " ")))




;;; --------------------------------------------------------------------- BOTTOM SHELL

;; Toggle a shell window across the bottom of the frame. 
;; Useful for quick command-line use.
(use-package shell-pop
  :ensure t
  :config
  (setq shell-pop-window-size 30
		shell-pop-full-span t
		shell-pop-window-position "bottom"
		shell-pop-autocd-to-working-dir t
		shell-pop-restore-window-configuration t)
  :bind (("C-c _"    . shell-pop)))



;;; --------------------------------------------------------------------- CHEAT SHEET

;; Hydra with commonly used but easily forgotten key bindings and operations.


(defhydra hydra-cheat-sheet (:color blue :hint nil)
  "
  ^Movement^                  ^Searching^                 ^Windows^                     ^Misc^                        ^Errors^
  ^^────────────────────────  ^^────────────────────────  ^^──────────────────────────  ^^──────────────────────────  ^^──────────────────────────
  cycle position C-u C-SPC  _o_ occur       C-c o       switch-window M-j                                       next flycheck error M-n
  pop global     C-x C-SPC  _*_ occur word  C-c *         _↑_                         _p_ helm-projectile    C-p p  prev flycheck error M-p
  _a_ beg of defun    C-M-a     helm-grep   C-c p g     _←_   _→_                         switch project     C-p s  list flycheck errs  C-c e
  _e_ end of defun    C-M-e   _w_ helm-grep-w C-c p *       _↓_                         _y_ new snippet      C-c y n  next compile error  C-x `
  _(_ backward list   C-M-p   In incremental-search:                                _b_ list snippets    C-c y l  prev compile error  C-x ~
  _)_ forward list    C-M-n    C-w yank word            ^Toggle^
    back to ind.    M-m      M-s c toggle case        ^^──────────────────────────
  _]_ pop ez mark     C-]      M-s w toggle word        _h_ help                         repeat          C-x z
                             M-s r toggle rexp        _v_ messages                     eval defun      C-M x
                             C-o occur                _c_ compilation                  eval sexp bef.  C-x C-e

  ^Editing^                   ^Killing^                   ^Marking^                     ^Look^                        ^Git^
  ^^────────────────────────  ^^────────────────────────  ^^──────────────────────────  ^^──────────────────────────  ^^──────────────────────────
  _j_ join lines    u C-j     kill whole line  M-k      _+_ expand region     C-\\     _r_ gruvbox                   _m_ magit           C-c g g
  _'_ align regexp    C-c a   zap-to-char      M-z      _d_ mark defun        C-M-h   _l_ leuven                    _\\_ diff to main    C-c C-\\
  _2_ dup. line/reg   M-\"     zap-up-to-char   M-Z      _h_ mark buffer       C-x h   _n_ nord                      _d_ vc-resolve-conf.
  _o_ open line above C-o     kill sexp        C-M-k                                _b_ borland
  _b_ open line below M-o     just one space   M-SPC                                _|_ Fill column
    yank before     C-c yb  del blank lnes   C-x C-o                              _z_ Wide margins
    yank after      C-c ya  kill line backw  M-0 C-k                              _Z_ Wide margins (all wins)
							copy to OS       C-M-w                                 
							paste from OS    C-M-y
"
  ("q" nil "quit")

  ;; Movement
  ("]" pop-ez-mark  :color red)
  (")" forward-list nil :color red)
  ("(" backward-list nil :color red)
  ("e" end-of-defun nil :color red)
  ("a" beginning-of-defun nil :color red)

  ;; Searching
  ("o" occur nil)
  ("*" occur-current-word nil)
  ("w" helm-git-grep-at-point nil)

  ;; Editing
  ("j" delete-indentation :color red)
  ("p" helm-projectile nil)
  ("'" align-regexp nil)
  ("2" duplicate-current-line-or-region nil :color red)
  ("o" open-previous-line :color red)

  ;; Marking
  ("+" er/expand-region :color red)
  ("d" mark-defun nil)
  ("h" mark-whole-buffer nil)

  ;; Misc
  ("y" yas-new-snippet nil)

  ;; Toggles
  ("v" ((lambda() (interactive) (toggle-window "*Messages*"))) )
  ("c" ((lambda() (interactive) (toggle-window "*compilation*"))) )
  ("h" ((lambda() (interactive) (toggle-window "*Help*"))) )

  ;; Look
  ("r" ((lambda() (interactive) (load-theme 'gruvbox-dark-medium t))))
  ("l" ((lambda() (interactive) (load-theme 'leuven t))))
  ("n" ((lambda() (interactive) (load-theme 'nord t))))
  ("b" ((lambda() (interactive) (load-theme 'borland-blue t))))
  ("f" ((lambda() (interactive) (load-theme 'material-light t))))
  ("|" display-fill-column-indicator-mode nil)
  ("z" wide-margins :color red)
  ("Z" ((lambda() (interactive) (wide-margins t))) :color red)

  ;; Git
  ("m" magit nil)
  ("\\" magit-diff-to-main nil)
  ("d" vc-resolve-conflicts nil)

  ;; Windows
  ("<left>" (lambda() (interactive) (enlarge-horizontally-percent .10)) :color red)
  ("<right>" (lambda() (interactive) (enlarge-horizontally-percent -.10)) :color red)
  ("<up>" (lambda() (interactive) (enlarge-vertically-percent .10)) :color red)
  ("<down>" (lambda() (interactive) (enlarge-vertically-percent -.10)) :color red)

  )
(global-set-key (kbd "C-c h") 'hydra-cheat-sheet/body)




;;; --------------------------------------------------------------------- HANDY TOOLS


(defun insert-random-uuid ()
  "Insert a random UUID.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

WARNING: this is a simple implementation.
The chance of generating the same UUID is much higher than a robust algorithm."
  (interactive)
  (insert
   (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
		   (random (expt 16 4))
		   (random (expt 16 4))
		   (random (expt 16 4))
		   (random (expt 16 4))
		   (random (expt 16 4))
		   (random (expt 16 6))
		   (random (expt 16 6)) ) ) )




;;; --------------------------------------------------------------------- EXPERIMENTAL


(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not.
Emacs will not reuse a dedicated window for output, such as compilation."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
		 (set-window-dedicated-p window
								 (not (window-dedicated-p window))))
	   "Window '%s' is dedicated"
	 "Window '%s' is normal")
   (current-buffer)))

(require 'go-template-mode)
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . go-template-mode))

;;; --------------------------------------------------------------------- SAMPLE DIR LOCAL

;; Example .dir-locals.el file.

;; ((nil . ((projectile-globally-ignored-directories . ("api/vendor"))
;;	 (projectile-globally-ignored-file-suffixes . ("validate.go"
;;							 "pb.gw.go"
;;							 "pb.d.ts"
;;							 ".png"
;;							 ".jpg"))
;; (local-services . 
;; 								 ((:name "gRPC Server"
;; 												 :tags (g grpc a)
;; 												 :command"make"
;; 												 :cwd "$HOME/code/xyz"
;; 												 :args ("start_grpc") 
;; 												 :env (("CONSOLE_FORMAT" "yes"))							
;; 									       :start-sequences ((a 1)))
;; 									(:name "UI Server"
;; 												 :tags (u ui a)
;; 												 :command "make"
;; 												 :cwd "$HOME/code/xyz"
;; 												 :args ("start_ui") 
;; 												 :env (("CONSOLE_FORMAT" "yes"))
;; 									       :start-sequences ((a 2)))
;; 								 (:name "Spanner Emulator"
;; 												:tags (d)
;; 												:command "docker"
;; 												:cwd "$HOME"
;; 												:args ("run" "-p" "9010:9010" "-p" "9020:9020" "gcr.io/cloud-spanner-emulator/emulator") 
;; 												:env (("CONSOLE_FORMAT" "yes")))))
;;
;;        (flycheck-protoc-import-path . ("/home/xyz/proto/stuff"
;;					   "/home/xyz/proto/api/"
;;					   "/home/xyz/proto/include")))))


;; These local variables turn off annoying warnings and errors in init.el. 

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; sentence-end-double-space: nil
;; End:

(use-package fzf
  :ensure t
  :bind
  ;; Don't forget to set keybinds!
  :config
  (setenv "FZF_DEFAULT_COMMAND" "rg --files --follow -g '!node_modules' -g '!gowsdl' -g '!wsdl' -g '!*.serviceV2.go' -g '!*.pb.go' -g '!vendor/' -g '!*.pb.gw.go' -g '!*.pb.validate.go'")
  (setq fzf/args "-x --print-query --margin=1,0 --no-hscroll"
		;; fzf/args "-x --print-query --margin=1,0 --no-hscroll --preview 'bat --color=always --style=header,grid --line-range :300 {}'"
        fzf/args-for-preview "--preview 'bat --color=always --style=header,grid --line-range :100 {}'"
        fzf/args-for-preview ""
		fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s ':!*.pb.validate.go' ':!**/.dockerignore' ':!**/.gcloudignore' ':!**/.moreignore' ':!**/.vscode/**'  ':!**/.gitattributes' ':!**/.gitignore'  ':!*.pb.go' ':!*.d.ts' ':!*_pb.js' ':!**/gowsdl/' ':!**/wsdl/' "
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 25))

(defhydra hydra-find (:color blue)
  "
Find Stuff
   "
  ("q" nil "quit")
  ("f" ((lambda() (interactive) (fzf-projectile t))) "files in project")
  ("b" ((lambda() (interactive) (fzf-switch-buffer))) "buffer")
  ("r" ((lambda() (interactive) (fzf-recentf t))) "recent")
  ("g" ((lambda() (interactive) (ezfzf-git-grep))) "git grep")
  ("*" ((lambda() (interactive) (ezfzf-git-grep-word))) "git grep *")
  )

(run-with-idle-timer
 2
 nil
 `(lambda() (recentf-mode 1)))

(global-set-key (kbd "C-c f") 'hydra-find/body)




(defun ezfzf-git-grep-word ()
  (interactive)
  (let ((fzf--target-validator (fzf--use-validator
                                (function fzf--pass-through)))
        (fzf--extractor-list (fzf--use-extractor
                              (list fzf--file-lnum-regexp 1 2))))
    (fzf-with-command 
	 (format (concat "git grep " fzf/git-grep-args) (shell-quote-argument "")) ; COMMAND
     #'fzf--action-find-file-with-line  ; ACTION
     (locate-dominating-file default-directory ".git") ; DIRECTORY
	 t ; AS_FILTER
	 (thing-at-point 'word 'no-properties) ; INITQ
	 nil ; FILE-PATTERN
	 )))


(defun ezfzf-git-grep ()
  (interactive)
  (let ((fzf--target-validator (fzf--use-validator
                                (function fzf--pass-through)))
        (fzf--extractor-list (fzf--use-extractor
                              (list fzf--file-lnum-regexp 1 2))))
    (fzf-with-command (format (concat "git grep " fzf/git-grep-args) (shell-quote-argument ""))
                      #'fzf--action-find-file-with-line
                      (locate-dominating-file default-directory ".git"))))


;;; init.el ends here

(custom-set-faces
 '(treemacs-root-face ((t (:height 1 :weight bold :foreground "#799B8E" )))))

;; (use-package telephone-line
;;   :custom 
;;   (telephone-line-primary-left-separator 'telephone-line-cubed-hollow-left)
;;   (telephone-line-secondary-left-separator 'telephone-line-flat)
;;   (telephone-line-primary-right-separator 'telephone-line-cubed-right)
;;   (telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
;;   (telephone-line-height 24)
;;   (telephone-line-evil-use-short-tag t)  
;;   :config
;;   (telephone-line-defsegment telephone-line-pdf-segment ()
;; 			     (if (eq major-mode 'pdf-view-mode)
;; 				 (propertize (pdf-view-page-number)
;; 					     'face '(:inherit)
;; 					     'display '(raise 0.0)
;; 					     'mouse-face '(:box 1)
;; 					     'local-map (make-mode-line-mouse-map
;; 							 'mouse-1 (lambda ()
;; 								    (interactive)
;; 								    (pdf-view-goto-page))))))
;;   (telephone-line-defsegment telephone-line-winum-segment ()
;; 			     (propertize winum--mode-line-segment
;; 					 'face '(:box (:line-width 2 :color "cyan" :style released-button))		
;; 					 'display '(raise 0.0)
;; 					 'mouse-face '(:box 1)))
;;   (setq telephone-line-lhs '((accent . (telephone-line-winum-segment
;; 					telephone-line-pdf-segment
;; 					telephone-line-vc-segment
;; 					telephone-line-erc-modified-channels-segment
;; 					telephone-line-process-segment))
;; 			     (nil . (telephone-line-projectile-segment telephone-line-buffer-segment))))
;;   (telephone-line-mode t))


;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :config
;;   (setq doom-modeline-workspace-name t
;; 		doom-modeline-lsp t
;; 		doom-modeline-time t   
;; ))

;; (use-package telephone-line
;;   :init
;;   (defface +telephone/position-face '((t (:foreground "red" :background "grey10"))) "")
;;   (defface +telephone/mode-face '((t (:foreground "white" :background "dark green"))) "")
;;   (defface +telephone/file-info-face '((t (:foreground "white" :background "Dark Blue"))) "")
;;   :custom
;;   (telephone-line-faces
;;    '((evil      . telephone-line-modal-face)
;;      (modal     . telephone-line-modal-face)
;;      (ryo       . telephone-line-ryo-modal-face)
;;      (accent    . (telephone-line-accent-active . telephone-line-accent-inactive))
;;      (nil         . (mode-line                    . mode-line-inactive))
;;      (position  . (+telephone/position-face     . mode-line-inactive))
;;      (mode      . (+telephone/mode-face         . mode-line-inactive))
;;      (file-info . (+telephone/file-info-face    . mode-line-inactive))))
;;   (telephone-line-primary-left-separator    'telephone-line-flat)
;;   (telephone-line-secondary-left-separator  'telephone-line-flat)
;;   (telephone-line-primary-right-separator   'telephone-line-flat)
;;   (telephone-line-secondary-right-separator 'telephone-line-flat)
;;   (telephone-line-height 24)
;;   (telephone-line-evil-use-short-tag nil)
;;   :config
;;   (telephone-line-defsegment +telephone/buffer-or-filename ()
;;     (cond
;;      ((buffer-file-name)
;;       (if (and (fboundp 'projectile-project-name)
;;              (fboundp 'projectile-project-p)
;;              (projectile-project-p))
;;           (list ""
;;                 (funcall (telephone-line-projectile-segment) face)
;;                 (propertize
;;                  (concat "/"
;;                          (file-relative-name (file-truename (buffer-file-name))
;;                                              (projectile-project-root)))
;;                  'help-echo (buffer-file-name)))
;;         (buffer-file-name)))
;;      (t (buffer-name))))

;;   (telephone-line-defsegment +telephone/get-position ()
;;     `(,(concat "%lL:%cC"
;;                (if (not mark-active)
;;                    ""
;;                  (format " | %dc" (- (+ 1 (region-end)) (region-beginning)))))))

;;   (setq-default
;;    telephone-line-lhs '((mode telephone-line-major-mode-segment)
;;                         (file-info telephone-line-input-info-segment)
;;                         (position +telephone/get-position)
;;                         (accent   +telephone/buffer-or-filename
;;                                   telephone-line-process-segment))
;;    telephone-line-rhs '((accent telephone-line-flycheck-segment telephone-line-misc-info-segment
;;                                 telephone-line-projectile-segment)
;;                         (file-info telephone-line-filesize-segment)
;;                         (evil  telephone-line-evil-tag-segment)))
;;   (telephone-line-mode))


(defvar ml-selected-window nil)

;; Important changes start here
(defun ml-record-selected-window ()
  (setq ml-selected-window (selected-window)))

(defun ml-update-all ()
  (force-mode-line-update t))

(add-hook 'post-command-hook 'ml-record-selected-window)
(add-hook 'buffer-list-update-hook 'ml-update-all)


(setq-default
 mode-line-format
 '((:eval
    (ez/simple-mode-line-render
     ;; Left side
     (quote (
			 (:eval (propertize " EMACS " 'face 
								(if (eq ml-selected-window (selected-window))
								  '(:background "#FE8019" :foreground "#343130" :weight bold )
									'(:background "#9D8E7A" :foreground "#343130" :weight bold ))))
			 (:eval (propertize "  " 'face 
								(if (eq ml-selected-window (selected-window))	
									'(:foreground "#FE8019" :background "#46403D" :weight bold )					
								'(:foreground "#9D8E7A" :background "#46403D"))))
			 (:eval (propertize  (ez/git-branch)
								 'face
								 '(:foreground "#E7D6AC" :background "#46403D"))) 
			 (:eval (propertize "  " 'face 
								'(:foreground "#46403D" :background "#343130")))
			 (:eval (propertize (ez/change-indicator) 'face 
									(if (buffer-modified-p)
									'(:foreground "#fabd2f" :weight bold))))
             "%b" ;; buffer name 
             ))
     ;; Right right side
     (quote (
			 " "
			 (:eval (ez/short-directory-name 20))
			 " "
			 (:eval (ez/mode-name))
			 " "
			 (:eval (ez/lsp-name))
			 " "
			 (:eval (propertize " " 'face 
								'(:foreground "#46403D" :background "#343130")))
			 (:eval (propertize "  %l:%C " 'face 
								'(:foreground "#9D8E7A" :background "#46403D")))
			 (:eval (propertize " " 'face 
								'(:foreground "#9D8E7A" :background "#46403D")))
			 
			 (:eval (propertize (concat " " (format-time-string "%l:%M %p" (current-time) "CST6CDT") "   ") 'face 
								'(:background "#9D8E7A" :foreground "#343130" :weight bold ))) 

 			 ))))))

(defun ez/git-branch()
  "Return a string with the git branch of the current buffer."
  (if (fboundp 'vc-git--symbolic-ref)
	  ;; need this if statement because vc-git--symbolic-ref will be bound
	  ;; if ANY buffer is under vc; but if the current buffer isn't it will 
	  ;; return nil in the let, causing an error
	  (if (and (buffer-file-name) (vc-git--symbolic-ref (buffer-file-name)))
		  (let ((branch (vc-git--symbolic-ref (buffer-file-name))))
			(if (stringp branch)
				(concat "  " branch "  ")
			  "Nope"))
		"   ")
	"    "))
 

(defun ez/change-indicator()
  "A graphical representation of whether the buffer is modified or not."
  (if (buffer-modified-p)
	  (concat "    ")
	(concat "    ")))

(defun ez/mode-name()
  "How to display the major mode name in the mode line."
  (concat (downcase mode-name)))

(defun ez/lsp-name()
  "The text to display when LSP is enabled and connected to an LSP server."
  (when (fboundp 'lsp-workspaces)
	(let ((servers (mapconcat #'lsp--workspace-print (lsp-workspaces) " ")))
	  (when (not (string= servers ""))
		(format "﮼ %s " servers)))))

;; (defun ez/col-line()
;;   "Text displayed for the column and line location in the mode line."


(defun ez/simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively. Used to format the modeline text."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
			   (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))


  ;; ;; TODO: something else 

  ;; (advice-add #'vc-git-mode-line-string :filter-return #'ez/replace-git-status)
  ;; (defun ez/replace-git-status (tstr)
  ;; 	"Advice to reformat the Git mode line string (TSTR)."
  ;; 	(let* ((tstr (replace-regexp-in-string "Git" "" tstr))
  ;;          (first-char (substring tstr 0 1))
  ;;          (rest-chars (substring tstr 1)))
  ;;     (concat "  " (cond
  ;; 					 ((string= ":" first-char) ;;; Modified
  ;; 					  (replace-regexp-in-string "^:" "" tstr))
  ;; 					 ((string= "-" first-char) ;; No change
  ;; 					  (replace-regexp-in-string "^-" "" tstr))
  ;; 					 (t tstr)))))

  ;; TODO something 
  (defun  ez/short-directory-name(maxlength)
	"Make a short directory name, up to MAXLENGTH characters long."
	(and (stringp (buffer-file-name))
		 (let ((name (abbreviate-file-name (directory-file-name (file-name-directory (buffer-file-name))))))
		   (ez/shorten-name name maxlength))))

  (defun ez/shorten-name(name maxlength)
	"Recursively shorten the directory NAME by removing elements of the path starting at the front, as long as there are path elements to remove and the length of the string exceeds MAXLENGTH."
	(when (and (> (length name) maxlength) (string-match "/" name))
	  (let ((arr (split-string name "/")))
		(setq name (ez/shorten-name (combine-and-quote-strings (cons ".." (cdr (cdr arr))) "/") maxlength))))
	name)


(use-package magit-todos
  :ensure t
  :config 
  (magit-todos-mode))

  (use-package hl-todo 
	;; Highlights TODO in text 
	:ensure t
	:config 
	(custom-set-faces
	 '(hl-todo ((t (:weight bold :slant italic :foreground "#000000" :background "#8AA497")))))
	(setq hl-todo-keyword-faces
		  '(("TODO" . "#000000")))
	(global-hl-todo-mode))




  ;; (defvar bootstrap-version)
  ;; (let ((bootstrap-file
  ;; 		 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
  ;; 		(bootstrap-version 6))
  ;; 	(unless (file-exists-p bootstrap-file)
  ;;     (with-current-buffer
  ;;         (url-retrieve-synchronously
  ;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
  ;;          'silent 'inhibit-cookies)
  ;; 		(goto-char (point-max))
  ;; 		(eval-print-last-sexp)))
  ;; 	(load bootstrap-file nil 'nomessage))

  ;; (straight-use-package 'use-package)

  ;; (defvar native-comp-deferred-compilation-deny-list nil)
  ;; (use-package treemacs-devicons
  ;; 	:straight (treemacs-devicons
  ;;              :type git :host github :repo "rainstormstudio/treemacs-devicons")
  ;; 	:config
  ;; 	(treemacs-load-theme "devicons"))


  (use-package treemacs-devicons
	:load-path "~/.config/emacs/local/treemacs-devicons"
	:config
	(treemacs-load-theme "devicons"))



(defun wordp (c) (= ?w (char-syntax c)))
(defun lowercasep (c) (and (wordp c) (= c (downcase c))))
(defun uppercasep (c) (and (wordp c) (= c (upcase c))))

(defun toggle-letter-case ()
  (interactive)
  (save-excursion
	(setq p1 (point))
	(right-char)
	(setq p2 (point)))
  (if (lowercasep (char-after))
	  (upcase-region p1 p2)
	(downcase-region p1 p2)))

;; TODO something 
(global-set-key (kbd "M-c") 'toggle-letter-case)


(defgroup ez-compilation-group nil
  "Group for customization"
  :prefix "ez-")

(defface ez-highlight-go-pass-face
  '((t :inherit (default)
	   :background "#b8bb26"
       :foreground "#000000"))
  "Face to highlight that Go test passed"
  :group 'ez-compilation-group)

(defface ez-highlight-go-fail-face
  '((t :inherit (default)
	   :background "firebrick"
       :foreground "#ffffff"))
  "Face to highlight that Go test failed"
  :group 'ez-compilation-group)



(add-hook 'compilation-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\(PASS\\)[^:]" 1 'ez-highlight-go-pass-face t)
                                      ("\\(FAIL\\)[^:]" 1 'ez-highlight-go-fail-face t)))
			(font-lock-fontify-buffer)))



