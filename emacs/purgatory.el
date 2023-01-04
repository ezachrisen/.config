;;; purgatory.el --- deleted init.el code

;;; Commentary:

;; Contains former init.el code that may be removed completely, 
;; put back into init.el

;;; Code:




;; ;; We use two backup strategies: first, the standard Emacs backup strategy.
;; ;; A file is backed up when it is first saved in a session. Subsequent saves are not saved. 
;; (defvar --backup-directory "~/.backups-per-session")
;; (setq backup-directory-alist `(("." . ,--backup-directory)))
;; (setq make-backup-files t               ; backup of a file the first time it is saved.
;;       backup-by-copying t               ; don't clobber symlinks
;;       version-control t                 ; version numbers for backup files
;;       delete-old-versions t             ; delete excess backup files silently
;;       delete-by-moving-to-trash t
;;       kept-old-versions 2               ; oldest versions to keep when a new numbered backup is made (default: 2)
;;       kept-new-versions 10              ; newest versions to keep when a new numbered backup is made (default: 2)
;; 	  vc-make-backup-files t
;;       )


;; (setq display-line-numbers-type 'relative)

;; (setq warning-minimum-level :emergency)

;; (defun custom-theme-colors(theme &rest args)
;;   "Modifications to THEME, unused ARGS."
;;   (message "in custom theme colors")
;;   (cond ((string= theme "gruvbox-dark-soft")
;;  		 (custom-theme-set-faces
;;  		  'gruvbox-dark-soft
;; 		  `(underline ((t (:underline nil))))
;;  		  `(line-number ((t (:background nil :foreground "#7c6f64"))))
;; 		  `(isearch ((t (:background "#fabd2f" :foreground "#000000"))))
;; 		  `(isearch-lazy-highlight ((t (:background "#66999D" :foreground "#ffffff"))))))
;; 		((string= theme "gruvbox-dark-medium")
;;  		 'gruvbox-dark-medium
;;  		 (custom-theme-set-faces
;; 		  'gruvbox-dark-medium
;; 		  `(underline ((t (:underline nil))))
;;  		  `(line-number ((t (:background nil :foreground "#7c6f64"))))
;; 		  `(isearch ((t (:background "#fabd2f" :foreground "#000000"))))
;; 		  `(lazy-highlight ((t (:background "#66999D" :foreground "#ffffff"))))))
;; 		((string= theme "leuven")
;;  		 'leuven
;;  		 (custom-theme-set-faces
;; 		  'leuven
;; 		  `(underline ((t (:underline nil))))
;; 		  `(vertical-border ((t (:background "#ffffff" :foreground "gray80"))))
;; 		  `(isearch ((t (:background "#fabd2f" :foreground "#000000"))))
;; 		  `(lazy-highlight ((t (:background "#66999D" :foreground "#ffffff"))))))
;; 		((string= theme "nord")
;; 		 (custom-theme-set-faces
;; 		  'nord
;; 		  `(underline ((t (:underline nil))))
;; 		  `(isearch ((t (:background "#fabd2f" :foreground "#000000"))))
;; 		  `(lazy-highlight ((t (:background "#66999D" :foreground "#ffffff"))))
;;  		  `(line-number ((t (:background nil :foreground "#4C566A"))))))))




;;(global-set-key (kbd "C-M-]") 'pop-global-mark)		; Cycle through previous global positions
;(global-set-key (kbd "C-]")   'cycle-position)		; Cycle through previous position in buffer
;; (defun cycle-position()
;;   "Move cursor to last mark position of current buffer.
;; Call this repeatedly will cycle all positions in `mark-ring'.
;; URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
;; version 2016-04-04"
;;   (interactive)
;;   (set-mark-command t))

;;(setq xterm-mouse-utf-8 1)

;;(setq dabbrev-case-replace t)     ; Preserve original case when expanding





;; (if (not (getenv "MAUI_HOME"))
;;   (message "MAUI_HOME is not set; flycheck protoc import paths may be incorrect"))

;; (use-package flycheck
;;   :ensure t
;;   :config (setq flycheck-standard-error-navigation nil)
;; 				flycheck-protoc-import-path (list (concat (getenv "MAUI_HOME") "/proto/api/alticeusa/maui") 
;; 												  (concat (getenv "MAUI_HOME") "/proto/api/") 
;; 												  (concat (getenv "MAUI_HOME") "/proto/include")))
;;   :init (global-flycheck-mode)
;;   :bind (("M-n" . flycheck-next-error)
;;  		 ("M-p" . flycheck-previous-error)
;; 		 ("C-c e" . flycheck-list-errors)))


;;; --------------------------------------------------------------------- TREE SITTER

;; (global-tree-sitter-mode)

;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)


;; (use-package helm-projectile
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq  projectile-indexing-method 'hybrid
;; 		 projectile-globally-ignored-directories '("api/vendor")
;; 		 projectile-globally-ignored-file-suffixes '("validate.go" 
;; 													 "generated.svelte"
;; 													 "pb.gw.go" 
;; 													 "pb.d.ts" 
;; 													 "pb.js"
;; 													 "pb.d.ts"
;; 													 "rwInterface.go"
;; 													 ".png" 
;; 													 ".jpg"))
;;   :bind (("C-c p s" . 'helm-projectile-switch-project)
;; 		 ("C-c m" . 'projectile-compile-project)
;; 		 ("C-c C-f" . 'helm-projectile)
;; 		 ("C-c f" . 'helm-projectile)))


;; Compile command that will run from any buffer
;; (global-set-key (kbd "C-c g") 'global-compile-command-set)
;; (global-set-key (kbd "C-c c") 'global-compile-command-run)

;; 	     ;; (subword-mode)   ; word-based commands top at CaseChanges
;; 		  ;;(flyspell-prog-mode)


  ;; (defun copy-from-osx ()
;;   (shell-command-to-string "pbpaste"))

;; (defun paste-to-osx (text &optional push)
;;   (let ((process-connection-type nil))
;;     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))

;; (setq interprogram-cut-function 'paste-to-osx)
;; (setq interprogram-paste-function 'copy-from-osx)



;; (let ((local-settings "~/.emacs.d/local.el"))
;;   (when (file-exists-p local-settings)
;;	(load-file local-settings)))


;; (setq-default
;;  mode-line-format
;;  '((:eval
;;     (ez/simple-mode-line-render
;;      ;; Left side
;;     (quote (
;;			(:eval (ez/change-indicator))
;;              "%12b" ; mode-line-buffer-identification
;;              ))
;;      ;; Right right side
;;      (quote (
;;			 (:eval (ez/short-directory-name 20))
;;			 " "
;;			 (:eval (ez/mode-name))
;;			 " "
;;			 (:eval (ez/lsp-name))
;;			 " %I "
;;			 "  %l:%C "
;;			 (vc-mode vc-mode)
;;			 " "
;;			 ))))))

;; ; To make a vertical bar: ⎜

;; (defun ez/change-indicator()
;;   "A graphical representation of whether the buffer is modified or not."
;;   (if (buffer-modified-p)
;;		(concat "  ")
;;		(concat "  ")))

;; (defun ez/mode-name()
;;   "How to display the major mode name in the mode line."
;;   (concat " " (downcase mode-name)))

;; (defun ez/lsp-name()
;;   "The text to display when LSP is enabled and connected to an LSP server."
;;   (when (fboundp 'lsp-workspaces)
;;	(let ((servers (mapconcat #'lsp--workspace-print (lsp-workspaces) " ")))
;;	  (when (not (string= servers ""))
;;		(format "﮼ %s " servers)))))

;; ;; (defun ez/col-line()
;; ;;   "Text displayed for the column and line location in the mode line."


;; (defun ez/simple-mode-line-render (left right)
;;   "Return a string of `window-width' length.
;; Containing LEFT, and RIGHT aligned respectively. Used to format the modeline text."
;;   (let ((available-width
;;          (- (window-total-width)
;;             (+ (length (format-mode-line left))
;;                (length (format-mode-line right))))))
;;     (append left
;;             (list (format (format "%%%ds" available-width) ""))
;;             right)))




;; (advice-add #'vc-git-mode-line-string :filter-return #'ez/replace-git-status)
;; (defun ez/replace-git-status (tstr)
;;   "Advice to reformat the Git mode line string (TSTR)."
;;   (let* ((tstr (replace-regexp-in-string "Git" "" tstr))
;;          (first-char (substring tstr 0 1))
;;          (rest-chars (substring tstr 1)))
;;     (concat "  " (cond
;;      ((string= ":" first-char) ;;; Modified
;;       (replace-regexp-in-string "^:" "" tstr))
;;      ((string= "-" first-char) ;; No change
;;       (replace-regexp-in-string "^-" "" tstr))
;;      (t tstr)))))


;; (defun  ez/short-directory-name(maxlength)
;;   "Make a short directory name, up to MAXLENGTH characters long."
;;   (and (stringp (buffer-file-name))
;;	   (let ((name (abbreviate-file-name (directory-file-name (file-name-directory (buffer-file-name))))))
;;		 (ez/shorten-name name maxlength))))

;; (defun ez/shorten-name(name maxlength)
;;   "Recursively shorten the directory NAME by removing elements of the path starting at the front, as long as there are path elements to remove and the length of the string exceeds MAXLENGTH."
;;   (when (and (> (length name) maxlength) (string-match "/" name))
;;	(let ((arr (split-string name "/")))
;;	  (setq name (ez/shorten-name (combine-and-quote-strings (cons ".." (cdr (cdr arr))) "/") maxlength))))
;;   name)

;; (defun xeshell-pop-bottom()
;;   "pop eshell at bottom"
;;   (interactive)
;;   (let ((pos-buffer (current-buffer))
;; 	(tmp-eshell (get-buffer my-eshell))
;; 	(dir (get-current-directory)))
;; 	;;check if my-eshell exist,if not create one.
;; 	(unless tmp-eshell
;; 	  (setq tmp-eshell (eshell 100))
;; 	  (eshell-send-input)
;; 	  (with-current-buffer tmp-eshell
;; 	(eshell/clear-scrollback)
;; 	(rename-buffer my-eshell)
;; 	(switch-to-buffer pos-buffer)))
;; 	(setq window
;; 	  (select-window
;; 	   (display-buffer-in-side-window tmp-eshell '((side . bottom))) t))
;; 	(set-window-dedicated-p window t)
;; 	(when (not (equal pre-path dir))
;; 	  (eshell/cd dir)
;; 	  (eshell-send-input)
;; 	  (setq pre-path dir))))


;;; --------------------------------------------------------------------- KEYBINDING CHEAT SHEET
;;; ----- Documentation
;;; C-c dd    lsp-describe-thing-at-point

;;; ----- Toggling
;;; C-c t t    toggle Treemacs
;;; C-c t d    toggle lsp docs
;;; C-c t c    toggle compile window
;;; C-c t h    toggle help window
;;; C-c t h    toggle occur window


;;; ----- Finding
;;; C-c o      occur

;;; ----- Movement
;;; C-]        cycle positions in buffer
;;; C-M-]      cycle positions in all buffers
;;; M-g        goto line
;;; C-j        ace jump character
;;; M-j        ace jump window

;;; ----- Editing
;;; C-o        open previous line
;;; M-o        open next line
;;; M-k        kill whole line
;;; M-z        zap-to-char
;;; M-Z        zap-up-to-char
;;; C-\        expand region

;;; ----- Project
;;; C-c p p    Helm projectile
;;; C-c p s    switch project
;;; C-c p g    git-grep in project

;;; ----- Misc
;;; C-i        LSP imenu
;;; C-c m      Helm mini
;;; C-c f      select treemacs window
;;; C-c g      set global compile command
;;; C-c c      run global compile command
;;; C-c l      toggle line numbers
;;; C-x r w    register window layout
;;; C-x r j    jump to register

;; (provide 'init)

;; (defun restart-app()
;; (interactive)
;; (with-current-buffer "grpc-server"
;; (eshell-kill-process)
;; (run-with-timer 0.5 nil
;; 				(lambda ()
;; 				  (with-current-buffer "grpc-server"
;; 					(setq eshell-scroll-to-bottom-on-input t)
;; 					(setq eshell-scroll-to-bottom-on-output t)
;; 					(setq eshell-scroll-show-maximum-output t)
;; 					;; (setq eshell-move-point-for-output t)
;; 					;; (setq es-move-point-for-matching-input t)
;; 					(goto-char (point-max))
;; 					(insert "./run.sh")
;; 					(eshell-send-input)
;; 					(goto-char (point-max)))))))


;; (use-package dired-sidebar
;;   :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
;;   :ensure t
;;   :commands (dired-sidebar-toggle-sidebar)
;;   :init
;;   (add-hook 'dired-sidebar-mode-hook
;;             (lambda ()
;;			  (dired-omit-mode)
;;			  (display-line-numbers-mode -1)
;;               (unless (file-remote-p default-directory)
;;                 (auto-revert-mode))))

;;   :config
;;   (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
;;   (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
;;  ;; (setq dired-sidebar-subtree-line-prefix "  ")
;;  (setq dired-sidebar-subtree-line-prefix "  ")
;;   (setq dired-omit-files
;;		(concat dired-omit-files "\\|^.git$\\|^.gitignore$"))
;;   //(setq dired-sidebar-theme 'icons)
;;   (setq dired-sidebar-theme 'ascii)
;;   (setq dired-sidebar-use-term-integration t))



;; (use-package dired-narrow
;;   :ensure t)


;; (use-package dired-x
;;   :ensure t)


;; (use-package logview
;;   :ensure t)

;; (use-package git-gutter
;;   :ensure t
;;   :config
;;   (global-git-gutter-mode t)
;;   (setq git-gutter:update-interval 0.02))



;; ;; http://blog.vivekhaldar.com/post/4809065853/dotemacs-extract-interactively-change-font-size
;; (defun my/zoom-in ()
;;   "Increase font size by 10 points"
;;   (interactive)
;;   (set-face-attribute 'default nil
;; 					  :height
;; 					  (+ (face-attribute 'default :height)
;; 						 10)))

;; (defun my/zoom-out ()
;;   "Decrease font size by 10 points"
;;   (interactive)
;;   (set-face-attribute 'default nil
;; 					  :height
;; 					  (- (face-attribute 'default :height)
;; 						 10)))

;; ;; change font size, interactively
;; (global-set-key (kbd "s-=") 'my/zoom-in)
;; (global-set-key (kbd "s--") 'my/zoom-out)

;; (define-key ctl-x-map [(control ?+)] 'zoom-in/out)
;; (define-key ctl-x-map [(control ?-)] 'zoom-in/out)
;; (define-key ctl-x-map [(control ?=)] 'zoom-in/out)
;; (define-key ctl-x-map [(control ?0)] 'zoom-in/out)


;; (prodigy-define-tag
;;   :name 'node
;;   :on-output (lambda (&rest args)
;;                (let ((output (plist-get args :output))
;;                      (service (plist-get args :service)))
;;                  (when (s-matches? "➜  Local:   http:" output)
;;                    (prodigy-set-status service 'ready)))))
					   

;; (prodigy-define-tag
;;   :name 'grpc
;;   :on-output (lambda (&rest args)
;;                (let ((output (plist-get args :output))
;;                      (service (plist-get args :service)))
;;                  (when (s-matches? "Serving GRPC on port" output)
;;                    (prodigy-set-status service 'ready)))))


;; (defvar go-lint-cmd
;;   "golangci-lint run --print-issued-lines=false --issues-exit-code 0 ./... "
;;   "Command to run golangci-lint on the module.")

;; (defun go-lint()
;;   "Run golangci-lint on the Go module."
;;   (interactive)
;;   (compile-wrap (concat "cd " (go-module-dir) " && " go-lint-cmd)))



;; (defun go-all-linters-cmd()
;;   "Combined command to run all the Go linters."
;;   (concat "echo 'golangci-lint' && " go-lint-cmd " &&  echo 'staticcheck' && " go-staticcheck-cmd " && echo 'go vet' && go vet ./..."))


;; (defun go-all-linters()
;;   "Run all linters on the Go module."
;;   (interactive)
;;   (compile-wrap (concat "cd "
;; 			(go-module-dir)
;; 			" && "
;; 			(go-all-linters-cmd))))



;; (defun check-expansion ()
;;   (save-excursion
;; 	(if (looking-at "\\_>") t
;; 	  (backward-char 1)
;; 	  (if (looking-at "\\.") t
;; 	(backward-char 1)
;; 	(if (looking-at "->") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;; 	(yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (cond
;;    ((minibufferp)
;; 	(minibuffer-complete))
;;    (t
;; 	(indent-for-tab-command)
;; 	(if (or (not yas/minor-mode)
;; 		(null (do-yas-expand)))
;; 	(if (check-expansion)
;; 		(progn
;; 		  (company-manual-begin)
;; 		  (if (null company-candidates)
;; 		  (progn
;; 			(company-abort)
;; 			(indent-for-tab-command)))))))))

;; (defun tab-complete-or-next-field ()
;;   (interactive)
;;   (if (or (not yas/minor-mode)
;; 	  (null (do-yas-expand)))
;; 	  (if company-candidates
;; 	  (company-complete-selection)
;; 	(if (check-expansion)
;; 	  (progn
;; 		(company-manual-begin)
;; 		(if (null company-candidates)
;; 		(progn
;; 		  (company-abort)
;; 		  (yas-next-field))))
;; 	  (yas-next-field)))))

;; (defun expand-snippet-or-complete-selection ()
;;   (interactive)
;;   (if (or (not yas/minor-mode)
;; 	  (null (do-yas-expand))
;; 	  (company-abort))
;; 	  (company-complete-selection)))

;; (defun abort-company-or-yas ()
;;   (interactive)
;;   (if (null company-candidates)
;; 	  (yas-abort-snippet)
;; 	(company-abort)))

;; (global-set-key [tab] 'tab-indent-or-complete)
;; (global-set-key (kbd "TAB") 'tab-indent-or-complete)
;; (global-set-key [(control return)] 'company-complete-common)

;; (define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
;; (define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)

;; (define-key yas-minor-mode-map [tab] nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)

;; (define-key yas-keymap [tab] 'tab-complete-or-next-field)
;; (define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
;; (define-key yas-keymap (kbd "C-M-i") 'yas-next-field)
;; (define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)








;;; purgatory.el ends here 
