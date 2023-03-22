;; Copyright of the below functions belong to the author/contributors of https://github.com/dominikh/go-mode.el.


;; Usage: Plug go-mode's gofmt to the new go-ts-mode (built-in go mode which uses tree-sitter) in emacs. 
;; go-ts-mode landed on emacs `master` branch on 2022-12-16.

(defcustom gofmt-command "gofmt"
  "The 'gofmt' command.
Some users may replace this with 'goimports'
from https://golang.org/x/tools/cmd/goimports."
  :type 'string
  :group 'go)

(defcustom gofmt-args nil
  "Additional arguments to pass to gofmt."
  :type '(repeat string)
  :group 'go)

(defcustom gofmt-show-errors 'buffer
  "Where to display gofmt error output.
It can either be displayed in its own buffer, in the echo area, or not at all.
Please note that Emacs outputs to the echo area when writing
files and will overwrite gofmt's echo output if used from inside
a `before-save-hook'."
  :type '(choice
          (const :tag "Own buffer" buffer)
          (const :tag "Echo area" echo)
          (const :tag "None" nil))
  :group 'go)

(defun gofmt--is-goimports-p ()
  (string-equal (file-name-base gofmt-command) "goimports"))

(defun go--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun go--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun go--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0)
        (column (current-column)))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in go--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (go--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (go--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in go--apply-rcs-patch")))))))
    (move-to-column column)))

(defun gofmt ()
  "Format the current buffer according to the formatting tool.
The tool used can be set via ‘gofmt-command’ (default: gofmt) and additional
arguments can be set as a list via ‘gofmt-args’."
  (interactive)
  (let ((tmpfile (make-nearby-temp-file "gofmt" nil ".go"))
        (patchbuf (get-buffer-create "*Gofmt patch*"))
        (errbuf (if gofmt-show-errors (get-buffer-create "*Gofmt Errors*")))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        our-gofmt-args)

    (unwind-protect
        (save-restriction
          (widen)
          (if errbuf
              (with-current-buffer errbuf
                (setq buffer-read-only nil)
                (erase-buffer)))
          (with-current-buffer patchbuf
            (erase-buffer))

          (write-region nil nil tmpfile)

          (when (and (gofmt--is-goimports-p) buffer-file-name)
            (setq our-gofmt-args
                  (append our-gofmt-args
                          ;; srcdir, despite its name, supports
                          ;; accepting a full path, and some features
                          ;; of goimports rely on knowing the full
                          ;; name.
                          (list "-srcdir" (file-local-name
                                           (file-truename buffer-file-name))))))
          (setq our-gofmt-args
                (append our-gofmt-args gofmt-args
                        (list "-w" (file-local-name tmpfile))))
          (message "Calling gofmt: %s %s" gofmt-command our-gofmt-args)
          ;; We're using errbuf for the mixed stdout and stderr output. This
          ;; is not an issue because gofmt -w does not produce any stdout
          ;; output in case of success.
          (if (zerop (apply #'process-file gofmt-command nil errbuf nil our-gofmt-args))
              (progn
                ;; There is no remote variant of ‘call-process-region’, but we
                ;; can invoke diff locally, and the results should be the same.
                (if (zerop (let ((local-copy (file-local-copy tmpfile)))
                             (unwind-protect
                                 (call-process-region
                                  (point-min) (point-max) "diff" nil patchbuf
                                  nil "-n" "-" (or local-copy tmpfile))
                               (when local-copy (delete-file local-copy)))))
                    (message "Buffer is already gofmted")
                  (go--apply-rcs-patch patchbuf)
                  (message "Applied gofmt"))
                (if errbuf (gofmt--kill-error-buffer errbuf)))
            (message "Could not apply gofmt")
            (if errbuf (gofmt--process-errors (buffer-file-name) tmpfile errbuf))))

      (kill-buffer patchbuf)
      (delete-file tmpfile))))


(defun gofmt--process-errors (filename tmpfile errbuf)
  (with-current-buffer errbuf
    (if (eq gofmt-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (gofmt--kill-error-buffer errbuf))
      ;; Convert the gofmt stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (if (save-excursion
            (save-match-data
              (search-forward "flag provided but not defined: -srcdir" nil t)))
          (insert "Your version of goimports is too old and doesn't support vendoring. Please update goimports!\n\n"))
      (insert "gofmt errors:\n")
      (let ((truefile
             (if (gofmt--is-goimports-p)
                 (concat (file-name-directory filename) (file-name-nondirectory tmpfile))
               tmpfile)))
        (while (search-forward-regexp
                (concat "^\\(" (regexp-quote (file-local-name truefile))
                        "\\):")
                nil t)
          (replace-match (file-name-nondirectory filename) t t nil 1)))
      (compilation-mode)
      (display-buffer errbuf))))

(defun gofmt--kill-error-buffer (errbuf)
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (kill-buffer errbuf))))

;;;###autoload
(defun gofmt-before-save ()
  "Add this to .emacs to run gofmt on the current buffer when saving:
\(add-hook 'before-save-hook 'gofmt-before-save).
Note that this will cause ‘go-mode’ to get loaded the first time
you save any file, kind of defeating the point of autoloading."

  (interactive)
  (message "Running gofmt before save")
  (when (eq major-mode 'go-ts-mode) (gofmt))) ;NOTE: Use tree-sitter implementation of go

(add-hook 'before-save-hook #'gofmt-before-save)

(provide 'gofmt)
