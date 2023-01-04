;;; ezmark.el --- Keep a global mark ring 

;; Author: Espen Zachrisen <ezachrisen@alum.mit.edu>
;; Version: 1.0
;; Package-Requires: ((flange "1.0"))
;; Keywords: navigation, movement

;;; Commentary:

;; This package provides functions to keep and navigate a global
;; mark ring. Marks are placed on the ring by calling push-ez-mark 
;; manually, or by advising the Emacs-standard set-mark command. 


(defvar ez-mark-ring '())
(defvar ez-mark-ring-max 16)

(defun pop-ez-mark ()
  "Pop off ez mark ring and jump to the top location."
  (interactive)
  ;; Pop entries that refer to non-existent buffers.
  (while (and ez-mark-ring (not (marker-buffer (car ez-mark-ring))))
    (setq ez-mark-ring (cdr ez-mark-ring)))
  (or ez-mark-ring
      (error "No ez mark set"))
  (let* ((marker (car ez-mark-ring))
	 (buffer (marker-buffer marker))
	 (position (marker-position marker)))
    (setq ez-mark-ring (nconc (cdr ez-mark-ring)
				  (list (car ez-mark-ring))))
    (set-buffer buffer)
    (or (and (>= position (point-min))
	     (<= position (point-max)))
	(if widen-automatically
	    (widen)
	  (error "Ez mark position is outside accessible part of buffer %s"
                 (buffer-name buffer))))
    (goto-char position)
    (switch-to-buffer buffer)))

(defun clear-mark-ring()
  (interactive)
  (setq ez-mark-ring '()))


(defun x () 
  (interactive)
  (let* ((marker (car ez-mark-ring))
		 (position (marker-position marker)))
	(message "Marker is %s -- %s" marker position)))


(defun push-ez-mark(&optional r)
  "Push the current mark onto the ez mark ring"
  (interactive)
  (unless (window-minibuffer-p) 
	(let ((old (nth ez-mark-ring-max ez-mark-ring))
          (history-delete-duplicates t))
	  (set-marker (mark-marker) (point) (current-buffer))
      (add-to-history
       'ez-mark-ring (copy-marker (mark-marker)) ez-mark-ring-max t)
	  (message "pushed to ez mark ring")
      (when old
		(set-marker old nil)))))


(defun show-ez-mark-ring()
  "Display the ez mark ring in a buffer"
  (interactive)
  (setq output (get-buffer-create "*ez mark ring*"))
  (display-buffer output)
  (with-current-buffer "*ez mark ring*"
	(erase-buffer)
	(dolist (e ez-mark-ring) 
	  (insert (concat 
				(buffer-name (marker-buffer e))
				" @ "
				(format "%d" (marker-position e))))
	  (newline))))


;q;(advice-add #'push-mark :before #'push-ez-mark)

;; (defun push-ez-mark()
;;   (interactive)
;;   (let ((old (nth global-mark-ring-max global-mark-ring))
;;         (history-delete-duplicates t))
;; 	(set-marker (mark-marker) (point) (current-buffer))
;;     (add-to-history
;;      'global-mark-ring (copy-marker (mark-marker)) global-mark-ring-max t)
;; 	(message "pushed to global mark ring")
;;     (when old
;;       (set-marker old nil))
;; 	  (show-global-mark-ring))))


;; (setq global-mark-ring-max 200)

;; (defun show-global-mark-ring()
;;   (interactive)
;;   (setq output (get-buffer-create "*global mark ring*"))
;;   (display-buffer output)
;;   (with-current-buffer "*global mark ring*"
;; 	(erase-buffer)
;; 	(dolist (e global-mark-ring) 
;; 	  (insert (concat 
;; 				(buffer-name (marker-buffer e))
;; 				" @ "
;; 				(format "%d" (marker-position e))))
;; 	  (newline))))


;; 	  (message (type-of e))))
;; 	  (newline)
;; 	  (insert e))))


