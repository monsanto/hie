
;;; hie.el --- minor mode Haskell-In-Emacs (ide like stuff)
     
;; Copyright (C) 2012 Christopher Monsanto.
     
;; Author: Christopher Monsanto <chris@monsan.to>
;; Maintainer: Christopher Monsanto <chris@monsan.to>
;; Created: 15 Mar 2012
;; Version: 0.1
;; Keywords: haskell intellisense autocomplete
;; License: GPLv3
;; 

(require 'cl)
(require 'haskell-mode)

;

(defvar hie-modules-dir "~/.hie/"
  "Where should we look for global definitions?")

(defvar hie-modules-cache-dir "~/.hie-cache/"
  "Where should we cache stuff?")

(defvar hie-update-interval 0.5
  "How long should you be idle before we update?")

;; (defvar hie-exclude-docs
;;   "What documentation should we *not* show? For instance, you /probably/ know the type signature of map and don't want it clouding your view..."
;;   )

;
; We keep a variety of hash tables.
; First, there is a hash table of globally available identifiers.
; Second, there is a hash table of identifiers pulled from the buffers imports.
; Third, there is a hash table of identifiers pulled from the buffer itself.
;

(defvar hie-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-h") 'hie-show-signature)
    (define-key map (kbd "C-c C-j") 'hie-jump-to-definition)
    map)
  "hie-mode keymap")

(define-minor-mode hie-mode "Toggle hie mode." nil " hie" hie-mode-map
  (hie-setup-buffer))

;
; The cache. These are generally modified by hie-incorporate.
;

(defun hie-new-hash ()
  "Create a hash table."
  (make-hash-table :test 'equal))

(defvar hie-import-defs-hash (hie-new-hash)
  "A hash table mapping hieimports to module hashes. We are not editing any of these files in Emacs. (Think system libraries).

If you DO edit this, you can clear all.")

(defvar hie-all-buffers-import-defs-hash (hie-new-hash)
  "Map module name to (exports idents hash-table).")

;; (defvar hie-all-buffers-exported-hash (hie-new-hash)
;;   "As above, but from all of the open buffers. Editing hie-buffer-module means you have to update this hash.")

;; (defvar hie-all-buffers-idents-hash (hie-new-hash)
;;   "A hash table for all of the identifiers we care about from other open buffers in Emacs.

;; Updating hie-buffer-idents-hash implies that you should update this hash.")

; all-buffers needs the two below
(defvar hie-buffer-module-name nil)
(make-variable-buffer-local 'hie-buffer-module-name)

(defvar hie-buffer-idents-hash nil
  "A hash table mapping local identifiers to hiedefs.")
(make-variable-buffer-local 'hie-buffer-idents-hash)

(defvar hie-buffer-imports-hash
  "A hash table mapping hieimports to restricted/renamed definitions.

Updating this implies that you should update hie-buffer-idents-hash, but note that the local definitions in hie-buffer-module override this hash.")
(make-variable-buffer-local 'hie-buffer-imports-hash)

;; (defvar hie-buffer-exports-hash nil
;;   "A set of exports. Updating this implies you should update hie-all-buffers-exported-modules-hash.")
;; (make-local-variable 'hie-buffer-exports-hash)



;
; Structures
;

(defstruct hieimport
  name list alias is-qualified is-hidden)

(defstruct hiedef
  name is-instance parent file line signature help)

; in the normal version, all are lists
; in the cached version, defs is a hash table from name to hiedef
;  imports is a map from hieimport to internal defs, exports is a set
(defstruct hiemod
  name defs imports exports)

;
; Main interactives
;

(defun hie-global-defshash ()
  (if hie-mode
      hie-buffer-idents-hash
    hie-buffer-idents-hash))

(defun hie-initial-text ()
  (when (gethash (haskell-ident-at-point)
		 (hie-global-defshash))
    (haskell-ident-at-point)))

(defun hie-read-ident (prompt &optional filter)
  (let ((name (completing-read prompt
				(hie-global-defshash)
				filter 
				nil
				(hie-initial-text))))
    (gethash name (hie-global-defshash))))

(defun hie-jump-to-definition ()
  "Jump to the given definition."
  (interactive)
  (let ((def (hie-read-ident "Jump to def: " (lambda (key def) (not (hiedef-is-instance def))))))
    (when def
      (find-file (hiedef-file def))
      (goto-char (point-min))
      (forward-line (1- (hiedef-line def))))))

(defun hie-show-signature ()
  "Show the signature in the mode line."
  (interactive)
  (let ((def (hie-read-ident "Signature: ")))
    (when def 
      (display-message-or-buffer (hie-make-help def)))))

(defun hie-make-help (def) 
  (if (and (hiedef-signature def) (hiedef-help def))
      (concat (hiedef-signature def) "\n\n" (hiedef-help def))
    (or (hiedef-signature def) (hiedef-help def))))

;


(defun hie-load-file (file)
  "Create completion table, etc for given module."
  (setq *hie-load* nil)
  (load file t t)
  *hie-load*)

(defun hie-dump-defs (name defs)
  
  (with-temp-file (concat hie-modules-cache-dir name)
    (insert (format "(setq *hie-load* (list %s))"
		    (mapconcat 'identity 
			       (loop for def in defs
				     collect (prin1-to-string def)) " "))))
  (byte-compile-file (concat hie-modules-cache-dir name)))

(defun hie-import-defs-nonlocal (import)
  ; Try globals
  (let ((defs (gethash import hie-import-defs-hash 'missing))
	(name (hieimport-name import)))
    (cond
     ((eq defs 'recursive-hack) nil)
     ((and (eq defs 'missing) (not (hieimport-alias import))) ; if we dont have the raw version, make it
      (let ((defs (hie-load-file (concat hie-modules-cache-dir name))))
	(if defs
	    (puthash (make-hieimport :name name) defs hie-import-defs-hash) 
	  (let ((mod (hie-load-file
		      (concat hie-modules-dir name))))
	    (if mod
		(progn
		  (puthash (make-hieimport :name name) 'recursive-hack hie-import-defs-hash)
		  (let ((defs (hie-resolve-exports-defs
			       (hiemod-exports mod)
			       (hie-resolve-imports-defs (hiemod-imports mod) (hiemod-defs mod))))) 
		    (hie-dump-defs name defs)
		    (puthash (make-hieimport :name name) defs hie-import-defs-hash)))
	      (puthash (make-hieimport :name name) nil hie-import-defs-hash))))))
     ((eq defs 'missing) ; if this isn't the raw version, import the raw version and fix it
      (let ((mod (hie-import-defs-nonlocal (make-hieimport :name name)))) 
	(puthash import (if mod (hie-fix-raw-import-defs mod import) nil) hie-import-defs-hash)))
     (t defs))))

(defun hie-import-defs-local (import)
  ; Try globals
  (let ((d (gethash (hieimport-name import) hie-all-buffers-import-defs-hash)))
    (when d
      (destructuring-bind (exports idents hash) d 
	(let ((defs (gethash import hash 'missing)))
	  (cond 
	   ((and (eq defs 'missing) (not (hieimport-alias import))) ; if we dont have the raw version, make it 
	    (puthash (make-hieimport :name (hieimport-name import))
		     (hie-resolve-exports-defs exports (loop for v being the hash-values in idents collect v))
		     hash))
	   ((eq defs 'missing) ; if this isn't the raw version, import the raw version and fix it
	    (puthash import 
		     (hie-fix-raw-import-defs
		      (hie-import-defs-local (make-hieimport :name (hieimport-name import)))
		      import)
		     hash))
	   (t defs)))))))

(defun hie-import-defs (import)
  (or (hie-import-defs-nonlocal import) (hie-import-defs-local import)))

(defun hie-fix-raw-import-defs (base-defs import)
  (let* ((defs
	   (if (hieimport-list import)
	       (if (hieimport-is-hidden import)
		   (reduce (lambda (rest exp)
			     (hie-filter-defs nil exp rest))
			   (hieimport-list import) :initial-value base-defs)
		 (loop for export in (hieimport-list import)
		       append (hie-filter-defs t export base-defs))) 
	     base-defs))
	 (alias-defs (hie-alias-defs (hieimport-alias import) defs)))
    (if (hieimport-is-qualified import)
	alias-defs
      (append defs alias-defs))))

(defun hie-resolve-exports-defs (exports defs)
  (hie-filter-prefixes (if exports
			     (loop for export in exports
				   append
				   (hie-filter-defs t export defs))
			 defs)))

(defun hie-resolve-imports-defs (imports defs)
  (append defs
	  (loop for import in imports 
		append (hie-import-defs import))))

(defun hie-has-prefix (prefix string)
  (eq 0 (string-match
	 (concat "^" (regexp-quote prefix) "\\.") string)))

(defun hie-filter-prefixes (defs)
  (loop for def in defs
	collect
	(progn
	  (setq def (copy-hiedef def)) 
	  (setf (hiedef-name def)
		(replace-regexp-in-string "[A-Za-z0-9]+\\." "" (hiedef-name def)))
	  def)))

(defun hie-filter-defs (they-select export defs)
  (destructuring-bind (tag name) export
    (cond
     ((eq tag 'id)
      (loop for def in defs
	    if (eq they-select (equal (hiedef-name def) name))
	    collect def))
     ((eq tag 'all)
      (loop for def in defs
	    if (eq they-select (or (equal (hiedef-name def) name)
				   (equal (hiedef-parent def) name)))
	    collect def)

      )
     ((eq tag 'mod)
      (loop for def in defs
	    if (eq they-select (hie-has-prefix
				name
				(hiedef-name def)))
	    collect def)))))

(defun hie-alias-defs (new-scope defs)
  "Strips out anything not in the namespace old-mod, and renames the rest to new-mod."
  (loop for def in defs
	collect (progn
		  (setq def (copy-hiedef def)) 
		  (setf (hiedef-name def)
			(concat new-scope "." (hiedef-name def)))
		  def)))


;; (defun hie-global-import-module (name)
;;   (let ((mod (hie-load-module name)))
;;     (hie-alias-defs name (hie-external-defs mod))))
  

; I'll make this "multithreaded" once I get lexically scoped variables... not everyone uses Emacs 24 I guess. :(
(defun hie-run ()
  "Runs hie and loads the module it generates."
  (let ((file1 (make-temp-file "hie"))
	(file2 (make-temp-file "hie"))
	(text (buffer-string))
	(name (buffer-name)))
    (with-temp-file file1
      (insert text)) 
    (call-process-shell-command (format "hie %s < %s > %s" name file1 file2))
    (hie-load-file file2)))

;; (defun hie-run ()
;;   "Runs hie and loads the module it generates."
;;   (let ((proc (start-process "hie" nil "hie" (buffer-name))))
;;     (process-send-string proc (buffer-string))
;;     (set-process-filter proc 'hie-proc-filter)
;;     (set-process-sentinel proc 'hie-proc-sentinel)
    
 
;;     (hie-load-file file2)))

(defun hie-try-idle-update ()
  "Try to update the current buffer when we're idle."
  (when (and hie-mode (buffer-modified-p)) 
    (hie-setup-buffer)))

(defun hie-try-save-update ()
  "Try to update the current buffer when we're idle."
  (when hie-mode 
    (hie-setup-buffer)))

;

(defun hie-populate-defshash (defs defshash)
  (loop for def in defs do (puthash (hiedef-name def) def defshash)))

(defun hie-setup-buffer ()
  "Prepare the buffer for hie."
  (interactive)
  (let ((mod (hie-run)))
    ; local business
    (setq hie-buffer-idents-hash (hie-new-hash))
    (loop for import in (hiemod-imports mod)
	  do (hie-populate-defshash (hie-import-defs import) hie-buffer-idents-hash))
    (hie-populate-defshash (hiemod-defs mod) hie-buffer-idents-hash)
    ; global business
    (when hie-buffer-module-name
      (remhash hie-buffer-module-name hie-all-buffers-import-defs-hash))
    (setq hie-buffer-module-name (hiemod-name mod))    
    (when hie-buffer-module-name
      (puthash hie-buffer-module-name
	       (list (hiemod-exports mod) hie-buffer-idents-hash (hie-new-hash))
	       hie-all-buffers-import-defs-hash))))

;; Autocomplete stuff

(defun hie-ac-candidates ()
  (let (candidates)
    (maphash
     (lambda (name def)
       (unless (hiedef-is-instance def)
	 (setq candidates (cons name candidates))))
     hie-buffer-idents-hash)
    candidates))
					
(defun hie-ac-document (name)
  (let ((def (gethash name hie-buffer-idents-hash)))
    (hie-make-help def)))

(defvar ac-source-hie
  '((candidates . hie-ac-candidates)
    (document . hie-ac-document)))

(defun hie-ac-module-candidates ()
  (directory-files hie-modules-dir nil "^[^.]" t))

(defvar ac-source-hie-modules
  '((candidates . hie-ac-module-candidates)
    (cache)))

;

(defvar hie-idle-timer nil)

(when hie-idle-timer
  (cancel-timer hie-idle-timer))

(setq hie-idle-timer (run-with-idle-timer hie-update-interval t 'hie-try-idle-update))

(add-hook 'after-save-hook 'hie-try-save-update)

(provide 'hie)
