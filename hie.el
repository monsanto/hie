
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
  "Where should we look for caches?")

(defvar hie-update-interval 0.5
  "How long should you be idle before we update?")

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

(defvar hie-global-external-defs-hash (hie-new-hash)
  "A hash table mapping module names to module hashes. We are not editing any of these files in Emacs. (Think system libraries).

If you DO edit this, you can clear all.")

;; (defvar hie-all-buffers-exported-hash (hie-new-hash)
;;   "As above, but from all of the open buffers. Editing hie-buffer-module means you have to update this hash.")

;; (defvar hie-all-buffers-idents-hash (hie-new-hash)
;;   "A hash table for all of the identifiers we care about from other open buffers in Emacs.

;; Updating hie-buffer-idents-hash implies that you should update this hash.")

(defvar hie-buffer-idents-hash nil
  "A hash table mapping local identifiers to hiedefs.")
(make-local-variable 'hie-buffer-idents-hash)

(defvar hie-buffer-imports-hash
  "A hash table mapping hieimports to restricted/renamed definitions.

Updating this implies that you should update hie-buffer-idents-hash, but note that the local definitions in hie-buffer-module override this hash.")
(make-local-variable 'hie-buffer-imports-hash)

;; (defvar hie-buffer-exports-hash nil
;;   "A set of exports. Updating this implies you should update hie-all-buffers-exported-modules-hash.")
;; (make-local-variable 'hie-buffer-exports-hash)

(defvar hie-buffer-defined-hash nil
  "A hash table mapping defined identifiers to hiedefs. If you update this, you should update hie-buffer-idents-hash.")
(make-local-variable 'hie-buffer-defined-hash)

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

(defun hie-the-hash ()
  (if hie-mode
      hie-buffer-idents-hash
    hie-buffer-idents-hash))

(defun hie-jump-to-definition (&optional name)
  "Jump to the given definition."
  (interactive)
  (let* ((ident (haskell-ident-at-point))
	 (name (completing-read "Jump to definition: "
				(hie-the-hash)
				nil
				nil
				(or name (if (gethash ident (hie-the-hash)) ident nil))))
	 (def (gethash name (hie-the-hash))))
    (find-file (hiedef-file def))
    (goto-char (point-min))
    (forward-line (1- (hiedef-line def)))))

(defun hie-show-signature (&optional name)
  "Show the signature in the mode line."
  (interactive)
  (let* ((ident (haskell-ident-at-point))
	 (name (completing-read "Show signature: "
				(hie-the-hash)
				(lambda (key def) (not (hiedef-is-instance def))) 
				nil 
				(or name (if (gethash ident (hie-the-hash)) ident nil))))
	 (def (gethash name (hie-the-hash))))
    (display-message-or-buffer (hie-make-help def))))


(defun hie-make-help (def) 
  (if (and (hiedef-signature def) (hiedef-help def))
      (concat (hiedef-signature def) "\n\n" (hiedef-help def))
    (or (hiedef-signature def) (hiedef-help def))))

;


(defun hie-load-file (file)
  "Create completion table, etc for given module."
  (setq *hie-load* (make-hiemod :name nil :defs nil :imports nil :exports nil))
  (load file t t)
  *hie-load*)

(defun hie-import-defs (import)
  "Import a module given by import spec `import`, into namespace `name`."
  (let* ((base-defs (hie-get-external-defs (hieimport-name import)))
	 (defs (if (hieimport-list import)
		   (loop for export in (hieimport-list import)
			 append (hie-filter-defs (not (hieimport-is-hidden import)) export base-defs)) 
		 base-defs))
	 (alias-defs (hie-alias-defs (hieimport-alias import) defs)))
    (if (hieimport-is-qualified import)
 	alias-defs
      (append defs alias-defs))))

(defun hie-get-external-defs (name)
  "Get the external defshash from module `name`."
  (let ((defs (gethash name hie-global-external-defs-hash 'missing)))
    (cond
     ((eq defs 'recursive-hack) nil)
     ((eq defs 'missing) (hie-cache-external-defs (hie-load-file (concat hie-modules-dir name))))
     (t defs))))

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

(defun hie-cache-external-defs (mod)
  ; hack for recursion
  (when (hiemod-name mod)
    (puthash (hiemod-name mod) 'recursive-hack hie-global-external-defs-hash)
    (puthash (hiemod-name mod)
	     (let ((defs (append (hiemod-defs mod)
				 (loop for import in (hiemod-imports mod) 
				       append (hie-import-defs import)))))
	       (hie-filter-prefixes (loop for export in (hiemod-exports mod)
					  append
					  (hie-filter-defs t export defs))))
	     hie-global-external-defs-hash)))


;; (defun hie-global-import-module (name)
;;   (let ((mod (hie-load-module name)))
;;     (hie-alias-defs name (hie-external-defs mod))))
  

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

(defun hie-try-idle-update ()
  "Try to update the current buffer when we're idle."
  (when (and hie-mode (buffer-modified-p))
   ; (hie-incorporate-internal (hie-run (buffer-file-name)))
    (hie-update-buffer)))

;

(defun hie-populate-defshash (defs defshash)
  (loop for def in defs do (puthash (hiedef-name def) def defshash)))

(defun hie-depopulate-defshash (defs defshash)
  (loop for def in defs do (remhash (hiedef-name def) defshash)))

(defun hie-update-buffer ()
  "Load (internal to `current-buffer`) module and update any caches."
  (interactive)
  
  (let ((mod (hie-run)))
    ; HACK.
    (when (or (hiemod-name mod) (hiemod-defs mod) (hiemod-imports mod) (hiemod-exports mod))
      (let ((new-imports-hash (copy-hash-table hie-buffer-imports-hash)))
	;; First, diff the imports.
	(loop for import in (hiemod-imports mod) do
	      ;; This is a new module
	      (unless (gethash import new-imports-hash)
		(let ((defs (hie-import-defs import)))
		  ;; Add it to the module defs
		  (puthash import defs new-imports-hash)
		  ;; Add it to the idents. (Note: if its shadowed by a local definition, it will get overwritten)
		  (hie-populate-defshash defs hie-buffer-idents-hash)))
	      (remhash import hie-buffer-imports-hash) ; Remove this key; if we iterate over this hashtable, we see the ones that were removed
	      )
	(loop for import being the hash-keys in hie-buffer-imports-hash 
	      ; again we might readd these
	      do (progn 
		   (hie-depopulate-defshash (gethash import hie-buffer-imports-hash) hie-buffer-idents-hash)
		   (remhash import new-imports-hash)))
	(setq hie-buffer-imports-hash new-imports-hash))

      ;; Second, handle the local definitions
      (let ((new-defined-hash (hie-new-hash)))
	(hie-populate-defshash (hiemod-defs mod) new-defined-hash)
	(hie-populate-defshash (hiemod-defs mod) hie-buffer-idents-hash)
	(hie-depopulate-defshash (hiemod-defs mod) hie-buffer-defined-hash) ; now only the ones to delete are left!

	;; don't bother any of this if there is nothing to delete
	(when (< 0 (hash-table-count hie-buffer-defined-hash))
	  (let (import-test-hash (hie-new-hash)) 
	    (loop for import being the hash-keys in hie-buffer-imports-hash
		  ; no use in checking qualified imports; can't shadow these
		  unless (hieimport-is-qualified import)
		  do (loop for def in (gethash import hie-buffer-imports)
			   ; ignore aliases
			   unless (string-match "^[A-Za-z0-9_]+\\." (hiedef-name def))
			   do (puthash (hiedef-name def) t import-test-hash)))
	    (loop for name being the hash-keys in hie-buffer-defined-hash
		  unless (gethash name import-test-hash)
		  do (remhash name hie-buffer-idents-hash)))) 
	(setq hie-buffer-defined-hash new-defined-hash)))))

(defun hie-do-setup-buffer (mod)
  "Prepare the buffer for hie."
  (interactive)
  (hie-populate-defshash (hiemod-defs mod) hie-buffer-defined-hash)
  (loop for import in (hiemod-imports mod)
	do (progn
	     (let ((defs (hie-import-defs import)))
	       (puthash import defs hie-buffer-imports-hash) 
	       (hie-populate-defshash defs hie-buffer-idents-hash))))
  (hie-populate-defshash (hiemod-defs mod) hie-buffer-idents-hash))

(defun hie-setup-buffer ()
  "Prepare the buffer for hie."
  (interactive)
  (when hie-mode
    (setq hie-buffer-idents-hash (hie-new-hash))
    (setq hie-buffer-imports-hash (hie-new-hash))
    ;(setq hie-buffer-exports-hash (hie-new-hash))
    (setq hie-buffer-defined-hash (hie-new-hash))
    (hie-do-setup-buffer (hie-run))))

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

(provide 'hie)
