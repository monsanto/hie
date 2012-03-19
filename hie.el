
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

(defvar hie-modules-dir "~/.hie/"
  "Where should we look for caches?")

(defvar hie-jump-to-definition-regexp ".*"
  "A regexp matching the modules you want to show up in the jump-to-definition list.")

(defvar hie-active-modules-regexp nil
  "A regexp matching modules we are currently editing.")
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
; Consts
;

(defconst hie-import-line-regexp
  (concat "^[ \t]*import[ \t]"
	      "[ \t]*\\(?:\\(qualified\\)[ \t]\\)?"
	      "[ \t]*\\([A-Za-z0-9.]+\\)"
	      "[ \t]*\\(?:as[ \t]*\\([A-Za-z0-9.]+\\)\\)?"
	      "[ \t]*\\(?:\\(hiding\\)\\)?"
	      "[ \t]*\\(?:(\\(.+\\))\\)?"
	      )
  "The regexp used for finding imports")

(defconst hie-module-regexp
  "^[ \t]*module[ \t]+\\([A-Za-z0-9.]+\\)"
  "The regexp used for finding modules")

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
				(or name (if (gethash ident (hie-the-hash)) ident nil))))
	 (def (gethash name (hie-the-hash))))
    (display-message-or-buffer (hie-make-help def))))

;
; Formatting functions
;


;
; Scanning functions
;

(defun hie-is-active-module ()
  "Return true if this is an active module."
  (let ((mod (hie-get-module-name)))
    (if (and mod hie-active-modules-regexp)
      (string-match hie-active-modules-regexp (hie-get-module-name)))))

(defun hie-does-import-active-module ()
  "Return true if this is an active module."
  (loop for (mod . rest) in hie-buffer-imports 
	if (and hie-active-modules-regexp (string-match hie-active-modules-regexp mod))
	do (return t)))

(defun hie-get-imports ()
  "Scan for imports in the file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (loop repeat 10000 while (re-search-forward hie-import-line-regexp nil t)
    	  collect
    	  (list (match-string-no-properties 2)
    		(match-string-no-properties 3)
    		(save-match-data (split-string (or (match-string-no-properties 5) "") "[ ,\t\n\r]+" t))
    		(match-string-no-properties 1)
    		(match-string-no-properties 4)))))

(defun hie-get-module-name ()
  "Get the module of the Haskell file."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward hie-module-regexp nil t)
	(match-string-no-properties 1))))

;
; Hash table and loading code
;


;; (defun hie-update-hash1 (new-hash old-hash composite-hash)
;;   "Incrementally update composite-hash1,
;; by exploiting the difference between new-hash and old-hash. Warning: This destroys old-hash!"
;;   (maphash (lambda (k v)
;; 	     (puthash k v composite-hash) 
;; 	     (remhash k old-hash)) new-hash)
;;   (maphash (lambda (k v)
;; 	     (remhash k composite-hash)) old-hash))

;; (defun hie-diff-hash (add-hash remove-hash composite-hash)
;;   (maphash (lambda (k v) (puthash k v composite-hash)) add-hash)
;;   (maphash (lambda (k v) (remhash k composite-hash)) remove-hash))

;; (defun hie-update-hash2 (new-hash old-hash composite-hash1 composite-hash2)
;;   "Incrementally update composite-hash1 and composite-hash2,
;; by exploiting the difference between new-hash and old-hash. Warning: This destroys old-hash!"
;;   (maphash (lambda (k v)
;; 	     (puthash k v composite-hash1)
;; 	     (puthash k v composite-hash2)
;; 	     (remhash k old-hash)) new-hash)
;;   (maphash (lambda (k v)
;; 	     (remhash k composite-hash1)
;; 	     (remhash k composite-hash2)) old-hash))



(defun hie-load-file (file)
  "Create completion table, etc for given module."
  (setq *hie-load* (make-hiemod :name nil :defs nil :imports nil :exports nil))
  (load file t t)
  *hie-load*)


;; Scratch Space

(defun hie-make-help (def) 
  (if (and (hiedef-signature def) (hiedef-help def))
      (concat (hiedef-signature def) "\n\n" (hiedef-help def))
    (or (hiedef-signature def) (hiedef-help def))))

;; (defun hie-external-defs (mod)
;;   (hie-filter-defs t (hiemod-exports mod) (hie-internal-defs mod)))

;; (defun hie-internal-defs (mod)
;;   (append (hiemod-defs mod) 
;; 	  (loop for import in (hiemod-imports mod)
;; 		append (hie-import-module import))))

(defun hie-import-defs (import)
  "Import a module given by import spec `import`, into namespace `name`."
  (let* ((base-defs (hie-get-external-defs (hieimport-name import)))
	 (defs (if (hieimport-list import) 
		   (hie-filter-defs (hieimport-is-hidden import) (hieimport-list import) base-defs) 
		 base-defs))
	 (alias-defs (hie-alias-defs (hieimport-alias import) defs)))
    (if (hieimport-is-qualified import)
 	alias-defs
      (append defs alias-defs))))

; TODO handle recursive stuff
(defun hie-get-external-defs (name)
  "Get the external defshash from module `name`."
  (or (gethash name hie-global-external-defs-hash)
      ;(gethash name hie-all-buffers-exported)
      (hie-cache-external-defs (hie-load-file (concat hie-modules-dir name)))))

(defun hie-filter-defs (selector export defs)
  (loop for def in defs
	with (tag . name) = export
	if (and (eq tag 'id) (eq selector (equal (hiedef-name def) name)))
	collect def
	if (and (eq tag 'all) (or (equal (hiedef-name def) name)
				  (equal (hiedef-parent def) name)))
	collect def
	; TODO
	;if (and (eq tag 'mod) (eq selector (has-prefix (hiedef-name def))))
	;collect def-fixed
	))

(defun hie-filter-populate-defshash (selector export defshash dest-defshash)
  "Add elements of defshash to dest-defshash if they pass muster!"
  (loop for k being the hash-keys in defshash using (hash-value v)
	with (tag . name) = export
	do (cond
	    ((eq tag 'id)
	     (when (eq selector (equal (hiedef-name def) name))
	       (puthash k v dest-defshash)))
	    ((eq tag 'all)
	     (when (eq selector (or (equal (hiedef-name def) name)
				    (equal (hiedef-parent def) name)))
	       (puthash k v dest-defshash)))
	    ((eq tag 'mod)
	     (when (eq selector (has-prefix (hiedef-name def)))
	       (puthash (without-prefix k) v dest-defshash))))))

(defun hie-alias-defs (new-scope defs)
  "Strips out anything not in the namespace old-mod, and renames the rest to new-mod."
  (loop for def in defs
	collect (progn
		  (setq def (copy-hiedef def)) 
		  (setf (hiedef-name def)
			(concat new-scope "." (hiedef-name def)))
		  def)))

(defun hie-cache-external-defs (mod)
  (puthash (hiemod-name mod)
	   (append (hiemod-defs mod)
		   (loop for import in (hiemod-imports mod)
			 append (hie-filter-defs t
						 (hiemod-exports mod)
						 (hie-import-defs import))))
	   hie-global-external-defs-hash))

;; (defun hie-load-active-modules ()
;;   (if hie-active-modules-regexp
;;       (loop for file in (directory-files hie-modules-dir t hie-active-modules-regexp)
;; 	    append
;; 	    (hie-load-file file))))

;; (defun hie-global-import-module (name)
;;   (let ((mod (hie-load-module name)))
;;     (hie-alias-defs name (hie-external-defs mod))))



;; (defun hie-incorporate-internal (mod)
;;   "Load (internal to `current-buffer`) module and update any caches."
  
;;   ; diff the local definitions
;;   ; - if different, update hie-buffer-local-defs-hash
;;   ; diff the import list
;;   ; - if different, update hie-buffer-imports-hash

;;   ; Update local definitions
;;   (setf (hiemod-defs hie-buffer-module)
;; 	(hie-update-hashes (hiemodule-defs mod)
;; 			   (hiemodule-defs hie-buffer-module)
;; 			   hie-buffer-idents-hash
;; 			   hie-all-buffers-idents-hash))

;;   ; this should return add and del modules
;;   (destructuring-bind (new-hash add del) (hie-update-hashes1
;; 					  (hiemod-imports mod)
;; 					  (hiemod-imports hie-buffer-module))
;;     (setf (hiemod-imports hie-buffer-module) new-hash)
;;     (loop for import in add
;; 	  do 
;; 	  (let* ((defs (hie-import-module import)))
;; 	    (puthash import defs (hiemod-imports hie-buffer-module))
;; 	    (loop for def in defs
;; 					; if the ident was defined locally, don't clobber it
;; 		  unless (member (hiedef-name def) (hiemod-defs hie-buffer-module))
;; 		  do (progn
;; 		       (puthash (hiedef-name def) def hie-buffer-idents-hash)
;; 		       (puthash (hiedef-name def) def hie-all-buffers-idents-hash) ; need to alias
;; 		       ))))
;;     (loop for import in del
;; 	  do (let* ((defs (hie-import-module import)))
;; 	       (remhash import (hiemod-imports hie-buffer-module))
;; 	       (loop for def in defs
;; 					; if the ident was defined locally, don't clobber it
;; 		     unless (member (hiedef-name def) (hiemod-defs hie-buffer-module))
;; 		     do (progn
;; 			  (remhash (hiedef-name def) hie-buffer-idents-hash)
;; 			  (remhash (hiedef-name def) hie-all-buffers-idents-hash) ; need to alias
;; 			  )))))

 
  ; TODO a deletion from local defs is legit if not in the new imports
  
  ; keep track of add/remove local def and import defs... 
  ; keep track of adding and removing from export list
  ; for each local delete, go ahead and delete from the all-exported
  ; for each local add, does it exist in the new exported modules?
  ; for each export add not in the local delete, go through the OLD ids and see if we can add.
  ; for each export delete, go ahead and delete from the all-exported.
  
  ; now check exports... what do we do on rename?
  ;(update hie-exported-modules-hash)
  

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
    (hie-update-buffer)
    ))


;; End Scratch Space 

;

(defun hie-populate-defshash (defs defshash)
  (loop for def in defs do (puthash (hiedef-name def) def defshash)))

(defun hie-depopulate-defshash (defs defshash)
  (loop for def in defs do (remhash (hiedef-name def) defshash)))


(defun hie-update-buffer ()
  "Load (internal to `current-buffer`) module and update any caches."
  
  ; diff the local definitions
  ; - if different, update hie-buffer-local-defs-hash
  ; diff the import list
  ; - if different, update hie-buffer-imports-hash

 
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
	(loop for import being the hash-keys in hie-buffer-imports-hash using (hash-value defs) 
	      ; again we might readd these
	      (hie-depopulate-defshash defs hie-buffer-idents-hash))
	(setq hie-buffer-imports-hash new-imports-hash))

      ;; Second, handle the local definitions
      (let ((new-defined-hash (hie-new-hash)))
	(hie-populate-defshash (hiemod-defs mod) new-defined-hash)
	(hie-populate-defshash (hiemod-defs mod) hie-buffer-idents-hash)
	(hie-depopulate-defshash (hiemod-defs mod) hie-buffer-defined-hash) ; now only the ones to delete are left!
	(when (< 0 (hash-table-count hie-buffer-defined-hash))
	  (let (import-test-hash (hie-new-hash)) 
	    (loop for import being the hash-keys in hie-buffer-imports-hash using (hash-value defs) 
		  unless (hieimport-is-qualified import)
		  do (loop for def in defs
			   unless ()

			   )
		  (puthash  t )
		  )
	    (loop for name being the hash-keys in hie-buffer-defined-hash using (hash-value def)
	      ; if we delete a binding, it might be picked up by an import. check
	      ; TODO check if anything is deleted, then make a hash table (skipping qualified modules), now check.
	      (unless t
		(remhash name hie-buffer-idents-hash)))
	    ))
	
	(setq hie-buffer-defined-hash new-defined-hash)))))

(defun hie-do-setup-buffer (mod)
  "Prepare the buffer for hie."
  (interactive)
  (hie-populate-defshash (hiemod-defs mod) hie-buffer-defined-hash)
  (loop for import in (hiemod-imports mod)
	do (progn
	     (let ((defs (hie-import-defs import)))
	       (hie-populate-defshash defs hie-buffer-imports-hash)
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

; For some reason this works better if we copy it every time...?
;; (defun hie-ac-candidates ()
;;   (copy-hash-table hie-buffer-idents-hash))

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
  (directory-files hie-modules-dir nil "^[^.]"))

;; (defvar ac-source-hie-modules
;;   '((candidates . hie-ac-module-candidates)
;;     (prefix . "^[ \t]*import \\(.*\\)")
;;     (cache)))

(run-with-idle-timer 1 t 'hie-try-idle-update)
;(add-hook 'after-save-hook 'hie-on-buffer-save)

(provide 'hie)
