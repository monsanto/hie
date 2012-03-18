
;;
;; Haskell In Emacs (HIE) v0.1
;;
;; Author:  Christopher Monsanto <chris@monsan.to>
;; Website: http://github.com/monsanto/eih/
;; License: GPLv3
;;
;; This file is not apart of GNU Emacs.
;; ^---- why do people always put this in their modules? Who cares?
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

(defvar hie-global-hash nil
  "A hash table of the identifiers globally available.")

(defvar hie-active-hash nil
  "A hash table of the (global) identifiers we are actively editing.")

(defvar hie-buffer-global-local-hash nil
  "A hash table for all of the identifiers in global scope, with local aliases.
The union of hie-global-hash and hie-buffer-local-hash.")
(make-variable-buffer-local 'hie-buffer-global-local-hash)

(defvar hie-buffer-local-hash nil
  "A hash table for all of the identifiers in local scope.
The union of hie-buffer-imports-hash and hie-buffer-defined-hash.")
(make-variable-buffer-local 'hie-buffer-local-hash)

(defvar hie-buffer-imports-hash nil
  "A hash table for all of the imported identifiers.")
(make-variable-buffer-local 'hie-buffer-imports-hash)

(defvar hie-buffer-defined-hash nil
  "A hash table for all the locally defined identifiers.")
(make-variable-buffer-local 'hie-buffer-defined-hash)

(defvar hie-buffer-imports nil
  "A list of all imported modules in the buffer.")
(make-variable-buffer-local'hie-buffer-imports)

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
; Main interactives
;

(defun hie-the-hash ()
  (if hie-mode
      hie-buffer-global-local-hash
    hie-global-hash))

(defun hie-jump-to-definition (&optional name)
  "Jump to the given definition."
  (interactive)
  (let* ((ident (haskell-ident-at-point))
	 (name (completing-read "Jump to definition: "
				(hie-the-hash)
				(lambda (key info) (string-match hie-jump-to-definition-regexp (nth 0 info)))
				nil
				(or name (if (gethash ident (hie-the-hash)) ident nil))))
	 (info (gethash name (hie-the-hash))))
    (find-file (nth 2 info))
    (goto-line (nth 3 info))))

(defun hie-show-signature (&optional name)
  "Show the signature in the mode line."
  (interactive)
  (let* ((ident (haskell-ident-at-point))
	 (name (completing-read "Show signature: "
				(hie-the-hash)
				(lambda (key info) (not (nth 1 info)))
				nil
				(or name (if (gethash ident (hie-the-hash)) ident nil))))
	 (info (gethash name (hie-the-hash))))
    (display-message-or-buffer (hie-make-help info))))

;
; Formatting functions
;

(defun hie-make-qualname (mod name)
  (if mod
      (concat mod "." name)
    name))

(defun hie-make-help (info)
  "Make the help!"
  (destructuring-bind (mod instance file line column sig qh) info
    (cond
     ((and (not (equal sig "")) (not (equal qh ""))) (concat sig "\n\n" qh))
     ((equal qh "") sig)
     ((equal sig "") qh))))

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

(defun hie-new-hash (table)
  "Create a hash table."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (name . info) in table do
	  (puthash name info hash))
    hash))

(defun hie-update-hash1 (new-hash old-hash composite-hash)
  "Incrementally update composite-hash1,
by exploiting the difference between new-hash and old-hash. Warning: This destroys old-hash!"
  (maphash (lambda (k v)
	     (puthash k v composite-hash) 
	     (remhash k old-hash)) new-hash)
  (maphash (lambda (k v)
	     (remhash k composite-hash)) old-hash))

(defun hie-diff-hash (add-hash remove-hash composite-hash)
  (maphash (lambda (k v) (puthash k v composite-hash)) add-hash)
  (maphash (lambda (k v) (remhash k composite-hash)) remove-hash))

(defun hie-update-hash2 (new-hash old-hash composite-hash1 composite-hash2)
  "Incrementally update composite-hash1 and composite-hash2,
by exploiting the difference between new-hash and old-hash. Warning: This destroys old-hash!"
  (maphash (lambda (k v)
	     (puthash k v composite-hash1)
	     (puthash k v composite-hash2)
	     (remhash k old-hash)) new-hash)
  (maphash (lambda (k v)
	     (remhash k composite-hash1)
	     (remhash k composite-hash2)) old-hash))

(defun hie-global-run (file module)
  (call-process-shell-command (format "hie export %s %s%s" file hie-modules-dir module)))

(defun hie-local-run (file)
  (let ((tmpfile (make-temp-file "hie")))
    (call-process-shell-command (format "hie all %s %s" file tmpfile))
    tmpfile))

(defun hie-load-file (file)
  "Create completion table, etc for given module."
  (setq hie-load nil)
  (load file t t)
  hie-load)

(defun hie-load-module (module &optional renamemod)
  "Create completion table, etc for given module."
  (let ((table (hie-load-file (concat hie-modules-dir module))))
    (if renamemod
	(loop for (name mod instance file line column sig qh) in table
	      collect (list name renamemod instance file line column sig qh))
	table)))

(defun hie-load-all-modules () 
  (loop for file in (directory-files hie-modules-dir t "^[^.]") append (hie-load-file file)))

(defun hie-load-active-modules ()
  (if hie-active-modules-regexp
      (loop for file in (directory-files hie-modules-dir t hie-active-modules-regexp) append (hie-load-file file))))

(defun hie-global-identifiers-table (table)
  (loop for (name mod instance file line column sig qh) in table
	collect
	(list (hie-make-qualname mod name) mod instance file line column sig qh)))

(defun hie-local-identifiers-table (table)
  (loop for (name mod instance file line column sig qh) in table
	for (mod modalias importlist qualified hiding) = (assoc mod hie-buffer-imports)
	for qualname = (hie-make-qualname (or modalias mod) name)
	for exposed =  (or (not importlist)
			   (and hiding (not (member name importlist)))
			   (and (not hiding) (member name importlist)))
	if exposed
	collect (list qualname mod instance file line column sig qh)
	if (and exposed (not qualified))
	collect (list name mod instance file line column sig qh)))

;
; Updating the global identifiers
;
(defun hie-update-global-hash ()
  "If the current buffer has been modified, incrementally update the database of globals and propagate"
  (if (and hie-mode (hie-is-active-module))
      (progn
	(hie-global-run (buffer-file-name) (hie-get-module-name))
	(let* ((hash (hie-new-hash (hie-global-identifiers-table (hie-load-active-modules)))))
	  ; note, hie-active-hash is now the diff.
 	  (hie-update-hash1 hash hie-active-hash hie-global-hash)
	  (loop for buffer in (buffer-list)
		do (with-current-buffer buffer
		     (when hie-mode 
		       (hie-diff-hash hash hie-active-hash hie-buffer-global-local-hash)
		       (if (hie-does-import-active-module)
			   (hie-update-imports-hash)))))
	  (setq hie-active-hash hash)))))

(defun hie-setup-global ()
  "Load all global identifiers."
  (interactive)
  (setq hie-global-hash (hie-new-hash (hie-global-identifiers-table (hie-load-all-modules))))
  (setq hie-active-hash (hie-new-hash (hie-global-identifiers-table (hie-load-active-modules)))))

;
; Updating buffer imports
;

(defun hie-update-imports-hash ()
  "Update the import hash."
  (interactive)
  (let* ((hie-list (loop for module in hie-buffer-imports append (hie-load-module (nth 0 module))))
	 (hash (hie-new-hash (hie-local-identifiers-table hie-list))))
    (hie-update-hash2 hash hie-buffer-imports-hash hie-buffer-local-hash hie-buffer-global-local-hash)
    (setq hie-buffer-imports-hash hash)))

(defun hie-try-update-imports ()
  "Check if imports have changed; if they have, update the import hash."
  (interactive)
  (if hie-mode 
      (let ((new-imports (hie-get-imports)))
	(unless (equal new-imports hie-buffer-imports)
	  (setq hie-buffer-imports new-imports)
	  (hie-update-imports-hash)))))

;
; Updating locally defined identifiers
;

(defun hie-update-defined-hash ()
  "Update the locally defined identifiers hash."
  (interactive)
  (if hie-mode
      (let ((hash (hie-new-hash (hie-local-identifiers-table
				 (hie-load-file (hie-local-run (buffer-file-name)))))))
	; TODO: fix total hack here
	(unless (= 0 (hash-table-count hash))
	  (hie-update-hash2 hash hie-buffer-defined-hash hie-buffer-local-hash hie-buffer-global-local-hash)
	  (setq hie-buffer-defined-hash hash)))))

;

(defun hie-setup-buffer ()
  "Prepare the buffer for hie."
  (interactive)
  (when hie-mode
    (setq hie-buffer-global-local-hash (copy-hash-table hie-global-hash))
    (setq hie-buffer-local-hash (hie-new-hash nil))
    (setq hie-buffer-imports-hash (hie-new-hash nil))
    (setq hie-buffer-defined-hash (hie-new-hash nil))
    (hie-try-update-imports)
    (hie-update-defined-hash)))

(defun hie-on-buffer-save ()
  "Update hash table."
  (interactive)
  (hie-try-update-imports)
  (hie-update-defined-hash)
  (hie-update-global-hash))

;; Autocomplete stuff

; For some reason this works better if we copy it every time...?
(defun hie-ac-candidates ()
  (let (candidates)
    (maphash
     (lambda (k v)
       (unless (nth 1 v) (setq candidates (cons k candidates)))) hie-buffer-local-hash)
    candidates))
					
(defun hie-ac-document (name)
  (let ((info (gethash name hie-buffer-local-hash)))
    (hie-make-help info)))

(defvar ac-source-hie
  '((candidates . hie-ac-candidates)
    (document . hie-ac-document)))

(defun hie-ac-module-candidates ()
  (directory-files hie-modules-dir nil "^[^.]"))

(defvar ac-source-hie-modules
  '((candidates . hie-ac-module-candidates)
    (prefix . "^[ \t]*import \\(.*\\)")
    (cache)))

; Setup

(hie-setup-global)
(add-hook 'after-save-hook 'hie-on-buffer-save)

(provide 'hie)
