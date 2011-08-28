;; This mode is implemented as a derivation of `haskell' mode,
;; indentation and font locking is courtesy that mode.  The
;; inter-process communication is courtesy `comint'.  The symbol at
;; point acquisition is courtesy `thingatpt'.  The directory search
;; facilities are courtesy `find-lisp'.

(require 'scheme)
(require 'comint)
(require 'thingatpt)
(require 'find-lisp)

(defvar hsc3-help-directory
  nil
  "*The directory containing the help files (default=nil).")

(defvar hsc3-literate-p
  t
  "*Flag to indicate if we are in literate mode (default=t).")

(make-variable-buffer-local 'hsc3-literate-p)

(defun hsc3-quit-haskell ()
  "Quit haskell."
  (interactive)
  (hsc3-send-string ":quit"))

(defun hsc3-unlit (c s)
  "Remove bird, or alternate, literate marks"
   (replace-regexp-in-string (format "^%c " c) "" s))

(defun hsc3-remove-literates (c s)
  "Remove bird, or alternate, literate lines"
  (replace-regexp-in-string (format "^[^%c]*$" c) "" s))

(defun hsc3-help ()
  "Lookup up the name at point in the hsc3 help files."
  (interactive)
  (mapc (lambda (filename)
	  (find-file-other-window filename))
	(find-lisp-find-files hsc3-help-directory
			      (concat "^"
				      (thing-at-point 'symbol)
				      "\\.help\\.lhs"))))

(defun hsc3-sc3-help ()
  "Lookup up the name at point in the SC3 help files."
  (interactive)
  (hsc3-send-string (format "viewSC3Help (toSC3Name \"%s\")"
                            (thing-at-point 'symbol))))

(defun hsc3-ugen-summary ()
  "Lookup up the UGen at point in hsc3-db"
  (interactive)
  (hsc3-send-string (format "Sound.SC3.UGen.DB.ugenSummary_ci \"%s\""
                            (thing-at-point 'symbol))))

(defun hsc3-request-type ()
  "Ask ghci for the type of the name at point."
  (interactive)
  (hsc3-send-string (concat ":t " (thing-at-point 'symbol))))

(defun chunk-string (n s)
  "Split a string into chunks of 'n' characters."
  (let* ((l (length s))
         (m (min l n))
         (c (substring s 0 m)))
    (if (<= l n)
        (list c)
      (cons c (chunk-string n (substring s n))))))

(defun hsc3-send-string (s)
  (if (comint-check-proc inferior-haskell-buffer)
      (let ((cs (chunk-string 64 (concat s "\n"))))
        (mapcar
         (lambda (c) (comint-send-string inferior-haskell-buffer c))
         cs))
    (error "no hsc3 process running?")))

(defun hsc3-run-line ()
  "Send the current line to the interpreter."
  (interactive)
  (let* ((s (buffer-substring (line-beginning-position)
			      (line-end-position)))
	 (s* (if hsc3-literate-p
		 (hsc3-unlit ?> s)
	       s)))
    (hsc3-send-string s*)))

(defun hsc3-run-multiple-lines ()
  "Send the current region to the interpreter as a single line."
  (interactive)
  (let* ((s (buffer-substring-no-properties (region-beginning)
					    (region-end)))
	 (s* (if hsc3-literate-p
		 (hsc3-unlit ?> (hsc3-remove-literates ?> s))
	       s)))
    (hsc3-send-string (replace-regexp-in-string "\n" " " s*))))

(defun hsc3-run-main ()
  "Run current main."
  (interactive)
  (hsc3-send-string "main"))

(defun hsc3-interrupt-haskell ()
  "Interrup haskell interpreter"
  (interactive)
  (if (comint-check-proc inferior-haskell-buffer)
      (with-current-buffer inferior-haskell-buffer
        (interrupt-process (get-buffer-process (current-buffer))))
    (error "no haskell interpreter process running?")))

(defun hsc3-reset-scsynth ()
  "Reset scsynth"
  (interactive)
  (hsc3-send-string "withSC3 reset"))

(defun hsc3-stop ()
  "Interrup haskell interpreter & reset scsynth"
  (interactive)
  (progn
    (hsc3-interrupt-haskell)
    (hsc3-reset-scsynth)))

(defun hsc3-status-scsynth ()
  "Status"
  (interactive)
  (hsc3-send-string "withSC3 serverStatus >>= mapM putStrLn"))

(defun hsc3-quit-scsynth ()
  "Quit"
  (interactive)
  (hsc3-send-string "withSC3 (\fd -> send fd quit)"))

(defvar hsc3-mode-map nil
  "Haskell SuperCollider keymap.")

(defun hsc3-mode-keybindings (map)
  "Haskell SuperCollider keybindings."
  (define-key map [?\C-c ?\C-c] 'hsc3-run-line)
  (define-key map [?\C-c ?\C-e] 'hsc3-run-multiple-lines)
  (define-key map [?\C-c ?\C-h] 'hsc3-help)
  (define-key map [?\C-c ?\C-j] 'hsc3-sc3-help)
  (define-key map [?\C-c ?\C-i] 'hsc3-interrupt-haskell)
  (define-key map [?\C-c ?\C-k] 'hsc3-stop)
  (define-key map [?\C-c ?\C-m] 'hsc3-run-main)
  (define-key map [?\C-c ?\C-p] 'hsc3-status-scsynth)
  (define-key map [?\C-c ?\C-q] 'hsc3-quit-haskell)
  (define-key map [?\C-c ?\C-0] 'hsc3-quit-scsynth)
  (define-key map [?\C-c ?\C-s] 'hsc3-reset-scsynth)
  (define-key map [?\C-c ?\C-u] 'hsc3-ugen-summary))

(defun hsc3-mode-menu (map)
  "Haskell SuperCollider menu."
  (define-key map [menu-bar hsc3]
    (cons "Haskell-SuperCollider" (make-sparse-keymap "Haskell-SuperCollider")))
  (define-key map [menu-bar hsc3 help]
    (cons "Help" (make-sparse-keymap "Help")))
  (define-key map [menu-bar hsc3 help hsc3]
    '("Haskell SuperCollider help" . hsc3-help))
  (define-key map [menu-bar hsc3 help ugen]
    '("UGen parameter summary" . hsc3-ugen-summary))
  (define-key map [menu-bar hsc3 expression]
    (cons "Expression" (make-sparse-keymap "Expression")))
  (define-key map [menu-bar hsc3 expression run-main]
    '("Run main" . hsc3-run-main))
  (define-key map [menu-bar hsc3 expression run-multiple-lines]
    '("Run multiple lines" . hsc3-run-multiple-lines))
  (define-key map [menu-bar hsc3 expression run-line]
    '("Run line" . hsc3-run-line))
  (define-key map [menu-bar hsc3 scsynth]
    (cons "SCSynth" (make-sparse-keymap "SCSynth")))
  (define-key map [menu-bar hsc3 scsynth quit]
    '("Quit scsynth" . hsc3-quit-scsynth))
  (define-key map [menu-bar hsc3 scsynth status]
    '("Display status" . hsc3-status-scsynth))
  (define-key map [menu-bar hsc3 scsynth reset]
    '("Reset scsynth" . hsc3-reset-scsynth))
  (define-key map [menu-bar hsc3 haskell]
    (cons "Haskell" (make-sparse-keymap "Haskell")))
  (define-key map [menu-bar hsc3 haskell quit-haskell]
    '("Quit haskell" . hsc3-quit-haskell)))

(if hsc3-mode-map
    ()
  (let ((map (make-sparse-keymap "Haskell-SuperCollider")))
    (hsc3-mode-keybindings map)
    (hsc3-mode-menu map)
    (setq hsc3-mode-map map)))

(define-derived-mode
  literate-hsc3-mode
  hsc3-mode
  "Literate Haskell SuperCollider"
  "Major mode for interacting with an inferior haskell process."
  (setq hsc3-literate-p t)
  (setq haskell-literate 'bird)
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-hsc3-mode))

(define-derived-mode
  hsc3-mode
  haskell-mode
  "Haskell SuperCollider"
  "Major mode for interacting with an inferior haskell process."
  (setq hsc3-literate-p nil)
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.hs$" . hsc3-mode))

(provide 'hsc3)
