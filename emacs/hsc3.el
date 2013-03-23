;; This mode is implemented as a derivation of `haskell' mode,
;; indentation and font locking is courtesy that mode.  The
;; inter-process communication is courtesy `comint'.  The symbol at
;; point acquisition is courtesy `thingatpt'.  The directory search
;; facilities are courtesy `find-lisp'.

(require 'scheme)
(require 'comint)
(require 'thingatpt)
(require 'find-lisp)
(require 'inf-haskell)
;;(require 'sclang)

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

(defun hsc3-unlit (s)
  "Remove bird literate marks and preceding comment marker"
   (replace-regexp-in-string "^[> ]* ?" "" s))

(defun hsc3-uncomment (s)
  "Remove initial comment and Bird-literate markers if present"
   (replace-regexp-in-string "^[- ]*[> ]*" "" s))

(defun hsc3-remove-non-literates (s)
  "Remove non-bird literate lines"
  (replace-regexp-in-string "^[^>]*$" "" s))

(defun hsc3-help ()
  "Lookup up the name at point in the hsc3 help files."
  (interactive)
  (mapc (lambda (filename)
	  (find-file-other-window filename))
	(find-lisp-find-files hsc3-help-directory
			      (concat "^"
				      (thing-at-point 'symbol)
				      "\\.help\\.lhs"))))

(defun hsc3-sc3-ugen-help ()
  "Lookup up the UGen name at point in the SC3 help files."
  (interactive)
  (hsc3-send-string
   (format
    "Sound.SC3.viewSC3Help (Sound.SC3.toSC3Name \"%s\")"
    (thing-at-point 'symbol))))

(defun hsc3-sc3-server-help ()
  "Lookup up the Server Command name at point in the SC3 help files."
  (interactive)
  (hsc3-send-string
   (format "Sound.SC3.Server.Help.viewServerHelp \"%s\""
           (thing-at-point 'symbol))))

(defun hsc3-ugen-summary ()
  "Lookup up the UGen at point in hsc3-db"
  (interactive)
  (hsc3-send-string
   (format "Sound.SC3.UGen.DB.ugenSummary_ci \"%s\""
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

(defun hsc3-cd ()
  "Change directory at ghci to current value of 'default-directory'."
  (interactive)
  (hsc3-send-string (format ":cd %s" default-directory)))

(defun hsc3-load-buffer ()
  "Load the current buffer."
  (interactive)
  (save-buffer)
  (hsc3-see-haskell)
  (hsc3-send-string (format ":load \"%s\"" buffer-file-name)))

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
		 (hsc3-unlit s)
	       (hsc3-uncomment s))))
    (hsc3-send-string s*)))

(defun region-string ()
  "Get region as string (no properties)"
  (buffer-substring-no-properties (region-beginning)
                                  (region-end)))

(defun hsc3-concat (l)
  (apply #'concat l))

(defun hsc3-region-string ()
  "Translate the current region into a single line (unlit, uncomment)."
  (let* ((s (region-string))
	 (s* (if hsc3-literate-p
		 (hsc3-unlit (hsc3-remove-non-literates s))
	       (hsc3-concat (mapcar 'hsc3-uncomment (split-string s "\n"))))))
    (replace-regexp-in-string "\n" " " s*)))

(defun hsc3-run-multiple-lines ()
  "Send the current region to the haskell interpreter as a single line."
  (interactive)
  (hsc3-send-string (hsc3-region-string)))

(defun hsc3-run-multiple-lines-sclang ()
  "Send the current region to the sclang interpreter as a single line."
  (interactive)
  (sclang-eval-string (hsc3-region-string) t))

(defun hsc3-run-consecutive-lines ()
  "Send the current region to the interpreter one line at a time."
  (interactive)
  (mapcar 'hsc3-send-string
          (mapcar 'hsc3-unlit (split-string (region-string) "\n"))))

(defun hsc3-run-layout-block ()
  "Variant of `hsc3-run-consecutive-lines' with ghci layout quoting."
  (interactive)
  (hsc3-send-string ":{")
  (hsc3-run-consecutive-lines)
  (hsc3-send-string ":}"))

(defun hsc3-run-main ()
  "Run current main."
  (interactive)
  (hsc3-send-string "main"))

(defun hsc3-wait ()
  "Wait for prompt after sending command."
  (interactive)
  (inferior-haskell-wait-for-prompt (inferior-haskell-process)))

(defun hsc3-id-rewrite-region ()
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "hsc3-id-rewrite"
   nil
   t))

(defun hsc3-id-rewrite ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "hsc3-id-rewrite" nil t))

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
  (hsc3-send-string "Sound.SC3.withSC3 Sound.SC3.reset"))

(defun hsc3-stop ()
  "Interrup haskell interpreter & reset scsynth"
  (interactive)
  (progn
    (hsc3-interrupt-haskell)
    (hsc3-reset-scsynth)))

(defun hsc3-status-scsynth ()
  "Status"
  (interactive)
  (hsc3-send-string
   "Sound.SC3.withSC3 Sound.SC3.serverStatus >>= mapM putStrLn"))

(defun hsc3-quit-scsynth ()
  "Quit"
  (interactive)
  (hsc3-send-string
   "Sound.SC3.withSC3 (Sound.SC3.send Sound.SC3.quit)"))

(defun hsc3-update-hsc3-tags ()
  "Update hsc3 TAGS file, must be run from hsc3 directory."
  (interactive)
  (if (file-exists-p "hsc3.cabal")
      (call-process-shell-command
       "find Sound . -name '*.*hs' | xargs hasktags -e"
       nil
       nil)
    (error "not at hsc3 directory?")))

(defun hsc3-draw-graph ()
  "Draw the UGen graph at point."
  (interactive)
  (hsc3-send-string
   (concat "Sound.SC3.UGen.Dot.draw " (thing-at-point 'symbol))))

(defun hsc3-draw-graph-m ()
  "Draw the UGen graph at point."
  (interactive)
  (hsc3-send-string
   (concat "Sound.SC3.UGen.Dot.draw =<<" (thing-at-point 'symbol))))

(defun hsc3-local-dot ()
  "Copy '/tmp/hsc3.dot' to 'buffer-name' .dot."
  (interactive)
  (let ((nm (concat (file-name-sans-extension (buffer-name)) ".dot")))
    (copy-file "/tmp/hsc3.dot" nm t)))

(defun hsc3-set-prompt ()
  "Set ghci prompt to hsc3."
  (interactive)
  (hsc3-send-string ":set prompt \"hsc3> \""))

(defun hsc3-see-haskell ()
 "Show haskell output."
 (interactive)
 (let* ((p (inferior-haskell-process))
        (b (process-buffer p)))
   (hsc3-set-prompt)
   (delete-other-windows)
   (split-window-vertically)
   (with-current-buffer b
     (let ((window (display-buffer (current-buffer))))
       (goto-char (point-max))
       (save-selected-window
         (set-window-point window (point-max)))))))

(defvar hsc3-mode-map nil
  "Haskell SuperCollider keymap.")

(defun hsc3-mode-keybindings (map)
  "Haskell SuperCollider keybindings."
  (define-key map [?\C-c ?<] 'hsc3-load-buffer)
  (define-key map [?\C-c ?>] 'hsc3-see-haskell)
  (define-key map [?\C-c ?\C-c] 'hsc3-run-line)
  (define-key map [?\C-c ?\C-e] 'hsc3-run-multiple-lines)
  (define-key map [?\C-c ?\M-e] 'hsc3-run-multiple-lines-sclang)
  (define-key map [?\C-c ?\C-r] 'hsc3-run-consecutive-lines)
  (define-key map [?\C-c ?\C-f] 'hsc3-run-layout-block)
  (define-key map [?\C-c ?\C-h] 'hsc3-help)
  (define-key map [?\C-c ?\C-g] 'hsc3-draw-graph)
  (define-key map [?\C-c ?\M-g] 'hsc3-draw-graph-m)
  (define-key map [?\C-c ?\C-j] 'hsc3-sc3-ugen-help)
  (define-key map [?\C-c ?\C-/] 'hsc3-sc3-server-help)
  (define-key map [?\C-c ?\C-i] 'hsc3-interrupt-haskell)
  (define-key map [?\C-c ?\C-k] 'hsc3-reset-scsynth)
  (define-key map [?\C-c ?\C-m] 'hsc3-run-main)
  (define-key map [?\C-c ?\C-p] 'hsc3-status-scsynth)
  (define-key map [?\C-c ?\C-q] 'hsc3-quit-haskell)
  (define-key map [?\C-c ?\C-0] 'hsc3-quit-scsynth)
  (define-key map [?\C-c ?\C-s] 'hsc3-stop)
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
  (define-key map [menu-bar hsc3 help sc3-server]
    '("SuperCollider Server Command help" . hsc3-sc3-server-help))
  (define-key map [menu-bar hsc3 help sc3-ugen]
    '("SuperCollider UGen help" . hsc3-sc3-ugen-help))
  (define-key map [menu-bar hsc3 expression]
    (cons "Expression" (make-sparse-keymap "Expression")))
  (define-key map [menu-bar hsc3 expression stop]
    '("Stop (interrupt and reset)" . hsc3-stop))
  (define-key map [menu-bar hsc3 expression change-directory]
    '("Change directory" . hsc3-cd))
  (define-key map [menu-bar hsc3 expression load-buffer]
    '("Load buffer" . hsc3-load-buffer))
  (define-key map [menu-bar hsc3 expression run-main]
    '("Run main" . hsc3-run-main))
  (define-key map [menu-bar hsc3 expression run-layout-block]
    '("Run layout block" . hsc3-run-layout-block))
  (define-key map [menu-bar hsc3 expression run-consecutive-lines]
    '("Run consecutive lines" . hsc3-run-consecutive-lines))
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
    '("Quit haskell" . hsc3-quit-haskell))
  (define-key map [menu-bar hsc3 haskell interrupt-haskell]
    '("Interrupt haskell" . hsc3-interrupt-haskell))
  (define-key map [menu-bar hsc3 haskell see-haskell]
    '("See haskell" . hsc3-see-haskell)))

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
