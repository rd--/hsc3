;; Indentation and font locking is courtesy `haskell' mode.
;; Inter-process communication is courtesy `comint'.
;; Symbol at point acquisition is courtesy `thingatpt'.
;; Directory search facilities are courtesy `find-lisp'.

(require 'haskell)
(require 'comint)
(require 'thingatpt)
(require 'find-lisp)

(defvar hsc3-buffer
  "*hsc3*"
  "*The name of the hsc3 haskell process buffer.")

(defvar hsc3-interpreter
  (list "ghci")
  "*The name of the haskell interpreter to run (default=\"ghci\").")

(defvar hsc3-help-directory
  nil
  "*The directory containing the help files (default=nil).")

(defvar sc3-help-directory
  nil
  "*The directory containing the SC3 RTF help files (default=nil).")

(defvar hsc3-literate-p
  t
  "*Flag to indicate if we are in literate mode (default=t).")

(make-variable-buffer-local 'hsc3-literate-p)

(defun hsc3-chunk-string (n s)
  "Split a string into chunks of 'n' characters."
  (let* ((l (length s))
         (m (min l n))
         (c (substring s 0 m)))
    (if (<= l n)
        (list c)
      (cons c (hsc3-chunk-string n (substring s n))))))

(defun hsc3-send-string (s)
  (if (comint-check-proc hsc3-buffer)
      (let ((cs (hsc3-chunk-string 64 (concat s "\n"))))
        (mapcar
         (lambda (c) (comint-send-string hsc3-buffer c))
         cs))
    (error "no hsc3 process running?")))

(defun hsc3-send-layout-block (s)
  (hsc3-send-string (mapconcat 'identity (list ":{" s ":}") "\n")))

(defun hsc3-quit-haskell ()
  "Quit haskell."
  (interactive)
  (hsc3-send-string ":quit"))

(defun hsc3-unlit (s)
  "Remove bird literate marks and preceding comment marker"
   (replace-regexp-in-string "^[> ]* ?" "" s))

(defun hsc3-uncomment (s)
  "Remove initial comment and Bird-literate markers if present."
   (replace-regexp-in-string "^[- ]*[> ]*" "" s))

(defun hsc3-find-files (dir rgx)
  (mapc (lambda (filename)
          (find-file-other-window filename))
        (find-lisp-find-files dir rgx)))

(defun hsc3-help ()
  "Lookup up the name at point in the hsc3 help files."
  (interactive)
  (let ((rgx (concat "^" (thing-at-point 'symbol) "\\.help\\.l?hs$")))
    (hsc3-find-files hsc3-help-directory rgx)))

(defun hsc3-sc3-help ()
  "Lookup up the name at point in the SC3 (RTF) help files."
  (interactive)
  (let ((rgx (concat "^" (thing-at-point 'symbol) "\\\(.help\\\)?.rtf$")))
    (hsc3-find-files sc3-help-directory rgx)))

(defun hsc3-ugen-summary ()
  "Lookup up the UGen at point in hsc3-db"
  (interactive)
  (hsc3-send-string
      (format "Sound.SC3.UGen.DB.ugen_summary_wr \"%s\""
              (thing-at-point 'symbol))))

(defun hsc3-remove-trailing-newline (s)
  (replace-regexp-in-string "\n\\'" "" s))

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

(defun hsc3-run-line ()
  "Send the current line to the interpreter."
  (interactive)
  (let* ((s (buffer-substring-no-properties
             (line-beginning-position)
             (line-end-position)))
	 (s* (if hsc3-literate-p
		 (hsc3-unlit s)
	       (hsc3-uncomment s))))
    (hsc3-send-string s*)))

(defun hsc3-run-main ()
  "Run current main."
  (interactive)
  (hsc3-send-string "main"))

(defun hsc3-run-layout-block ()
  "Send region with ghci layout quoting"
  (interactive)
  (hsc3-send-layout-block
   (buffer-substring-no-properties (region-beginning) (region-end))))

(defun hsc3-audition-layout-block ()
  "Play region with ghci layout quoting"
  (interactive)
  (hsc3-send-layout-block
   (concat "audition $\n" (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun hsc3-draw-layout-block ()
  "Draw region with ghci layout quoting"
  (interactive)
  (hsc3-send-layout-block
   (concat
    "Sound.SC3.UGen.Dot.draw $\n"
    (buffer-substring-no-properties (region-beginning) (region-end)))))

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

(defun hsc3-reset-scsynth ()
  "Reset scsynth"
  (interactive)
  (hsc3-send-string "Sound.SC3.withSC3 Sound.SC3.reset"))

(defun hsc3-start-haskell ()
  "Start the hsc3 haskell process.

If `hsc3-interpreter' is not already running as a subprocess it is
started and a new window is created to display the results of
evaluating hsc3 expressions.  Input and output is via `hsc3-buffer'."
  (interactive)
  (if (comint-check-proc hsc3-buffer)
      (hsc3-see-haskell)
    (apply
     'make-comint
     "hsc3"
     (car hsc3-interpreter)
     nil
     (cdr hsc3-interpreter))
    (hsc3-see-haskell)))

(defun hsc3-interrupt-haskell ()
  "Interupt haskell process."
  (interactive)
  (interrupt-process hsc3-buffer comint-ptyp))

(defun hsc3-stop ()
  "Interrupt haskell interpreter & reset scsynth"
  (interactive)
  (progn
    (hsc3-interrupt-haskell)
    (sleep-for 0.15)
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
  (if (and (executable-find "hasktags") (file-exists-p "hsc3.cabal"))
      (call-process-shell-command
       "find Sound . -name '*.hs' | xargs hasktags -e"
       nil
       nil)
    (error "no hasktags binary or not at hsc3 directory?")))

(defun hsc3-audition-graph ()
  "Audition the UGen graph at point."
  (interactive)
  (hsc3-send-string
   (concat "Sound.SC3.audition " (thing-at-point 'symbol))))

(defun hsc3-audition-graph-m ()
  "Audition the (monadic) UGen graph at point."
  (interactive)
  (hsc3-send-string
   (concat "Sound.SC3.audition =<<" (thing-at-point 'symbol))))

(defun hsc3-audition-pattern ()
  "Audition the pattern at point."
  (interactive)
  (hsc3-send-string
   (concat "Sound.SC3.Lang.Pattern.paudition " (thing-at-point 'symbol))))

(defun hsc3-draw-graph ()
  "Draw the UGen graph at point."
  (interactive)
  (hsc3-send-string
   (concat "Sound.SC3.UGen.Dot.draw " (thing-at-point 'symbol))))

(defun hsc3-draw-graph-plain ()
  "Draw the UGen graph at point (plain)."
  (interactive)
  (hsc3-send-string
   (concat "Sound.SC3.UGen.Dot.draw_plain " (thing-at-point 'symbol))))

(defun hsc3-draw-graph-m ()
  "Draw the (monadic) UGen graph at point."
  (interactive)
  (hsc3-send-string
   (concat "Sound.SC3.UGen.Dot.draw =<<" (thing-at-point 'symbol))))

(defun hsc3-set-prompt ()
  "Set ghci prompt to hsc3."
  (interactive)
  (hsc3-send-string ":set prompt \"hsc3> \""))

(defun hsc3-see-haskell ()
 "Show haskell output."
 (interactive)
  (if (not (comint-check-proc hsc3-buffer))
      (hsc3-start-haskell)
   (hsc3-set-prompt)
   (delete-other-windows)
   (split-window-vertically)
   (with-current-buffer hsc3-buffer
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
  (define-key map [?\C-c ?\C-h] 'hsc3-help)
  (define-key map [?\C-c ?\C-a] 'hsc3-audition-graph)
  (define-key map [?\C-c ?\M-a] 'hsc3-audition-graph-m)
  (define-key map (kbd "C-c p") 'hsc3-audition-pattern)
  (define-key map (kbd "C-c C-g") 'hsc3-draw-graph)
  (define-key map (kbd "C-c C-S-g") 'hsc3-draw-graph-plain)
  (define-key map [?\C-c ?\M-g] 'hsc3-draw-graph-m)
  (define-key map [?\C-c ?\C-j] 'hsc3-sc3-help)
  ;(define-key map [?\C-c ?\C-/] 'hsc3-sc3-html-help)
  (define-key map [?\C-c ?i] 'hsc3-interrupt-haskell)
  (define-key map [?\C-c ?\C-k] 'hsc3-reset-scsynth)
  (define-key map [?\C-c ?\C-m] 'hsc3-run-main)
  (define-key map [?\C-c ?\C-p] 'hsc3-status-scsynth)
  (define-key map [?\C-c ?\C-q] 'hsc3-quit-haskell)
  (define-key map [?\C-c ?\C-0] 'hsc3-quit-scsynth)
  (define-key map [?\C-c ?\C-.] 'hsc3-stop)
  (define-key map [?\C-c ?\C-s] 'hsc3-stop) ; ie. sclang-mode key
  (define-key map [?\C-c ?\C-u] 'hsc3-ugen-summary))

(defun hsc3-mode-menu (map)
  "Haskell SuperCollider menu."
  (define-key map [menu-bar hsc3]
    (cons "Haskell-SuperCollider" (make-sparse-keymap "Haskell-SuperCollider")))
  (define-key map [menu-bar hsc3 help]
    (cons "Help" (make-sparse-keymap "Help")))
  (define-key map [menu-bar hsc3 help hsc3]
    '("HSC3 Help" . hsc3-help))
  (define-key map [menu-bar hsc3 help ugen]
    '("UGen Summary" . hsc3-ugen-summary))
  ;(define-key map [menu-bar hsc3 help sc3-server]
  ;  '("SuperCollider Server Command help" . hsc3-sc3-server-help))
  (define-key map [menu-bar hsc3 help sc3-ugen]
    '("SC3 Help" . hsc3-sc3-ugen-help))
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
  "Major mode for interacting with an inferior hsc3 process."
  (setq hsc3-literate-p nil)
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.hs$" . hsc3-mode))

(provide 'hsc3)
