;; hsc.el - (c) rohan drape, 2006

;; This mode is implemented as a derivation of `haskell' mode,
;; indentation and font locking is courtesy that mode.  The
;; inter-process communication is courtesy `comint'.  The symbol at
;; point acquisition is courtesy `thingatpt'.  The directory search
;; facilities are courtesy `find-lisp'.

(require 'scheme)
(require 'comint)
(require 'thingatpt)
(require 'find-lisp)

(defvar hsc-buffer
  "*hsc*"
  "*The name of the hsc process buffer (default=*hsc*).")

(defvar hsc-interpreter
  "ghci"
  "*The haskell interpter to use (default=ghci).")

(defvar hsc-interpreter-arguments
  (list)
  "*Arguments to the haskell interpreter (default=none).")

(defvar hsc-help-directory
  nil
  "*The directory containing the help files (default=nil).")

(defun hsc-start-haskell ()
  "Start haskell."
  (interactive)
  (if (comint-check-proc hsc-buffer)
      (error "An hsc process is already running")
    (apply
     'make-comint
     "hsc"
     hsc-interpreter
     nil
     hsc-interpreter-arguments)
    (hsc-see-output)))

(defun hsc-see-output ()
  "Show haskell output."
  (interactive)
  (when (comint-check-proc hsc-buffer)
    (delete-other-windows)
    (split-window-vertically)
    (with-current-buffer hsc-buffer
      (let ((window (display-buffer (current-buffer))))
	(goto-char (point-max))
	(save-selected-window
	  (set-window-point window (point-max)))))))

(defun hsc-quit-haskell ()
  "Quit haskell."
  (interactive)
  (kill-buffer hsc-buffer)
  (delete-other-windows))

(defun hsc-help ()
  "Lookup up the name at point in the Help files."
  (interactive)
  (mapc (lambda (filename)
	  (find-file-other-window filename))
	(find-lisp-find-files hsc-help-directory
			      (concat "^"
				      (thing-at-point 'symbol)
				      "\\.help\\.lhs"))))

(defun hsc-send-string (s)
  (if (comint-check-proc hsc-buffer)
      (comint-send-string hsc-buffer (concat s "\n"))
    (error "no hsc process running?")))

(defun hsc-transform-and-store (f s)
  "Transform example text into compilable form."
  (with-temp-file f
    (insert "> import Sound.OpenSoundControl\n")
    (insert "> import Sound.SC3\n")
    (insert "> import Control.Monad\n")
    (insert "> import System.Random\n")
    (insert "> main = do\n")
    (insert s)))

(defun hsc-run-region ()
  "Place the region in a do block and compile."
  (interactive)
  (hsc-transform-and-store
   "/tmp/hsc.lhs"
   (buffer-substring-no-properties (region-beginning) (region-end)))
  (hsc-send-string ":l \"/tmp/hsc.lhs\"")
  (hsc-send-string "main"))

(defun hsc-reset-scsynth ()
  "Reset"
  (interactive)
  (hsc-send-string "withSC3 reset"))

(defun hsc-status-scsynth ()
  "Status"
  (interactive)
  (hsc-send-string "withSC3 serverStatus >>= mapM putStrLn"))

(defun hsc-quit-scsynth ()
  "Quit"
  (interactive)
  (hsc-send-string "withSC3 (\fd -> send fd quit)"))

(defvar hsc-mode-map nil
  "hsc keymap.")

(defun hsc-mode-keybindings (map)
  "hsc keybindings."
  (define-key map "\C-c\C-s" 'hsc-start-haskell)
  (define-key map "\C-c\C-g" 'hsc-see-output)
  (define-key map "\C-c\C-x" 'hsc-quit-haskell)
  (define-key map "\C-c\C-k" 'hsc-reset-scsynth)
  (define-key map "\C-c\C-w" 'hsc-status-scsynth)
  (define-key map "\C-c\C-e" 'hsc-run-region)
  (define-key map "\C-c\C-o" 'hsc-quit-scsynth)
  (define-key map "\C-c\C-h" 'hsc-help))

(defun hsc-mode-menu (map)
  "hsc menu."
  (define-key map [menu-bar hsc]
    (cons "Hsc" (make-sparse-keymap "Hsc")))
  (define-key map [menu-bar hsc help]
    (cons "Help" (make-sparse-keymap "Help")))
  (define-key map [menu-bar hsc help hsc]
    '("Hsc help" . hsc-help))
  (define-key map [menu-bar hsc expression]
    (cons "Expression" (make-sparse-keymap "Expression")))
  (define-key map [menu-bar hsc expression run]
    '("Run region" . hsc-run-region))
  (define-key map [menu-bar hsc scsynth]
    (cons "SCSynth" (make-sparse-keymap "SCSynth")))
  (define-key map [menu-bar hsc scsynth quit]
    '("Quit scsynth" . hsc-quit-scsynth))
  (define-key map [menu-bar hsc scsynth status]
    '("Display status" . hsc-status-scsynth))
  (define-key map [menu-bar hsc scsynth reset]
    '("Reset scsynth" . hsc-reset-scsynth))
  (define-key map [menu-bar hsc haskell]
    (cons "Haskell" (make-sparse-keymap "Haskell")))
  (define-key map [menu-bar hsc haskell quit-haskell]
    '("Quit haskell" . hsc-quit-haskell))
  (define-key map [menu-bar hsc haskell see-output]
    '("See output" . hsc-see-output))
  (define-key map [menu-bar hsc haskell start-haskell]
    '("Start haskell" . hsc-start-haskell)))

(if hsc-mode-map
    ()
  (let ((map (make-sparse-keymap "hsc")))
    (hsc-mode-keybindings map)
    (hsc-mode-menu map)
    (setq hsc-mode-map map)))

(define-derived-mode
  hsc-mode
  literate-haskell-mode
  "hsc-mode"
  "Major mode for interacting with an inferior haskell process."
  (turn-on-haskell-font-lock))

(add-to-list 'auto-mode-alist '("\\.lhs$" . hsc-mode))

(provide 'hsc)
