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
  "*The name of the haskell interpreter (default=\"ghci\").")

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
    (error "no hsc3 process?")))

(defun hsc3-send-layout-block (s)
  (hsc3-send-string (mapconcat 'identity (list ":{" s ":}") "\n")))

(defun hsc3-send-quit ()
  "Send :quit to haskell."
  (interactive)
  (hsc3-send-string ":quit"))

(defun hsc3-unlit (s)
  "Remove bird literate marks and preceding comment marker."
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
  "Lookup up the UGen at point in hsc3-db."
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

(defun hsc3-load-current-file ()
  "Send :load and the current buffer file name to the interpreter."
  (interactive)
  (save-buffer)
  (hsc3-see-haskell)
  (hsc3-send-string (format ":load \"%s\"" buffer-file-name)))

(defun hsc3-send-current-line ()
  "Send the current line to the interpreter."
  (interactive)
  (let* ((s (buffer-substring-no-properties
             (line-beginning-position)
             (line-end-position)))
	 (s* (if hsc3-literate-p
		 (hsc3-unlit s)
	       (hsc3-uncomment s))))
    (hsc3-send-string s*)))

(defun hsc3-send-main ()
  "Send :main to the inerpreter."
  (interactive)
  (hsc3-send-string "main"))

(defun hsc3-region-string ()
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun hsc3-audition-region ()
  "Play region, if region spans multiple lines send using using ghci layout quoting"
  (interactive)
  (let ((str (hsc3-region-string)))
    (if (string-match "\n" str)
        (hsc3-send-layout-block (concat "audition $\n" str))
      (hsc3-send-string (concat "audition $ " str)))))

(defun hsc3-draw-region ()
  "Draw region, if region spans multiple lines send using using ghci layout quoting"
  (interactive)
  (let ((str (hsc3-region-string)))
    (if (string-match "\n" str)
        (hsc3-send-layout-block (concat "Sound.SC3.UGen.Dot.draw $\n" str))
    (hsc3-send-string (concat "Sound.SC3.UGen.Dot.draw $ " str)))))

(defun hsc3-id-rewrite-region ()
  "Run hsc3-id-rewrite on region."
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

If `hsc3-interpreter' is not already a subprocess it is
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

(defun hsc3-ugen-menu-core ()
  "xmenu of categorised core SC3 UGens"
  (interactive)
  (insert (shell-command-to-string "xmenu < ~/sw/hsc3-db/lib/xmenu/ugen-core-tree.text")))

(defun hsc3-ugen-menu-ext ()
  "xmenu of categorised external SC3 UGens"
  (interactive)
  (insert (shell-command-to-string "xmenu < ~/sw/hsc3-db/lib/xmenu/ugen-ext-tree.text")))

(defun hsc3-ugen-menu ()
  "xmenu of categorised core and external SC3 UGens"
  (interactive)
  (insert (shell-command-to-string "cat ~/sw/hsc3-db/lib/xmenu/ugen-core-tree.text ~/sw/hsc3-db/lib/xmenu/nil.text ~/sw/hsc3-db/lib/xmenu/ugen-ext-tree.text | xmenu")))

(defun hsc3-import-standard-modules ()
  "Import standard set of hsc3 and related modules"
  (interactive)
  (mapc 'hsc3-send-string
       (list "import Data.Bits {- base -}"
             "import Data.List {- base -}"
             "import System.Random {- random -}"
             "import Sound.SC3 {- hsc3 -}"
             "import qualified Sound.SC3.Common.Buffer.Gen as Gen {- hsc3 -}"
             "import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}"
             "import qualified Sound.SC3.UGen.Bindings.Composite.External as X {- hsc3 -}"
             "import qualified Sound.SC3.UGen.Bindings.HW.External.Zita as X {- hsc3 -}"
             "import qualified Sound.SC3.UGen.Bindings.DB.RDU as X {- sc3-rdu -}"
             "import qualified Sound.SC3.UGen.Protect as Protect {- hsc3-rw -}")))

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
  (define-key map (kbd "C-c <") 'hsc3-load-current-file)
  (define-key map (kbd "C-c >") 'hsc3-see-haskell)
  (define-key map (kbd "C-c C-c") 'hsc3-send-current-line)
  (define-key map (kbd "C-c C-h") 'hsc3-help)
  (define-key map (kbd "C-c C-a") 'hsc3-audition-region)
  (define-key map (kbd "C-c C-g") 'hsc3-draw-region)
  (define-key map (kbd "C-c C-j") 'hsc3-sc3-help)
  (define-key map (kbd "C-c C-i") 'hsc3-interrupt-haskell)
  (define-key map (kbd "C-c C-k") 'hsc3-reset-scsynth)
  (define-key map (kbd "C-c C-m") 'hsc3-send-main)
  (define-key map (kbd "C-c C-p") 'hsc3-status-scsynth)
  (define-key map (kbd "C-c C-q") 'hsc3-send-quit)
  (define-key map (kbd "C-c C-.") 'hsc3-stop)
  (define-key map (kbd "C-c C-u") 'hsc3-ugen-summary))

(defun hsc3-mode-menu (map)
  "Haskell SuperCollider Menu"
  (define-key map [menu-bar hsc3]
    (cons "Haskell-SuperCollider" (make-sparse-keymap "Haskell-SuperCollider")))
  (define-key map [menu-bar hsc3 help]
    (cons "Help" (make-sparse-keymap "Help")))
  (define-key map [menu-bar hsc3 help hsc3]
    '("HSC3 Help" . hsc3-help))
  (define-key map [menu-bar hsc3 help ugen]
    '("UGen Summary" . hsc3-ugen-summary))
  (define-key map [menu-bar hsc3 help sc3-ugen]
    '("SC3 Help" . hsc3-sc3-ugen-help))
  (define-key map [menu-bar hsc3 expression]
    (cons "Expression" (make-sparse-keymap "Expression")))
  (define-key map [menu-bar hsc3 expression stop]
    '("Stop (interrupt and reset)" . hsc3-stop))
  (define-key map [menu-bar hsc3 expression change-directory]
    '("Change directory" . hsc3-cd))
  (define-key map [menu-bar hsc3 expression load-current-file]
    '("Load current file" . hsc3-load-current-file))
  (define-key map [menu-bar hsc3 expression send-main]
    '("Send main" . hsc3-send-main))
  (define-key map [menu-bar hsc3 expression send-current-line]
    '("Send current line" . hsc3-send-current-line))
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
