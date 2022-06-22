;;; hsc3.el --- Haskell SuperCollider

;;; Commentary:

; Indentation and font locking is courtesy `haskell' mode (debian=haskell-mode).
; Inter-process communication is courtesy `comint'.
; Symbol at point acquisition is courtesy `thingatpt'.
; Directory search facilities are courtesy `find-lisp'.

(require 'haskell)
(require 'comint)
(require 'thingatpt)
(require 'find-lisp)

;;; Code:

(defcustom hsc3-buffer "*hsc3*"
  "The name of the hsc3 haskell process buffer."
  :type 'string)

(defcustom hsc3-interpreter (list "ghci")
  "The name of the haskell interpreter (default=\"ghci\")."
  :type '(repeat string))

(defvar hsc3-directory nil
  "The hsc3 directory (default=nil).")

(defvar sc3-help-directory nil
  "The directory containing the Sc3 RTF help files (default=nil).

These can be fetched from the SuperCollider archives by typing:

git checkout 2495f4f61c0955001169d28142543aabbe1bedcb build/old_rtf_help.tar.gz")

(defcustom hsc3-auto-import-modules nil
  "Flag to decide if the hsc3 standard module set is imported on startup (default=nil)."
  :type 'boolean)

(defun hsc3-chunk-string (n s)
  "Split the string S into chunks of N characters."
  (let* ((l (length s))
         (m (min l n))
         (c (substring s 0 m)))
    (if (<= l n)
        (list c)
      (cons c (hsc3-chunk-string n (substring s n))))))

(defun hsc3-send-line (s)
  "Send the string S, with newline appended, to haskell."
  (if (comint-check-proc hsc3-buffer)
      (let ((cs (hsc3-chunk-string 64 (concat s "\n"))))
        (mapcar
         (lambda (c) (comint-send-string hsc3-buffer c))
         cs))
    (error "No hsc3 process?")))

(defun hsc3-send-layout-block (s)
  "Send the string S to haskell using ghci layout block notation."
  (hsc3-send-line (mapconcat 'identity (list ":{" s ":}") "\n")))

(defun hsc3-send-text (str)
  "If the string STR spans multiple lines `hsc3-send-layout-block' else `hsc3-send-line'."
  (if (string-match "\n" str)
      (hsc3-send-layout-block str)
    (hsc3-send-line str)))

(defun hsc3-send-text-fn (fn str)
  "Send string STR with haskell function FN applied using the $ operator.
If STR has a newline the layout is adjusted accordingly."
  (hsc3-send-text (if (string-match "\n" str) (concat fn " $\n" str) (concat fn " $ " str))))

(defun hsc3-send-quit ()
  "Send :quit instruction to haskell."
  (interactive)
  (hsc3-send-line ":quit"))

(defun hsc3-uncomment (s)
  "Remove initial comment and Bird-literate markers from the string S if present."
   (replace-regexp-in-string "^[- ]*[> ]*" "" s))

(defun hsc3-find-files (dir rgx)
  "Find files at DIR matching RGX."
  (mapc (lambda (filename)
          (find-file-other-window filename))
        (find-lisp-find-files dir rgx)))

(defun hsc3-help ()
  "Lookup up the name at point in the hsc3 help files."
  (interactive)
  (let ((rgx (concat "^" (thing-at-point 'symbol) "\\.help\\.l?hs$")))
    (hsc3-find-files (concat hsc3-directory "Help/") rgx)))

(defun hsc3-sc3-help ()
  "Lookup up the name at point in the Sc3 (RTF) help files."
  (interactive)
  (let ((rgx (concat "^" (thing-at-point 'symbol) "\\\(.help\\\)?.rtf$")))
    (hsc3-find-files sc3-help-directory rgx)))

(defun hsc3-sc3-help-scdoc ()
  "Lookup up the Ugen name at point in the Sc3 (SCDOC) help files."
  (interactive)
  (hsc3-send-line
   (format
    "Sound.Sc3.Common.Help.sc3_scdoc_help_open False (Sound.Sc3.Common.Help.sc3_scdoc_help_path (Sound.Sc3.Ugen.DB.ugen_sc3_name \"%s\"))"
    (thing-at-point 'symbol))))

(defun hsc3-ugen-summary ()
  "Lookup up the Ugen at point in hsc3-db."
  (interactive)
  (hsc3-send-line
   (format "Sound.Sc3.Ugen.DB.PP.ugen_summary_wr \"%s\"" (thing-at-point 'symbol))))

(defun hsc3-ugen-default-param ()
  "Insert the default Ugen parameters (arguments) for the Ugen before <point>."
  (interactive)
  (let ((p (format "hsc3-help ugen exemplar hs %s" (thing-at-point 'symbol))))
    (insert " ")
    (insert (hsc3-remove-trailing-newline (shell-command-to-string p)))))

(defun hsc3-ugen-control-param ()
  "Insert control Ugen parameters (arguments) for the Ugen before <point>."
  (interactive)
  (let ((p (format "hsc3-help ugen exemplar hs/control %s" (thing-at-point 'symbol))))
    (insert (hsc3-remove-trailing-newline (shell-command-to-string p)))))

(defun hsc3-ugen-control-param-let ()
  "Insert control Ugen parameters (arguments) for the Ugen before <point>."
  (interactive)
  (let ((p (format "hsc3-help ugen exemplar hs/control/let %s" (thing-at-point 'symbol))))
    (insert (hsc3-remove-trailing-newline (shell-command-to-string p)))))

(defun hsc3-remove-trailing-newline (str)
  "Delete trailing newlines from STR."
  (replace-regexp-in-string "\n\\'" "" str))

(defun hsc3-cd ()
  "Change directory at ghci to current value of 'default-directory'."
  (interactive)
  (hsc3-send-line (format ":cd %s" default-directory)))

(defun hsc3-load-current-file ()
  "Send :load and the current buffer file name to haskell."
  (interactive)
  (save-buffer)
  (hsc3-see-haskell)
  (hsc3-send-line (format ":load \"%s\"" buffer-file-name)))

(defun hsc3-current-line ()
  "Get current line."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun hsc3-send-current-line ()
  "Send the current line to haskell."
  (interactive)
  (hsc3-send-line (hsc3-uncomment (hsc3-current-line))))

(defun hsc3-send-main ()
  "Send main to haskell."
  (interactive)
  (hsc3-send-line "main"))

(defun hsc3-region-string ()
  "Get current region as string."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun hsc3-send-region ()
  "Send region text to interpreter."
  (interactive)
  (hsc3-send-text (hsc3-region-string)))

(defun hsc3-send-region-fn (fn)
  "Send region text with haskell function FN to be applied."
  (hsc3-send-text-fn fn (hsc3-region-string)))

(defun hsc3-play-region ()
  "Play region at scsynth."
  (interactive)
  (hsc3-send-region-fn "Sound.Sc3.scsynthPlay scsynth"))

(defun hsc3-play-region-at (k)
  "Play region at scsynth.  The (one-indexed) prefix agument K indicates which server to send to."
  (interactive "p")
  (hsc3-send-region-fn
   (format
    "Sound.Sc3.auditionAt (\"%s\",%d + %d) Sound.Sc3.def_play_opt"
    hsc3-server-host hsc3-server-port (- k 1))))

(defcustom hsc3-draw-command "draw"
  "*The un-qualified name of the draw function to use at `hsc3-draw-region'."
  :type 'string)

(defun hsc3-draw-region ()
  "Draw region Ugen graph."
  (interactive)
  (hsc3-send-region-fn (format "Sound.Sc3.Ugen.Dot.%s $ wrapOut Nothing " hsc3-draw-command)))

(defun hsc3-dump-ugens-region ()
  "Print region Ugen graph."
  (interactive)
  (hsc3-send-region-fn "Sound.Sc3.ugen_dump_ugens $ wrapOut Nothing"))

(defun hsc3-ui-region ()
  "UI for region Ugen graph."
  (interactive)
  (let ((str (hsc3-region-string)))
    (hsc3-send-region-fn "Sound.Sc3.UI.SCLang.Control.ugen_ui_run \"ui\" 1")))

(defun hsc3-ui-comment ()
  "UI for description in comment line."
  (interactive)
  (hsc3-send-line
   (format
    "Sound.Sc3.UI.SCLang.Control.control_text_ui_run \"ui\" 1 \"%s\""
    (hsc3-current-line))))

(defun hsc3-pp-html-region ()
  "HTML pretty-printer and viewer for region Ugen graph."
  (interactive)
  (let ((str (hsc3-region-string)))
    (hsc3-send-region-fn "Sound.Sc3.UI.HTML.ugen_graph_pp_html_wv")))

(defun hsc3-pp-forth ()
  "Forth PP."
  (interactive)
  (hsc3-send-region-fn "Sound.Sc3.Ugen.DB.PP.ugen_graph_forth_pp (False,False)"))

(defun hsc3-pp-smalltalk ()
  "Pretty print Ugen as Smalltalk."
  (interactive)
  (hsc3-send-region-fn "Sound.Sc3.Ugen.DB.PP.Graph.ugen_graph_smalltalk_pp"))

(defun hsc3-id-rewrite-region ()
  "Run hsc3-id-rewrite on region."
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "hsc3-rw id-rewrite" nil t))

(defun hsc3-id-rewrite-buffer ()
  "Run hsc3-id-rewrite on buffer."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "hsc3-rw id-rewrite" nil t))

(defun hsc3-uparam-rewrite ()
  "Rewrite uparam notation inplace."
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "hsc3-rw uparam expand" nil t))

(defcustom hsc3-server-host "127.0.0.1"
  "The host that scsynth is listening at."
  :type 'string)

(defcustom hsc3-server-port 57110
  "The port that scsynth is listening at."
  :type 'integer)

(defun hsc3-with-sc3 (txt)
  "Sound.Sc3.withSc3 at `hsc3-server-host' and `hsc3-server-port' of the string TXT."
  (hsc3-send-line
   (format "Sound.Sc3.withSc3At (\"%s\",%d) %s" hsc3-server-host hsc3-server-port txt)))

(defun hsc3-reset-scsynth ()
  "Send Sc3 reset instruction to haskell."
  (interactive)
  (hsc3-send-line "Sound.Sc3.scsynthReset scsynth"))

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
    (hsc3-set-prompt)
    (if hsc3-auto-import-modules
        (progn
          (hsc3-import-standard-modules)
          (hsc3-init-scsynth)))
    (hsc3-see-haskell)))

(defun hsc3-interrupt-haskell ()
  "Interupt haskell."
  (interactive)
  (interrupt-process hsc3-buffer comint-ptyp))

(defun hsc3-stop ()
  "Interrupt haskell & reset scsynth."
  (interactive)
  (progn
    (hsc3-interrupt-haskell)
    (sleep-for 0.15)
    (hsc3-reset-scsynth)))

(defun hsc3-server-status ()
  "Send serverStatus request to haskell."
  (interactive)
  (hsc3-with-sc3 "Sound.Sc3.serverStatus >>= mapM putStrLn"))

(defun hsc3-quit-scsynth ()
  "Quit."
  (interactive)
  (hsc3-with-sc3 "(Sound.OSC.sendMessage Sound.Sc3.quit)"))

(defcustom hsc3-seq-degree 2
  "Number of scsynth processes to address at -seq operations (default=2)."
  :type 'integer)

(defun hsc3-server-status-seq ()
  "Send serverStatus request to haskell."
  (interactive)
  (hsc3-send-line
   (format
    "Sound.Sc3.withSc3AtSeq (\"%s\",%d) %d Sound.Sc3.serverStatus >>= mapM putStrLn . concat"
    hsc3-server-host hsc3-server-port hsc3-seq-degree)))

(defun hsc3-play-region-seq ()
  "Sound.Sc3.auditionAtSeq of `hsc3-seq-degree'."
  (interactive)
  (hsc3-send-region-fn
   (format
    "Sound.Sc3.auditionAtSeq (\"%s\",%d) def_play_opt %d"
    hsc3-server-host hsc3-server-port hsc3-seq-degree)))

(defun hsc3-reset-scsynth-seq ()
  "Send Sc3 reset instruction to haskell."
  (interactive)
  (hsc3-send-line
   (format
    "Sound.Sc3.withSc3AtSeq_ (\"%s\",%d) %d Sound.Sc3.reset"
    hsc3-server-host hsc3-server-port hsc3-seq-degree)))

(defun hsc3-dmenu-ugen-core ()
  "Run dmenu to select a core Sc3 Ugen."
  (interactive)
  (insert (shell-command-to-string "hsc3-db dmenu ugen core")))

(defun hsc3-dmenu-ugen-ext ()
  "Run dmenu to select an external Sc3 Ugen."
  (interactive)
  (insert (shell-command-to-string "hsc3-db dmenu ugen external")))

(defun hsc3-dmenu-ugen-all ()
  "Run dmenu to select an Sc3 Ugen."
  (interactive)
  (insert (shell-command-to-string "hsc3-db dmenu ugen all")))

(defun hsc3-xmenu-ugen-core ()
  "Run xmenu to select a core Sc3 Ugen."
  (interactive)
  (insert (shell-command-to-string "hsc3-db xmenu core")))

(defun hsc3-xmenu-ugen-ext ()
  "Run xmenu to select an external Sc3 Ugen."
  (interactive)
  (insert (shell-command-to-string "hsc3-db xmenu external")))

(defun hsc3-xmenu-ugen-all ()
  "Run xmenu to select an Sc3 Ugen."
  (interactive)
  (insert
   (shell-command-to-string
    "cat ~/sw/hsc3-db/lib/xmenu/ugen-core-tree.text \
         ~/sw/hsc3-db/lib/xmenu/nil.text \
         ~/sw/hsc3-db/lib/xmenu/ugen-ext-tree.text | xmenu")))

(defun hsc3-load-file (fn)
  "Load file FN as string."
  (with-temp-buffer
    (insert-file-contents fn)
    (buffer-substring-no-properties
       (point-min)
       (point-max))))

(defun hsc3-unload-all-modules ()
  "Unload all modules and delete all bindings at ghci.

   :load without any arguments unloads all the currently loaded modules and bindings"
  (interactive)
  (hsc3-send-line ":load"))

(defun hsc3-import-standard-modules ()
  "Send standard set of hsc3 and related module imports to haskell."
  (interactive)
  (mapc
   'hsc3-send-line
   (split-string (hsc3-load-file (concat hsc3-directory "lib/hsc3-std-imports.hs")) "\n")))

(defun hsc3-init-scsynth ()
  "Initialise the scsynth interpreter variable."
  (interactive)
  (hsc3-send-line "scsynth <- Sound.Sc3.Server.Scsynth.newScsynth"))

(defun hsc3-set-prompt ()
  "Set ghci prompt to hsc3> and the continuation prompt to nil."
  (interactive)
  (hsc3-send-line ":set prompt \"hsc3> \"")
  (hsc3-send-line ":set prompt-cont \"\""))

(defun hsc3-see-haskell ()
 "Show haskell output."
 (interactive)
  (if (not (comint-check-proc hsc3-buffer))
      (hsc3-start-haskell)
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
  "Add Haskell SuperCollider keybindings to MAP."
  (define-key map (kbd "C-c <") 'hsc3-load-current-file)
  (define-key map (kbd "C-c >") 'hsc3-see-haskell)
  (define-key map (kbd "C-c C-c") 'hsc3-send-current-line)
  (define-key map (kbd "C-c C-h") 'hsc3-help)
  (define-key map (kbd "C-c C-r") 'hsc3-send-region)
  (define-key map (kbd "C-c C-a") 'hsc3-play-region)
  (define-key map (kbd "C-c C-S-a") 'hsc3-play-region-seq)
  (define-key map (kbd "C-c C-g") 'hsc3-draw-region)
  (define-key map (kbd "C-c C-d") 'hsc3-dump-ugens-region)
  (define-key map (kbd "C-c C-v") 'hsc3-ui-region)
  (define-key map (kbd "C-c C-f") 'hsc3-ui-comment)
  (define-key map (kbd "C-c C-j") 'hsc3-sc3-help)
  (define-key map (kbd "C-c C-i") 'hsc3-interrupt-haskell)
  (define-key map (kbd "C-c C-k") 'hsc3-reset-scsynth)
  (define-key map (kbd "C-c C-S-k") 'hsc3-reset-scsynth-seq)
  (define-key map (kbd "C-c C-m") 'hsc3-send-main)
  (define-key map (kbd "C-c C-p") 'hsc3-server-status)
  (define-key map (kbd "C-c C-S-p") 'hsc3-server-status-seq)
  (define-key map (kbd "C-c C-q") 'hsc3-send-quit)
  (define-key map (kbd "C-c C-.") 'hsc3-stop)
  (define-key map (kbd "C-c C-s") 'hsc3-dmenu-ugen-all)
  (define-key map (kbd "C-c C-S-s") 'hsc3-xmenu-ugen-all)
  (define-key map (kbd "C-c C-u") 'hsc3-ugen-summary))

(defun hsc3-mode-menu (map)
  "Add Haskell SuperCollider Menu to MAP."
  (define-key map [menu-bar hsc3] (cons "Haskell-SuperCollider" (make-sparse-keymap "Haskell-SuperCollider")))
  (define-key map [menu-bar hsc3 help] (cons "Help" (make-sparse-keymap "Help")))
  (define-key map [menu-bar hsc3 help hsc3] '("HSc3 Help" . hsc3-help))
  (define-key map [menu-bar hsc3 help ugen] '("Ugen Summary" . hsc3-ugen-summary))
  (define-key map [menu-bar hsc3 help sc3-ugen] '("Sc3 Help" . hsc3-sc3-ugen-help))
  (define-key map [menu-bar hsc3 expression] (cons "Expression" (make-sparse-keymap "Expression")))
  (define-key map [menu-bar hsc3 expression stop] '("Stop (interrupt and reset)" . hsc3-stop))
  (define-key map [menu-bar hsc3 expression change-directory] '("Change directory" . hsc3-cd))
  (define-key map [menu-bar hsc3 expression import-standard-modules] '("Import standard modules" . hsc3-import-standard-modules))
  (define-key map [menu-bar hsc3 expression server-status] '("Print server status" . hsc3-server-status))
  (define-key map [menu-bar hsc3 expression send-main] '("Send main" . hsc3-send-main))
  (define-key map [menu-bar hsc3 expression send-region] '("Send region" . hsc3-send-region))
  (define-key map [menu-bar hsc3 expression send-current-line] '("Send current line" . hsc3-send-current-line))
  (define-key map [menu-bar hsc3 expression id-rewrite-region] '("ID-rewrite region" . hsc3-id-rewrite-region))
  (define-key map [menu-bar hsc3 expression load-current-file] '("Load current file" . hsc3-load-current-file))
  (define-key map [menu-bar hsc3 expression dmenu-ugen] '("Ugen dmenu" . hsc3-dmenu-ugen-all))
  (define-key map [menu-bar hsc3 expression xmenu-ugen] '("Ugen xmenu" . hsc3-xmenu-ugen-all))
  (define-key map [menu-bar hsc3 expression draw-region] '("Draw region" . hsc3-draw-region))
  (define-key map [menu-bar hsc3 expression play-region] '("Play region" . hsc3-play-region))
  (define-key map [menu-bar hsc3 scsynth] (cons "ScSynth" (make-sparse-keymap "ScSynth")))
  (define-key map [menu-bar hsc3 scsynth quit] '("Quit scsynth" . hsc3-quit-scsynth))
  (define-key map [menu-bar hsc3 scsynth status] '("Display status" . hsc3-status-scsynth))
  (define-key map [menu-bar hsc3 scsynth reset] '("Reset scsynth" . hsc3-reset-scsynth))
  (define-key map [menu-bar hsc3 haskell] (cons "Haskell" (make-sparse-keymap "Haskell")))
  (define-key map [menu-bar hsc3 haskell quit-haskell] '("Quit haskell" . hsc3-quit-haskell))
  (define-key map [menu-bar hsc3 haskell interrupt-haskell] '("Interrupt haskell" . hsc3-interrupt-haskell))
  (define-key map [menu-bar hsc3 haskell see-haskell] '("See haskell" . hsc3-see-haskell)))

(if hsc3-mode-map
    ()
  (let ((map (make-sparse-keymap "Haskell-SuperCollider")))
    (hsc3-mode-keybindings map)
    (hsc3-mode-menu map)
    (setq hsc3-mode-map map)))

(define-derived-mode
  hsc3-mode
  haskell-mode
  "Haskell SuperCollider"
  "Major mode for interacting with an inferior hsc3 process."
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.hs$" . hsc3-mode))

(provide 'hsc3)

;;; hsc3.el ends here
