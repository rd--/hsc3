
;; (require 'haskell-interactive-mode)
;; (require 'haskell-process)

;; (defun hsc3-send-string (s)
;;   (haskell-process-send-string (haskell-interactive-process) s))

;; (defun hsc3-send-string-and-print (s)
;;   (haskell-process-show-repl-response s))

;; (defun hsc3-interrupt-haskell ()
;;   (haskell-process-interrupt))

;; (defun hsc3-see-haskell ()
;;   "Show haskell output."
;;   (interactive)
;;   (haskell-interactive-bring))

;; (require 'sclang)

;; (define-key map [?\C-c ?\C-e] 'hsc3-run-multiple-lines)
;; (define-key map [?\C-c ?\M-e] 'hsc3-run-multiple-lines-sclang)
;; (define-key map [?\C-c ?\C-r] 'hsc3-run-consecutive-lines)
;; (define-key map [?\C-c ?\C-f] 'hsc3-run-layout-block)
;; (define-key map [?\C-c ?\M-f] 'hsc3-sc3-forth-pp)
;; (define-key map [?\C-c ?\M-m] 'hsc3-load-main)
;; (define-key map [?\C-c ?\C-,] 'hsc3-ugen-default-param)

;; (define-key map [menu-bar hsc3 expression gen-default-param]
;;   '("Insert default parameters" . hsc3-ugen-default-param))
;; (define-key map [menu-bar hsc3 expression run-layout-block]
;;   '("Run layout block" . hsc3-run-layout-block))
;; (define-key map [menu-bar hsc3 expression run-consecutive-lines]
;;   '("Run consecutive lines" . hsc3-run-consecutive-lines))
;; (define-key map [menu-bar hsc3 expression run-multiple-lines]
;;   '("Run multiple lines" . hsc3-run-multiple-lines))

;; (defun hsc3-wait ()
;;   "Wait for prompt after sending command."
;;   (interactive)
;;   (inferior-haskell-wait-for-prompt (inferior-haskell-process)))

;; (defun hsc3-request-type ()
;;   "Ask ghci for the type of the name at point."
;;   (interactive)
;;   (hsc3-send-string (concat ":t " (thing-at-point 'symbol))))

;; (defun hsc3-load-main ()
;;   "Load current buffer and run main."
;;   (interactive)
;;   (hsc3-load-buffer)
;;   (hsc3-run-main))

;; (defun hsc3-ugen-default-param ()
;;   "Insert the default UGen parameters for the UGen before <point>."
;;   (interactive)
;;   (let ((p (format "hsc3-default-param %s" (thing-at-point 'symbol))))
;;     (insert " ")
;;     (insert (hsc3-remove-trailing-newline (shell-command-to-string p)))))

;; (defun hsc3-region-string ()
;;   "Get region as string (no properties)"
;;   (buffer-substring-no-properties
;;    (region-beginning)
;;    (region-end)))

;; (defun hsc3-gen-param ()
;;   "Rewrite an SC3 argument list as control definitions."
;;   (interactive)
;;   (hsc3-send-string
;;    (concat "putStrLn $ Sound.SC3.RW.PSynth.rewrite_param_list \"" (hsc3-region-string) "\"")))

;; (defun hsc3-local-dot ()
;;   "Copy '/tmp/hsc3.dot' to 'buffer-name' .dot."
;;   (interactive)
;;   (let ((nm (concat (file-name-sans-extension (buffer-name)) ".dot")))
;;     (copy-file "/tmp/hsc3.dot" nm t)))

;; (defun hsc3-concat (l)
;;   (apply #'concat l))

;; (defun hsc3-remove-non-literates (s)
;;   "Remove non-bird literate lines"
;;   (replace-regexp-in-string "^[^>]*$" "" s))

;; (defun hsc3-region-string-unlit ()
;;   "The current region (unlit, uncomment)."
;;   (let* ((s (hsc3-region-string)))
;;     (if hsc3-literate-p
;;         (hsc3-unlit (hsc3-remove-non-literates s))
;;       (hsc3-concat (mapcar 'hsc3-uncomment (split-string s "\n"))))))

;; (defun hsc3-region-string-one-line ()
;;   "Replace newlines with spaces in `hsc3-region-string'."
;;   (replace-regexp-in-string "\n" " " (hsc3-region-string-unlit)))

;; (defun hsc3-run-multiple-lines ()
;;   "Send the current region to the haskell interpreter as a single line."
;;   (interactive)
;;   (hsc3-send-string (hsc3-region-string-one-line)))

;; (defun hsc3-run-multiple-lines-sclang ()
;;   "Send the current region to the sclang interpreter as a single line."
;;   (interactive)
;;   (sclang-eval-string (hsc3-region-string-one-line) t))

;; (defun hsc3-run-consecutive-lines ()
;;   "Send the current region to the interpreter one line at a time."
;;   (interactive)
;;   (mapcar 'hsc3-send-string
;;           (split-string (hsc3-region-string) "\n")))

;; (define-key map [?\C-c ?\C-/] 'hsc3-sc3-html-help)

;;(defun hsc3-sc3-server-help ()
;;  "Lookup up the Server Command name at point in the SC3 help files."
;;  (interactive)
;;  (hsc3-send-string
;;   (format "Sound.SC3.Server.Help.viewServerHelp \"%s\""
;;           (thing-at-point 'symbol))))


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

;; (define-key map [?\C-c ?\M-a] 'hsc3-audition-graph-m)
(defun hsc3-audition-graph-m ()
  "Audition the (monadic) UGen graph at point."
  (interactive)
  (hsc3-send-string
   (concat "Sound.SC3.audition =<<" (thing-at-point 'symbol))))

;; (define-key map (kbd "C-c p") 'hsc3-audition-pattern)
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

;; (define-key map (kbd "C-c C-S-g") 'hsc3-draw-graph-plain)
(defun hsc3-draw-graph-plain ()
  "Draw the UGen graph at point (plain)."
  (interactive)
  (hsc3-send-string
   (concat "Sound.SC3.UGen.Dot.draw_plain " (thing-at-point 'symbol))))

;; (define-key map [?\C-c ?\M-g] 'hsc3-draw-graph-m)
(defun hsc3-draw-graph-m ()
  "Draw the (monadic) UGen graph at point."
  (interactive)
  (hsc3-send-string
   (concat "Sound.SC3.UGen.Dot.draw =<<" (thing-at-point 'symbol))))

;; (define-key map [menu-bar hsc3 help sc3-server]
;;   '("SuperCollider Server Command help" . hsc3-sc3-server-help))

;; (define-key map (kbd "C-c C-s") 'hsc3-stop) ; ie. sclang-mode key

;; (define-key map [?\C-c ?\C-0] 'hsc3-quit-scsynth)
