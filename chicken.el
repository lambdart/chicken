;;; chicken.el --- Chicken Scheme Minor Mode -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/chicken-el
;; Version: 0.0.1 Alpha
;; Keywords: chicken scheme
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2020 lambdart
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;;; Code:

(require 'comint)
(require 'scheme)
(require 'subr-x)

(defgroup chicken nil
  "Chicken Scheme Utilities."
  :prefix "chicken-"
  :group 'chicken)

(defcustom chicken-comint-prompt-read-only t
  "If non-nil, the comint buffer will be read-only."
  :group 'chicken
  :type 'boolean)

(defcustom chicken-pop-to-buffer-flag nil
  "Non-nil means pop to buffer in the same window."
  :group 'chicken
  :type 'boolean)

(defcustom chicken-buffer-name "csi"
  "Inferior buffer default name."
  :group 'chicken
  :type 'string)

(defcustom chicken-echo-last-output-flag nil
  "Non-nil means echo last output text.
Using the `message' builtin function."
  :group 'chicken
  :type 'boolean)

(defcustom chicken-display-overlay-flag t
  "Non-nil means display last output text in a overlay."
  :group 'chicken
  :type 'boolean)

(defcustom chicken-debug-buffer-name "*CHICKEN-DEBUG*"
  "Debug buffer name for the inferior process output."
  :group 'chicken
  :type 'string)

(defcustom chicken-comint-prompt-regexp "#[^;]*;[^:0-9]*:?[0-9]+> "
  "Regexp to recognize prompt."
  :group 'chicken
  :type 'regexp)

(defcustom chicken-comint-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Chicken comint input filter regex."
  :group 'chicken
  :type 'regexp)

(defcustom chicken-program (executable-find "csi")
  "Chicken executable full path program."
  :group 'chicken
  :type 'string
  :set `(lambda (symbol value)
          (set symbol (executable-find value))))

(defcustom chicken-comint-args '("-q")
  "Command-line arguments to pass to `chicken-program'."
  :group 'chicken
  :type 'list)

(defcustom chicken-comint-start-file
  (expand-file-name "chicken-start.scm" (cadr (split-string (pwd))))
  "The `make-comint' start file argument."
  :group 'chicken
  :type 'string)

(defcustom chicken-source-modes '(scheme-mode)
  "Used to determine if a buffer contains scheme source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Chicken source file by `chicken-load-file'."
  :group 'chicken
  :type '(repeat function))

(defcustom chicken-comint-output-timeout 4
  "Comint accept output timeout in seconds."
  :group 'chicken
  :type 'integer)

(defvar chicken-ops-alist
  `((doc      . ",doc %S")
    (toc      . ",toc %S")
    (wtf      . ",wtf %S")
    (help     . ",?")
    (load     . "(load \"%s\")")
    (import   . "(import %S)")
    (doc-dwim . "(doc-dwim \"%s\")")
    (apropos  . "(apropos \"%s\")")
    (compile  . "(compile-file \"%s\")")
    (trace    . "(trace/untrace \"%s\")"))
  "Operation associative list: (OP-KEY . OP-FMT).")

(defvar chicken-version "0.0.2 Alpha"
  "Current version string.")

(defvar chicken-overlay (make-overlay (point-min) (point-min) nil t t)
  "Overlay used to display the process output text.")

(defvar chicken-debug-buffer nil
  "Debug/Output buffer.")

(defvar chicken-proc-buffer nil
  "Comint process buffer.")

(defvar chicken-comint-output-cache '("")
  "Comint output filtered text list.")

(defvar chicken-comint-last-output ""
  "Process (cache) last output line.")

(defvar chicken-comint-filter-in-progress nil
  "Indicates if the comint filter function is still in progress.")

(defvar chicken-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.")

(defun chicken-display-version ()
  "Echo the current `chicken' version."
  (interactive)
  (message "chicken (version %s)" chicken-version))

(defun chicken-get-debug-buffer ()
  "Return/create the `chicken-debug-buffer'."
  (if (buffer-live-p chicken-debug-buffer)
      chicken-debug-buffer
    (let ((buffer (get-buffer-create chicken-debug-buffer-name)))
      (with-current-buffer buffer
        ;; enable scheme-mode if available and chicken-mode
        (and (require 'scheme nil t)
             (fboundp 'scheme-mode)
             (scheme-mode)
             (chicken-mode))
        ;; change read-only property (locally)
        (setq-local buffer-read-only t))
      ;; cache and return the debug buffer
      (setq chicken-debug-buffer buffer))))

(defun chicken-insert-debug-output (string)
  "Insert STRING into the `chicken-debug-buffer'."
  (let ((buffer (chicken-get-debug-buffer))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (insert string)
      (goto-char (point-max)))))

(defun chicken-display-overlay (text)
  "Display the TEXT using the `chicken-overlay'."
  (move-overlay chicken-overlay (point) (point) (current-buffer))
  ;; The current C cursor code doesn't know to use the overlay's
  ;; marker's stickiness to figure out whether to place the cursor
  ;; before or after the string, so let's spoon-feed it the pos.
  (put-text-property 0 1 'cursor t text)
  (overlay-put chicken-overlay 'after-string text))

(defun chicken-delete-overlay ()
  "Remove `chicken-overlay' display (if any) prior to new user input."
  (delete-overlay chicken-overlay))

(defun chicken-get-proc ()
  "Return current chicken process."
  (get-buffer-process (if (buffer-live-p chicken-proc-buffer)
                          chicken-proc-buffer
                        (chicken-comint-run))))


(defun chicken-proc-sentinel (process event)
  "Sentinel function to handle (PROCESS EVENT) relation."
  (princ (format "Process: %s had the event '%s'" process event)))

(defun chicken-filter-output-string (string)
  "Parse the process output STRING."
  ;; return the filtered output string
  (dolist (regexp `(,chicken-comint-prompt-regexp) string)
    (setq string (replace-regexp-in-string regexp "" string))))

(defun chicken-comint-in-progress-timeout ()
  "Timeout, set the control variable to nil."
  (setq chicken-comint-filter-in-progress nil))

(defun chicken-comint-wait-output ()
  "Wait for the comint output."
  ;; run with timer (timeout)
  (run-with-timer chicken-comint-output-timeout
                  nil
                  'chicken-comint-in-progress-timeout)
  ;; wait for the final prompt
  (while chicken-comint-filter-in-progress
    (sleep-for 0 10)))

(defun chicken-display-comint-output ()
  "Display/Show output from the comint csi interpreter buffer.

This function display (or insert the text) in diverse locations
with is controlled by the following custom flags:

`chicken-display-overlay-flag',
`chicken-echo-last-output-flag'

See its documentations to understand the be behavior
that will be imposed if they are true."

  ;; wait for the comint output
  (chicken-comint-wait-output)
  ;; parse text output
  (let ((line (pop chicken-comint-output-cache)))
    (while (and (not (eq line nil))
                (string-empty-p line))
      (setq line (pop chicken-comint-output-cache)))
    ;; update line if necessary
    (setq line (or line "nil"))
    ;; display line in the overlay
    (and chicken-display-overlay-flag
         (chicken-display-overlay (concat " => " line)))
    ;; echo the output line
    (and chicken-echo-last-output-flag
         (message " => %s" line))
    ;; cache last output line
    (setq chicken-comint-last-output line)
    ;; return nil
    nil))

(defun chicken-comint-preoutput-filter (string)
  "Return the output STRING."
  (let ((text (chicken-filter-output-string string)))
    ;; cache the parsed text
    (push text chicken-comint-output-cache)
    ;; insert text string in the debug buffer
    (chicken-insert-debug-output text)
    ;; verify filter in progress control variable
    (when (string-match-p chicken-comint-prompt-regexp string)
      (setq chicken-comint-filter-in-progress nil))
    ;; return the string to the comint buffer (implicit)
    string))

(defun chicken-comint-send (send-func &rest args)
  "Send ARGS, i.e, string or region using the chosen SEND-FUNC.
SEND-FUNC should be `comint-send-string' or `comint-send-region'."
  (let ((proc (chicken-get-proc)))
    ;; check if the process is alive
    (if (not (process-live-p proc))
        (message "[CHICKEN]: Error, process not found")
      ;; last comint output list should always start empty
      ;; set filter in progress control variable to true
      (setq chicken-comint-output-cache '("")
            chicken-comint-filter-in-progress t)
      ;; send string (or region) to the process
      (apply 'funcall send-func proc args)
      ;; always send a new line: <return> (implicit)
      (comint-send-string proc "\n"))))

;;;###autoload
(defun chicken-comint-run ()
  "Run an inferior instance of *CSI REPL* inside Emacs."
  (interactive)
  (let ((buffer (apply 'make-comint
                       chicken-buffer-name
                       chicken-program
                       chicken-comint-start-file
                       chicken-comint-args)))
    (unless buffer
      (error "[CHICKEN]: Error, start process %s: fails" chicken-program))
    ;; check comint process
    (comint-check-proc buffer)
    ;; set process sentinel
    (set-process-sentinel (get-buffer-process buffer) 'chicken-proc-sentinel)
    ;; start chicken comint mode
    (with-current-buffer buffer
      (chicken-comint-mode))
    ;; display buffer
    (display-buffer buffer 'display-buffer-pop-up-window)
    ;;; cache the process buffer (implicit: return it)
    (setq chicken-proc-buffer buffer)))

(defun chicken-comint-send-string (string &optional op-key)
  "Send STRING to the current inferior process.
Format the string selecting the right format using the OP-KEY."
  (let ((string (if (not op-key) string
                  (format (cdr (assoc op-key chicken-ops-alist)) string))))
    ;; send string
    (chicken-comint-send #'comint-send-string
                         (substring-no-properties string))))

(defun chicken-comint-send-region (start end)
  "Send region delimited bu START/END to the inferior process.
TIMEOUT, the `accept-process-output' timeout."
  (chicken-comint-send #'comint-send-region start end))

(defun chicken-comint-input-sender (_ string)
  "Comint input sender STRING function."
  (chicken-comint-send-string (if (string-empty-p string)
                                  "(begin)"
                                string)))

(defun chicken-comint-input-filter (string)
  "Don't save anything on the STRING matching `chicken-comint-filter-regexp'."
  (not (string-match-p chicken-comint-filter-regexp string)))

(defun chicken-comint-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun chicken-comint-setup ()
  "Helper function to setup `comint-mode' related variables.
This function will be called by `chicken-mode-hook.'"
  ;; set comint related variables
  (setq comint-process-echoes t
        comint-input-ignoredups nil
        comint-use-prompt-regexp t))

(defun chicken-comint-quit ()
  "Quit Chicken comint, i.e, quit subjob and kill the buffer."
  (interactive)
  ;; quit the subjob, if necessary
  (and chicken-proc-buffer
       (with-current-buffer chicken-proc-buffer
         (comint-quit-subjob)))
  ;; kill the buffer
  (kill-buffer chicken-proc-buffer))

(defun chicken-eval-definition ()
  "Send definition to the Chicken comint process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (chicken-comint-send-region (point) end))))

(defun chicken-read-thing (&optional thing prompt)
  "Return `thing-at-point' or read it.
If THING is non-nil use it as the `thing-at-point' parameter,
default: 'symbol.
If PROMPT is non-nil use it as the read prompt."
  (let* ((string (thing-at-point (or thing 'symbol) t))
         (fmt (if (not string) "%s: " "%s [%s]: "))
         (prompt (format fmt (or prompt "String") string)))
    ;; return the read list string
    (list (read-string prompt nil nil string))))

(defun chicken-eval-expression (sexp)
  "Eval SEXP, i.e, send it to Chicken comint process."
  (interactive (chicken-read-thing 'sexp "Eval"))
  ;; eval string s-expression
  (chicken-comint-send-string sexp))

(defun chicken-eval-last-sexp ()
  "Send the previous sexp to the inferior process."
  (interactive)
  ;; send region of the last expression
  (chicken-comint-send-region
   (save-excursion (backward-sexp) (point)) (point))
  ;; display the operation output (if any)
  (chicken-display-comint-output))

(defun chicken-eval-buffer ()
  "Eval current buffer."
  (interactive)
  (save-excursion (widen)
                  (let ((case-fold-search t))
                    (chicken-comint-send-region (point-min)
                                                (point-max)))))

(defun chicken-eval-region (start end)
  "Eval region delimited by START/END."
  (interactive "r")
  (chicken-comint-send-region start end))

(defun chicken-load-file (file-name)
  "Load the target FILE-NAME."
  (interactive (comint-get-source "File: "
                                  chicken-prev-l/c-dir/file
                                  chicken-source-modes t))
  ;; if the file is loaded into a buffer, and the buffer is modified, the user
  ;; is queried to see if he wants to save the buffer before proceeding with
  ;; the load or compile
  (comint-check-source file-name)
  ;; cache previous directory/filename
  (setq chicken-prev-l/c-dir/file
        (cons (file-name-directory file-name)
              (file-name-nondirectory file-name)))
  ;; load file operation
  (chicken-comint-send-string file-name 'load))

(defun chicken-load-current-file ()
  "Load current file."
  (interactive)
  (let ((file-name (buffer-file-name)))
    ;; load file operation
    (chicken-load-file file-name)))

(defun chicken-compile-current-file ()
  "Compile current buffer file."
  (interactive)
  (let ((file-name (buffer-file-name)))
    ;; if the file is loaded into a buffer, and the buffer is modified, the user
    ;; is queried to see if he wants to save the buffer before proceeding with
    ;; the load or compile
    (comint-check-source file-name)
    ;; cache previous directory/filename
    (setq chicken-prev-l/c-dir/file
          (cons (file-name-directory file-name)
                (file-name-nondirectory file-name)))
    ;; compile operation
    (chicken-comint-send-string file-name 'compile)))

(defun chicken-doc (string)
  "Call `,doc' STRING on the csi process."
  (interactive (chicken-read-thing nil "String"))
  ;; doc operation
  (chicken-comint-send-string string 'doc))

(defun chicken-trace (func)
  "Trace the target FUNC (function)."
  (interactive (chicken-read-thing 'symbol "Function"))
  ;; trace function operation
  (chicken-comint-send-string func 'trace))

(defvar chicken-keyword-list
  '((receive 2)
    (match 1)
    (match-lambda 0)
    (match-lambda* 0)
    (match-let scheme-let-indent)
    (match-let* 1)
    (match-letrec 1)
    (declare 0)
    (cond-expand 0)
    (let-values scheme-let-indent)
    (let*-values scheme-let-indent)
    (letrec-values 1)
    (letrec* 1)
    (parameterize scheme-let-indent)
    (let-location 1)
    (foreign-lambda 2)
    (foreign-lambda* 2)
    (foreign-primitive 2)
    (foreign-safe-lambda 2)
    (foreign-safe-lambda* 2)
    (set! 1)
    (let-optionals* 2)
    (let-optionals 2)
    (condition-case 1)
    (fluid-let 1)
    (and-let* 1)
    (assume 1)
    (cut 1)
    (cute 1)
    (when 1)
    (unless 1)
    (dotimes 1)
    (compiler-typecase 1)
    (ecase 1)
    (require-extension 0)
    (import 0)
    (handle-exceptions 2)
    (regex-case 1)
    (define-inline 1)
    (define-constant 1)
    (define-syntax-rule 1)
    (define-record-type 1)
    (define-values 1)
    (define-record 1)
    (define-specialization 1)
    (define-type 1)
    (with-input-from-pipe 1)
    (select 1)
    (functor 3)
    (define-interface 1)
    (module 2))
  "Chicken Scheme keyword list.")

(defvar chicken-indent-list
  '((printf 1)
    (fprintf 2)
    (sprintf 1)
    (set-record-printer! 1))
  "Chicken Scheme indent list.")

(defvar chicken-keyword-regexp ""
  "Keyword regex pattern.")

(defun chicken-module-indent (_state _indent-point _normal-indent)
  "Module Indentation." 0)

(defun chicken-build-keyword-regexp (keyword-list)
  "Build keyword regex pattern from the given KEYWORD-LIST."
  (let ((str "\\<\\(module\\>"))
    (dolist (keyword keyword-list)
      (put (car keyword) 'scheme-indent-hook (cadr keyword))
      (setq str (concat str "\\|" (regexp-quote (symbol-name (car keyword))) "\\>")))
    (concat str "\\)")))

(defun chicken-scheme-setup ()
  "Setup Chicken `scheme-mode' variables."
  ;; setup module indent
  (put 'module 'scheme-indent-function 'chicken-module-indent)
  ;; update keyword regex pattern
  (setq chicken-keyword-regexp
        (chicken-build-keyword-regexp chicken-keyword-list))
  ;; update scheme indentation hook
  (dolist (e chicken-indent-list)
    (put (car e) 'scheme-indent-hook (cadr e)))
  ;; add font lock keywords
  (font-lock-add-keywords
   'scheme-mode
   `(("\\<\\sw+\\>:" . font-lock-constant-face)
     ("##\\(core\\|sys\\)#\\sw+\\>" . font-lock-builtin-face)
     (,chicken-keyword-regexp . font-lock-keyword-face))))

(defvar chicken-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x")   #'chicken-eval-definition) ; Gnu convention
    (define-key map (kbd "C-x C-e") #'chicken-eval-last-sexp)  ; Gnu convention
    (define-key map (kbd "C-c C-e") #'chicken-eval-last-sexp)
    (define-key map (kbd "C-c C-b") #'chicken-eval-buffer)
    (define-key map (kbd "C-c C-r") #'chicken-eval-region)
    (define-key map (kbd "C-c C-l") #'chicken-load-file)
    (define-key map (kbd "C-c C-f") #'chicken-load-current-file)
    (define-key map (kbd "C-c C-d") #'chicken-doc)
    (define-key map (kbd "C-c C-q") #'chicken-comint-quit)
    map)
  "Chicken's minor-mode keymap.")

(defun chicken-define-menu ()
  "Define Chicken  menu."
  (easy-menu-define chicken-minor-mode-menu chicken-minor-mode-map
    "Chicken Minor Mode Menu"
    '("Chicken"
      ["Eval region" chicken-eval-region t]
      ["Eval buffer" chicken-eval-buffer t]
      ["Eval definition" chicken-eval-definition t]
      ["Eval last sexp" chicken-eval-last-sexp t]
      "--"
      ["Load file" chicken-load-file t]
      ["Load current file" chicken-load-file t]
      "--"
      ["Doc" chicken-doc t])))

;;;###autoload
(define-minor-mode chicken-mode
  "Minor mode for interacting with Chicken.

If called interactively, toggle ‘Chicken minor mode’.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is ‘toggle’.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode’s hook is called both when the mode is enabled and when
it is disabled.

The following commands are available:

\\{chicken-minor-mode-map}"

  :lighter ""
  :keymap chicken-minor-mode-map
  (cond
   (chicken-mode
    ;; scheme setup
    (chicken-scheme-setup)
    ;; define chicken menu
    (chicken-define-menu)
    ;; add delete overlay hook
    (add-hook 'pre-command-hook #'chicken-delete-overlay nil t))
   (t
    ;; ensure overlay was deleted
    (chicken-delete-overlay)
    ;; remove delete overlay hook
    (remove-hook 'pre-command-hook #'chicken-delete-overlay) t)))

(define-derived-mode chicken-comint-mode comint-mode "CHICKEN-REPL"
  "Major mode for `chicken-comint' REPL buffer.

Runs a CSI interpreter with the help of comint-mode,
use the buffer abstraction as the main I/O bridge between
Emacs and the inferior process.

You can send text to the inferior process from other buffers or the `minibuffer'
directly.

    `chicken-eval-defn'  sends function definition
    `chicken-eval-region' sends the current region
    `chicken-eval-buffer' sends the current buffer

The following commands are available:

\\{chicken-minor-mode-map}"
  :group 'chicken
  ;; set comint variables
  (setq comint-prompt-regexp chicken-comint-prompt-regexp
        comint-prompt-read-only chicken-comint-prompt-read-only
        comint-input-sender  (function chicken-comint-input-sender)
        comint-input-filter  (function chicken-comint-input-filter)
        comint-get-old-input (function chicken-comint-get-old-input))
  ;; add comint preoutput filter function
  (add-hook 'comint-preoutput-filter-functions
            'chicken-comint-preoutput-filter nil t)
  ;; set local paragraph variables
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) chicken-comint-prompt-regexp))

;;;###autoload
(add-hook 'chicken-comint-mode-hook 'chicken-comint-setup)

(provide 'chicken)

;;; chicken.el ends here
