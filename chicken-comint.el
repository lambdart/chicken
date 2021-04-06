;;; chicken-comint.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/chicken-comint.el
;; Version: 0.0.1 Alpha
;; Keywords:
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

(defgroup chicken-comint nil
  "Chicken Scheme Utilities."
  :prefix "chicken-comint-"
  :group 'chicken-comint)

(defcustom chicken-comint-prompt-read-only t
  "If non-nil, the comint buffer will be read-only."
  :group 'chicken-comint
  :type 'boolean)

(defcustom chicken-comint-buffer-name "csi"
  "Inferior buffer default name."
  :group 'chicken-comint
  :type 'string)

(defcustom chicken-debug-buffer-name "*CHICKEN-DEBUG*"
  "Debug buffer name for the inferior process output."
  :group 'chicken-comint
  :type 'string)

(defcustom chicken-comint-prompt-regexp "^#;[0-9]+>."
  "Regexp to recognize prompt."
  :group 'chicken-comint
  :type 'regexp)

(defcustom chicken-comint-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Chicken comint input filter regex."
  :group 'chicken-comint
  :type 'regexp)

(defcustom chicken-comint-program (executable-find "csi")
  "Chicken executable full path program."
  :group 'chicken-comint
  :type 'file
  :set `(lambda (symbol value)
          (set symbol (executable-find value))))

(defcustom chicken-comint-program-args '("-q")
  "Command-line arguments to pass to `chicken-comint-program'."
  :group 'chicken-comint
  :type 'list)

(defcustom chicken-comint-output-timeout 15
  "Comint accept output timeout in seconds."
  :group 'chicken-comint
  :type 'integer)

(defvar chicken-comint-start-file
  (expand-file-name "start/chicken-start.scm" "./")
  "The `make-comint' start file argument.")

(defvar chicken-comint-redirect-buffer-name "chicken-output"
  "Redirect output buffer name.")

(defvar chicken-comint-redirect-buffer nil
  "Redirect output buffer.")

(defvar chicken-comint-buffer nil
  "Comint process buffer.")

(defvar chicken-comint-output-cache '("")
  "Comint output filtered text list.")

(defvar chicken-comint-last-output ""
  "Process (cache) last output line.")

(defvar chicken-comint-proc-in-progress nil
  "Indicates if the comint filter function is still in progress.")

(defvar chicken-comint-resp-handler nil)

(defvar chicken-comint-prev-buffer nil)

(defun chicken-comint-redirect-buffer ()
  "Get or create the \\{chicken-comint-redirect-buffer}."
  (if (buffer-live-p chicken-comint-redirect-buffer)
      chicken-comint-redirect-buffer
    (let ((buffer (get-buffer-create chicken-comint-redirect-buffer-name)))
      (with-current-buffer buffer
        ;; enable scheme-mode if available and chicken-mode?
        (and (require 'scheme nil t)
             (fboundp 'scheme-mode)
             (scheme-mode)))
      ;; cache and return the buffer
      (setq chicken-comint-redirect-buffer buffer))))

(defun chicken-comint-redirect-buffer-content ()
  "Return the redirect-buffer content."
  (with-current-buffer (chicken-comint-redirect-buffer)
    (buffer-string)))

(defun chicken-comint-redirect-completed-p ()
  "Return if the PROC/BUFFER redirecting is over."
  ;; comint-redirect-completed is local in relation to the comint-buffer
  (with-current-buffer chicken-comint-buffer
    (null comint-redirect-completed)))

(defun chicken-comint-redirect-erase-buffer ()
  "Clean the redirect buffer."
  (with-current-buffer (chicken-comint-redirect-buffer)
    ;; remove read only protection (just in case)
    (setq buffer-read-only nil)
    ;; clean the buffer
    (erase-buffer)))

(defun chicken-comint-proc ()
  "Return current comint process."
  (let ((proc (get-buffer-process
               (and (buffer-live-p chicken-comint-buffer)
                    chicken-comint-buffer))))
    (if (process-live-p proc) proc
      (get-buffer-process (chicken-comint-run)))))

(defun chicken-comint-proc-sentinel (process event)
  "Sentinel function to handle (PROCESS EVENT) relation."
  (princ (format "Process: %s had the event '%s'" process event)))

(defun chicken-comint-proc-in-progress-timeout ()
  "Timeout, set the control variable to nil."
  (setq chicken-comint-proc-in-progress nil))

(defun chicken-comint-proc-wait ()
  "Wait for the comint output."
  (when chicken-comint-proc-in-progress
    ;; run with timer (timeout)
    (run-with-timer chicken-comint-output-timeout nil
                    'chicken-comint-proc-in-progress-timeout)
    ;; wait for the final prompt
    (while chicken-comint-proc-in-progress
      (sleep-for 0 10))))

(defmacro chicken-comint-with-redirect-output (&rest body)
  "Evaluate the BODY forms with the redirect output."
  ;; extract output from the buffer
  `(let ((output (chicken-comint-redirect-buffer-content)))
     ;; update output
     (setq output (if (string= output "") "nil" output))
     ;; evaluate body forms
     ,@body
     ;; clean display function
     (setq chicken-comint-resp-handler nil
           chicken-comint-prev-buffer nil)))

(defun chicken-comint-redirect-dispatch-resp-handler ()
  "Dispatch the display handler callback."
  (when chicken-comint-resp-handler
    (funcall chicken-comint-resp-handler
             chicken-comint-prev-buffer)))

(defun chicken-comint-cache-output ()
  "Return cached cache output."
  ;; concat the output and return it
  (eval `(concat ,@(reverse chicken-comint-output-cache))))

(defun chicken-comint-preoutput-filter (string)
  "Return the output STRING."
  ;; cache comint response
  (push string chicken-comint-output-cache)
  ;; verify filter in progress control variable
  (when (string-match-p chicken-comint-prompt-regexp string)
    (setq chicken-comint-proc-in-progress nil))
  ;; return the string to the comint buffer (implicit)
  string)

(defun chicken-comint-send (string)
  "Send STRING, using `comint-send-string' function."
  (let ((proc (chicken-comint-proc)))
    ;; check if the process is alive
    (if (not (process-live-p proc))
        (message "[CHICKEN]: error, process not found")
      ;; output list should always start empty
      ;; set progress control variable to true
      (setq chicken-comint-output-cache '("")
            chicken-comint-proc-in-progress t)
      ;; send string (or region) to the process
      (comint-send-string proc string)
      ;; always send a new line: <return> (implicit)
      (comint-send-string proc "\n"))))

(defun chicken-comint-redirect-input-to-process
    (send-func from-buffer echo &optional no-display &rest input)
  "Send INPUT FROM-BUFFER to process using the chosen SEND-FUNC.
If ECHO is non-nil, output in process buffer.
If NO-DISPLAY is non-nil, do not show the output buffer."
  (let* ((proc (chicken-comint-proc))
         (proc-buffer (process-buffer proc))
         (output-buffer (chicken-comint-redirect-buffer)))
    (if (not (buffer-live-p output-buffer))
        (message "[CHICKEN]: error, no redirect output buffer available")
      ;; erase redirect buffer
      (chicken-comint-redirect-erase-buffer)
      ;; change to the process buffer
      (with-current-buffer proc-buffer
        ;; set up for redirection
        (comint-redirect-setup output-buffer
                               chicken-comint-buffer        ; comint buffer
                               chicken-comint-prompt-regexp ; finished regexp
                               echo)                        ; echo input
        ;; set the filter
        (add-function :around (process-filter proc) #'comint-redirect-filter)
        ;; apply the right process send function
        (with-current-buffer from-buffer (apply send-func proc input))
        ;; always send new line <return>
        (process-send-string proc "\n")
        ;; show the output
        (or no-display (display-buffer (get-buffer-create output-buffer)))))))

(defun chicken-comint-input-sender (_ string)
  "Comint input sender STRING function."
  (chicken-comint-send (if (string= string "") "(begin)" string)))

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
  (and chicken-comint-buffer
       (with-current-buffer chicken-comint-buffer
         (comint-quit-subjob)))
  ;; kill the buffer
  (kill-buffer chicken-comint-buffer))

;;;###autoload
(defun chicken-comint-run ()
  "Run an inferior instance of *CSI REPL* inside Emacs."
  (interactive)
  (let ((buffer (apply 'make-comint
                       chicken-comint-buffer-name
                       chicken-comint-program
                       chicken-comint-start-file
                       chicken-comint-program-args)))
    (unless buffer
      (error "[CHICKEN]: Error, start process %s: fails"
             chicken-comint-program))
    ;; check comint process
    (comint-check-proc buffer)
    ;; set process sentinel
    (set-process-sentinel (get-buffer-process buffer)
                          'chicken-comint-proc-sentinel)
    ;; start chicken comint mode
    (with-current-buffer buffer
      (chicken-comint-mode))
    ;; display buffer
    (display-buffer buffer 'display-buffer-pop-up-window)
    ;;; cache the process buffer (implicit: return it)
    (setq chicken-comint-buffer buffer)))

(define-derived-mode chicken-comint-mode comint-mode "CHICKEN-REPL"
  "Major mode for `chicken-comint' REPL buffer.

Runs a CSI interpreter with the help of `comint-mode',
use the buffer abstraction as the main I/O bridge between
Emacs and the inferior process.

You can send text to the inferior process from other buffers or the `minibuffer'
directly.

    `chicken-eval-defn'  sends function definition
    `chicken-eval-region' sends the current region
    `chicken-eval-buffer' sends the current buffer

The following commands are available:

\\{chicken-comint-mode-map}"
  :group 'chicken-comint
  ;; set comint variables
  (setq comint-prompt-regexp chicken-comint-prompt-regexp
        comint-prompt-read-only chicken-comint-prompt-read-only
        comint-input-sender (function chicken-comint-input-sender)
        comint-input-filter (function chicken-comint-input-filter)
        comint-get-old-input (function chicken-comint-get-old-input))
  ;; add comint preoutput filter function
  (add-hook 'comint-preoutput-filter-functions
            'chicken-comint-preoutput-filter nil t)
  ;; customize redirect verbose
  (customize-set-variable 'comint-redirect-verbose t)
  ;; add dispatch display handler hook
  (add-hook 'comint-redirect-hook
            #'chicken-comint-redirect-dispatch-resp-handler)
  ;; set local paragraph variables
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) chicken-comint-prompt-regexp))

;;;###autoload
(add-hook 'chicken-comint-mode-hook 'chicken-comint-setup)

(provide 'chicken-comint)

;;; chicken-comint.el ends here

