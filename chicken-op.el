;;; chicken-op.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/chicken-op-op.el
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

(require 'scheme)                                        ;

(require 'chicken-comint)

(defvar chicken-op-alist
  `((input     . (nil "%s"))
    (doc       . (nil ",doc %s"))
    (toc       . (nil ",toc %s"))
    (wtf       . (nil ",wtf %s"))
    (apropos   . (chicken-apropos-display "(apropos %s sort: #:module)"))
    (doc-dwim  . (nil "(doc-dwim %S)"))
    (eval      . (nil "%s"))
    (eval-last . (chicken-overlay-display-handler "%s"))
    (load      . (nil "(load %S)"))
    (import    . (nil "(import %s)"))
    (compile   . (nil "(compile-file %S)"))
    (trace     . (nil "(trace/untrace %S)")))
  "Operation associative list: (OP-KEY . (OP-FN OP-FMT).
OP-KEY, the operation key selector.
OP-OUTPUT-HANDLER, the operation display response function,
manly used to parse/display the resulting text output.
OP-FORMAT-STRING, the operation format string.")

(defvar chicken-op-mode-map
  (let ((map (keymap-parent scheme-mode-map)))
    (define-key map (kbd "C-M-x")   #'chicken-op-eval-define) ; Gnu convention
    (define-key map (kbd "C-c C-e") #'chicken-op-eval-last-sexp)
    (define-key map (kbd "C-x C-e") #'chicken-op-eval-last-sexp)  ; Gnu convention
    (define-key map (kbd "C-c C-r") #'chicken-op-eval-region)
    (define-key map (kbd "C-c C-b") #'chicken-op-eval-buffer)
    (define-key map (kbd "C-c C-l") #'chicken-op-load-file)
    (define-key map (kbd "C-c C-f") #'chicken-op-load-current-file)
    (define-key map (kbd "C-c C-c") #'chicken-op-compile-current-file)
    (define-key map (kbd "C-c I")   #'chicken-op-import)
    (define-key map (kbd "C-c C-a") #'chicken-op-apropos)
    (define-key map (kbd "C-c d")   #'chicken-op-doc-dwim)
    (define-key map (kbd "C-c C-d") #'chicken-op-doc)
    (define-key map (kbd "C-c C-t") #'chicken-op-toc)
    (define-key map (kbd "C-c C-w") #'chicken-op-wtf)
    (define-key map (kbd "C-c C-q") #'chicken-comint-quit)
    map)
  "Chicken commands (or operations) keymap.")

(defun chicken-op-dispatch (op-key input-type &optional echo &rest input)
  "Dispatch the operation defined by OP-KEY.
INPUT-TYPE, the string \"region\" or \"string\".
If ECHO is non-nil, mirror the output in the comint buffer.
INPUT, the string or the region bounds."
  (let* ((op (cdr (assoc op-key chicken-op-alist))) ; select operation
         ;; get its response handler function
         (op-display-handler (car op))
         ;; get its format
         (op-format-string (cadr op)))
    ;; set comint display function callback and cache the current buffer
    (setq chicken-comint-resp-handler op-display-handler
          chicken-comint-prev-buffer (current-buffer))
    ;; send the parsed input to REPL process/buffer
    (apply 'chicken-comint-redirect-input-to-process
           ;; set process send function
           (intern (concat "process-send-" input-type))
           ;; from current buffer
           (current-buffer)
           ;; mirror output to comint buffer?
           (or echo nil)
           ;; display output?
           nil
           ;; format string or send region (beg/end)?
           (if (> (length input) 1) input
             (list (format op-format-string (car input)))))))

(defun chicken-op-thing-at-point (&optional thing prompt)
  "Return `thing-at-point' or read it.
If THING is non-nil use it as the `thing-at-point' parameter,
default: 'symbol.
If PROMPT is non-nil use it as the read prompt."
  (let* ((string (thing-at-point (or thing 'symbol) t))
         (fmt (if (not string) "%s: " "%s [%s]: "))
         (prompt (format fmt (or prompt "String") string)))
    ;; return the read list string
    (list (read-string prompt nil nil string))))

(defun chicken-op-eval-define ()
  "Send definition to the Chicken comint process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (chicken-op-dispatch 'eval-last "region" nil (point) end))))

(defun chicken-op-eval-sexp (sexp)
  "Eval SEXP string, i.e, send it to Chicken comint process."
  (interactive (chicken-op-thing-at-point 'sexp "Eval"))
  ;; eval string symbolic expression
  (chicken-op-dispatch 'eval "string" nil sexp))

(defun chicken-op-eval-last-sexp ()
  "Send the previous sexp to the inferior process."
  (interactive)
  ;; send region of the last expression
  (chicken-op-dispatch 'eval-last "region" nil
                       (save-excursion (backward-sexp) (point)) (point)))

(defun chicken-op-eval-buffer ()
  "Eval current buffer."
  (interactive)
  (save-excursion
    (widen)
    (let ((case-fold-search t))
      (chicken-op-dispatch 'eval "region" nil (point-min) (point-max)))))

(defun chicken-op-eval-region (beg end)
  "Eval BEG/END region."
  (interactive "r")
  (chicken-op-dispatch 'eval "region" nil beg end))

(defun chicken-op-import (library)
  "Import LIBRARY operation."
  (interactive (chicken-op-thing-at-point nil "Import"))
  ;; import operation
  (chicken-op-dispatch 'import "string" nil library))

(defvar chicken-op-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.")

(defvar chicken-source-modes '(scheme-mode)
  "Used to determine if a buffer contains scheme source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Chicken source file by `chicken-load-file'.")

(defun chicken-op-load-file (file-name)
  "Load the target FILE-NAME."
  (interactive (comint-get-source "File: "
                                  chicken-op-prev-l/c-dir/file
                                  chicken-source-modes t))
  ;; if the file is loaded into a buffer, and the buffer is modified, the user
  ;; is queried to see if he wants to save the buffer before proceeding with
  ;; the load or compile
  (comint-check-source file-name)
  ;; cache previous directory/filename
  (setq chicken-op-prev-l/c-dir/file
        (cons (file-name-directory file-name)
              (file-name-nondirectory file-name)))
  ;; load file operation
  (chicken-op-dispatch 'load "string" nil file-name))

(defun chicken-op-compile-current-file ()
  "Compile current buffer file."
  (interactive)
  (let ((file-name (buffer-file-name)))
    ;; if the file is loaded into a buffer, and the buffer is modified, the user
    ;; is queried to see if he wants to save the buffer before proceeding with
    ;; the load or compile
    (comint-check-source file-name)
    ;; cache previous directory/filename
    (setq chicken-op-prev-l/c-dir/file
          (cons (file-name-directory file-name)
                (file-name-nondirectory file-name)))
    ;; compile operation
    (chicken-op-dispatch 'compile "string" nil file-name)))

(defun chicken-op-load-current-file ()
  "Load current file."
  (interactive)
  (let ((file-name (buffer-file-name)))
    ;; load file operation
    (chicken-op-load-file file-name)))

(defun chicken-op-toc (input)
  "List contents (toc) of the given INPUT."
  (interactive (chicken-op-thing-at-point nil "Toc"))
  ;; ,toc operation
  (chicken-op-dispatch 'toc "string" nil input))

(defun chicken-op-wtf (input)
  "Send INPUT string to where-to-find operation."
  (interactive (chicken-op-thing-at-point nil "Pattern"))
  ;; ,wtf operation
  (chicken-op-dispatch 'wtf "string" nil input))

(defun chicken-op-doc (input)
  "Describe identifier INPUT (string) using the ,doc operation."
  (interactive (chicken-op-thing-at-point 'sexp "Doc"))
  ;; ,doc operation
  (chicken-op-dispatch 'doc "string" nil input))

(defun chicken-op-doc-dwim (input)
  "Send INPUT to the selected `doc-dwin' operation."
  (interactive (chicken-op-thing-at-point nil "Doc-dwim"))
  ;; doc-dwin operation
  (chicken-op-dispatch 'doc-dwim "string" nil input))

(defun chicken-op-apropos (input)
  "Send INPUT string to the selected `apropos' operation."
  (interactive (chicken-op-thing-at-point nil "Pattern"))
  ;; verify if the given/read string represents a quoted symbol
  (let ((input (if (string-match "^'" input) input (format "%S" input))))
    ;; send apropos operation
    (chicken-op-dispatch 'apropos "string" nil input)))

(defun chicken-op-trace (input)
  "Trace the target INPUT (function)."
  (interactive (chicken-op-thing-at-point 'symbol "Function"))
  ;; trace function operation
  (chicken-op-dispatch 'trace "string" nil input))

(provide 'chicken-op)

;;; chicken-op.el ends here
