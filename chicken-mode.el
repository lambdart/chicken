;;; chiken-mode.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/chiken-mode.el
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

(require 'scheme)

(require 'chicken-overlay)
(require 'chicken-comint)

(defvar chicken-mode nil)

(defvar chicken-mode-keyword-list
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

(defvar chicken-mode-indent-list
  '((printf 1)
    (fprintf 2)
    (sprintf 1)
    (set-record-printer! 1))
  "Chicken Scheme indent list.")

(defvar chicken-mode-keyword-regexp ""
  "Keyword regex pattern.")

(defun chicken-mode-indent (_state _indent-point _normal-indent)
  "Module Indentation." 0)

(defun chicken-mode-keyword-regexp (keyword-list)
  "Build keyword regex pattern from the given KEYWORD-LIST."
  (let ((str "\\<\\(module\\>"))
    (dolist (keyword keyword-list)
      (put (car keyword) 'scheme-indent-hook (cadr keyword))
      (setq str (concat str "\\|"
                        (regexp-quote (symbol-name
                                       (car keyword)))
                        "\\>")))
    (concat str "\\)")))

(defun chicken-mode-scheme-setup ()
  "Setup Chicken `scheme-mode' variables."
  ;; setup module indent
  (put 'module 'scheme-indent-function 'chicken-mode-indent)
  (put 'and-let* 'scheme-indent-function 1)
  (put 'parameterize 'scheme-indent-function 1)
  (put 'handle-exceptions 'scheme-indent-function 1)
  (put 'when 'scheme-indent-function 1)
  (put 'unless 'scheme-indent-function 1)
  (put 'match 'scheme-indent-function 1)
  ;; update keyword regex pattern
  (setq chicken-mode-keyword-regexp
        (chicken-mode-keyword-regexp chicken-mode-keyword-list))
  ;; update scheme indentation hook
  (dolist (e chicken-mode-indent-list)
    (put (car e) 'scheme-indent-hook (cadr e)))
  ;; add font lock keywords
  (font-lock-add-keywords
   'scheme-mode
   `(("\\<\\sw+\\>:" . font-lock-constant-face)
     ("##\\(core\\|sys\\)#\\sw+\\>" . font-lock-builtin-face)
     (,chicken-mode-keyword-regexp . font-lock-keyword-face))))
;; setup scheme-mode-map
;; (setq-local scheme-mode-map chicken-mode-map))

(defun chicken-mode-menu ()
  "Define mode menu."
  (easy-menu-define chicken-mode-menu scheme-mode-map
    "Chicken Minor Mode Menu"
    '("Chicken"
      ["Eval region" chicken-op-eval-region t]
      ["Eval buffer" chicken-op-eval-buffer t]
      ["Eval define" chicken-op-eval-define t]
      ["Eval last-sexp" chicken-op-eval-last-sexp t]
      "--"
      ["Import" chicken-import]
      "--"
      ["Load file" chicken-load-file t]
      ["Load current file" chicken-load-file t]
      "--"
      ["Doc" chicken-doc t]
      ["Toc" chicken-toc t]
      ["Wtf" chicken-wtf t]
      "--"
      ["Doc-dwim" chicken-doc-dwim t]
      ["Apropos" chicken-apropos t]
      "--"
      ["Compile" chicken-compile-current-file t])))

(defun chicken-mode-state ()
  "Show \\{chicken-mode} state, i.e: on or off."
  (interactive)
  (message "chicken-mode %s" (if chicken-mode "on" "off")))

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

\\{chicken-mode-map}"

  :lighter "chicken"
  :keymap chicken-op-mode-map
  :global nil
  (cond
   (chicken-mode
    ;; scheme setup
    (chicken-mode-scheme-setup)
    ;; define chicken menu
    (chicken-mode-menu)
    ;; add delete overlay hook
    (add-hook 'pre-command-hook #'chicken-overlay-delete nil t))
   (t
    ;; ensure overlay was deleted
    (chicken-overlay-delete)
    ;; remove delete overlay hook
    (remove-hook 'pre-command-hook #'chicken-overlay-delete t))))

;; remove redirect inspector hook
;; (remove-hook 'after-change-functions
;;              #'chicken-comint-redirect-buffer-changed t))))

(provide 'chiken-mode)

;;; chicken-mode.el ends here
