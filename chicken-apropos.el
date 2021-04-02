;;; chicken-apropos.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/chicken-apropos.el
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

(require 'button)
(require 'apropos)

(require 'chicken-op)
(require 'chicken-comint)
;; (require 'chicken-util)

(defvar chicken-apropos-buffer-name "*chicken-apropos*"
  "Chicken apropos buffer name.")

(defvar chicken-apropos-buffer nil
  "Chicken apropos cached buffer.")

(define-button-type 'apropos-special-form
  'apropos-label "Special form"
  'apropos-short-label "s"
  'face
  'font-lock-keyword-face
  'help-echo "mouse-2, RET: Display more help on this special form"
  'follow-link t
  'action (lambda (button)
            (chicken-op-doc (button-get button 'str-sexp))))

(defun chicken-apropos-buffer ()
  "Get cached \\{chicken-apropos-buffer} or create/cache it."
  (if (buffer-live-p chicken-apropos-buffer)
      chicken-apropos-buffer
    (setq chicken-apropos-buffer
          (with-current-buffer (get-buffer-create chicken-apropos-buffer-name)
            ;; activate the apropos-mode (necessary?)
            (apropos-mode)
            ;; activate read only property
            (setq buffer-read-only t)
            ;; return the created buffer
            (current-buffer)))))

(defun chicken-apropos-insert (text &optional header)
  "Insert parsed TEXT output into `chicken-apropos-buffer-name'.
If HEADER is non-nil insert it before the text."
  (with-current-buffer (chicken-apropos-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; (when header
      ;;   (if (boundp 'header-line-format)
      ;;       (setq-local header-line-format header)
      ;;     (insert header "\n\n")))
      (insert header "\n\n")
      (insert text)
      (goto-char (point-min)))
    (current-buffer)))

;; TODO: parse and propertize the apropos output, create the buttons

(defun chicken-apropos-display ()
  "Apropos display output function handler."
  ;; parse the response and display it in the apropos buffer
  ;; operation->output
  (chicken-comint-proc-wait-redirect
   (let ((header "Apropos: "))
     (save-excursion
       (display-buffer
        (chicken-apropos-insert output header))))))

(provide 'chicken-apropos)

;;; chicken-apropos.el ends here
