;;; company-english-helper.el --- English helper with company interface

;; Filename: company-english-helper.el
;; Description: English helper with company interface
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-07-06 23:22:22
;; Version: 1.0
;; Last-Updated: 2018-08-28 22:17:36
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/company-english-helper.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `cl' `cl-lib' `company' `company-english-helper-data'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; English helper with company interface.
;;

;;; Installation:
;;
;; Put company-english-helper.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'company-english-helper)
;;
;; And bind your key with function `toggle-company-english-helper'.
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET company-english-helper RET
;;

;;; Change log:
;;
;; 2018/08/27
;;      * Add fuzz search algorithm.
;;      * Make prefix-match-candidates as first candidates, then suffix-match-candidates and fuzz-match-candidates.
;;
;; 2018/08/10
;;      * Require `cl' avoid error "Symbol's function definition is void: remove-if-not".
;;
;; 2018/07/29
;;      * Calculate maximin length of match candidates dynamically.
;;      * Adjust require code place.
;;      * Adjust variable code place.
;;
;; 2018/07/20
;;      * Use `string-prefix-p' instead fuzz match, too many wrong candidates in completion result.
;;      * Use `company-grab-symbol' instead `company-grab-word' to fix that word "good-bye" can't completion, thanks et2010!
;;
;; 2018/07/06
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'cl)
(require 'cl-lib)
(require 'company)
(require 'company-english-helper-data)

;;; Code:

(defvar company-english-helper-candidate-max-width 30
  "The max width of candidates.
Default is 30, it will occur candidate is not alignment if this value too small.")

(defvar company-english-helper-active-p nil
  "The status of company-english-helper plugins.
Default is disable.")

(defvar company-english-helper-match-group-size 10
  "The max size of match group.")

(defun english-helper-annotation (s)
  (let* ((translation (get-text-property 0 :initials s))
         (translation-format-string (replace-regexp-in-string "\\cc" "" translation))
         (max-translation-length (+ 1 (apply 'max (mapcar 'length company-candidates))))
         (candidate-length (length s))
         (translation-length (length translation))
         (translation-format-length (length translation-format-string))
         (blank-length (max 0 (- max-translation-length candidate-length)))
         (dot-length (max 0 (- company-english-helper-candidate-max-width (- translation-length translation-format-length)))))
    (format "%s" (concat (make-string blank-length ?\ )
                         translation
                         (make-string dot-length ?\．)))))

(defun company-english-helper-search (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-english-helper-search))
    (prefix (company-grab-symbol))
    (candidates
     (progn
       (setq prefix-match-candidates
             (remove-if-not
              (lambda (c)  (string-prefix-p (downcase arg) c))
              english-helper-completions))

       (setq suffix-match-candidates
             (remove-if-not
              (lambda (c)  (string-suffix-p (downcase arg) c))
              english-helper-completions))

       (setq fuzz-match-candidates
             (remove-if-not
              (lambda (c)
                (and (string-match-p "-" (downcase arg))
                     (let* ((split-list (split-string (downcase arg) "-"))
                            (left-string (car split-list))
                            (right-string (cadr split-list)))
                       (and (string-prefix-p left-string c)
                            (string-match-p right-string (string-remove-prefix left-string c))
                            ))))
              english-helper-completions))

       ;; Make prefix-match-candidates as first candidates, then suffix-match-candidates and fuzz-match-candidates.
       (delete-dups (append
                     (subseq prefix-match-candidates 0 (min company-english-helper-match-group-size (length prefix-match-candidates)))
                     (subseq suffix-match-candidates 0 (min company-english-helper-match-group-size (length suffix-match-candidates)))
                     (subseq fuzz-match-candidates 0 (min company-english-helper-match-group-size (length fuzz-match-candidates)))
                     ))
       ))
    (annotation (english-helper-annotation arg))
    (sorted t)
    ))

(defun toggle-company-english-helper ()
  "Toggle company english helper."
  (interactive)
  (if company-english-helper-active-p
      (progn
        (setq company-backends (remove 'company-english-helper-search company-backends))
        ;; I need remove `company-english-helper-search' with `company-yasnippet',
        ;; it's not enough just remove `company-english-helper-search' from `company-backends'
        (setq company-backends (remove '(company-english-helper-search :with company-yasnippet) company-backends))
        (setq company-english-helper-active-p nil)
        (message "English helper has disable."))
    (add-to-list 'company-backends 'company-english-helper-search)
    (setq company-english-helper-active-p t)
    (message "English helper has enable.")))

(provide 'company-english-helper)

;;; company-english-helper.el ends here
