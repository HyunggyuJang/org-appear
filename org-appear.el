;;; org-appear.el --- Auto-toggle Org elements -*- lexical-binding: t; -*-

;; Portions of code in this file are taken from org-fragtog https://github.com/io12/org-fragtog
;; org-fragtog Copyright (C) 2020 Benjamin Levy - MIT/X11 License
;; org-appear Copyright (C) 2021 Alice Istleyeva - MIT License
;; Author: Alice Istleyeva <awth13@gmail.com>
;; Version: 0.2.4
;; Description: Toggle Org mode element visibility upon entering and leaving
;; Homepage: https://github.com/awth13/org-appear
;; Package-Requires: ((emacs "25.1") (org "9.3"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package enables automatic visibility toggling of various Org elements depending on cursor position.
;; It supports automatic toggling of emphasis markers, links, subscripts and
;; superscripts, entities, and keywords. By default, toggling is instantaneous
;; and only affects emphasis markers. If Org mode custom variables that control
;; visibility of elements are configured to show hidden parts, the respective
;; `org-appear' settings do not have an effect.

;;; Code:

(require 'org)
(require 'org-element)
(require 'subr-x)			; Compatibility

(defgroup org-appear nil
  "Auto-toggle Org elements"
  :group 'org)

(defcustom org-appear-autoemphasis t
  "Non-nil enables automatic toggling of emphasised and verbatim markers.
Does not have an effect if `org-hide-emphasis-markers' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autosubmarkers nil
  "Non-nil enables automatic toggling of subscript and superscript markers.
Does not have an effect if `org-pretty-entities' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autoentities nil
  "Non-nil enables automatic toggling of org entities.
Does not have an effect if `org-pretty-entities' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autolinks nil
  "Non-nil enables automatic toggling of links.
Does not have an effect if `org-link-descriptive' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autokeywords nil
  "Non-nil enables automatic toggling of keywords.
Does not have an effect if `org-hidden-keywords' is nil."
  :type 'boolean
  :group 'org-appear)


(defcustom org-appear-clearlatex nil
  "Non-nil enables automatic cleaning of Latex inline math blocks."
  :type 'boolean
  :group 'org-appear)

;;;###autoload
(define-minor-mode org-appear-mode
  "A minor mode that automatically toggles elements in Org mode."
  :init-value nil
  :lighter nil
  :keymap nil

  (cond
   (org-appear-mode
    (org-appear--set-elements))
   (t
    ;; Clean up current element when disabling the mode
    (when-let ((current-elem (org-appear--current-elem)))
      (org-appear--hide-invisible current-elem)
      (remove-hook 'post-command-hook #'org-appear--post-cmd t)))))

(defvar org-appear-elements nil
  "List of Org elements to toggle.")

(defvar-local org-appear--prev-elem nil
  "Previous element that surrounded the cursor or nil if the cursor was not
on an element.")

(defun org-appear--set-elements ()
  "Add elements to toggle to `org-appear-elements'."
  (let ((emphasis-elements '(bold
			     italic
			     underline
			     strike-through
			     verbatim
			     code))
	(script-elements '(subscript
			   superscript))
	(entity-elements '(entity))
	(link-elements '(link))
	(keyword-elements '(keyword))
	(latex-elements '(latex-fragment latex-environment)))

    ;; HACK: is there a better way to do this?
    (setq-local org-appear--prev-elem nil)
    (setq org-appear-elements nil)	; reset
    (when (and org-hide-emphasis-markers org-appear-autoemphasis)
      (setq org-appear-elements (append org-appear-elements emphasis-elements)))
    (when (and org-pretty-entities org-appear-autosubmarkers)
      (setq org-appear-elements (append org-appear-elements script-elements)))
    (when (and org-pretty-entities org-appear-autoentities)
      (setq org-appear-elements (append org-appear-elements entity-elements)))
    (when (and org-link-descriptive org-appear-autolinks)
      (setq org-appear-elements (append org-appear-elements link-elements)))
    (when (and org-hidden-keywords org-appear-autokeywords)
      (setq org-appear-elements (append org-appear-elements keyword-elements)))
    (when org-appear-clearlatex
      (setq org-appear-elements (append org-appear-elements latex-elements)))))

(defun org-appear-unhide-at-point ()
  "Unhide at current point."
  (interactive)
  (when-let ((current-elem (org-appear--current-elem)))
    (when current-elem
      (org-appear--show-with-lock current-elem))
    (setq org-appear--prev-elem current-elem)
    (add-hook 'post-command-hook #'org-appear--post-cmd nil t)))

(defun org-appear--post-cmd ()
  "This function is executed by `post-command-hook' in `org-appear-mode'.
It handles toggling elements depending on whether the cursor entered or exited them."
  (let* ((prev-elem org-appear--prev-elem)
         (prev-elem-start (org-element-property :begin prev-elem))
         (current-elem (org-appear--current-elem))
         (current-elem-start (org-element-property :begin current-elem)))

    (if (eq prev-elem-start current-elem-start) ;Cannot use `=' due to `nil'
        (org-appear--show-with-lock current-elem)
      ;; After leaving an element
      (save-excursion
        (goto-char prev-elem-start)
        ;; Reevaluate `org-element-context' in case the bounds
        ;; of the previous element changed
        (org-appear--hide-invisible (org-element-context))
        (remove-hook 'post-command-hook #'org-appear--post-cmd t)))))

(defun org-appear--current-elem ()
  "Return element at point.
Return nil if element is not supported by `org-appear-mode'."
  (when-let ((elem (org-element-context)))
    (let* ((elem-type (car elem))
	   (elem-end (- (org-element-property :end elem)
			(1- (org-element-property :post-blank elem))))
	   (link-ignore-p (and (eq elem-type 'link)
                               (or (org-link-get-parameter
                                    (org-element-property :type elem)
                                    :display)
                                   (eq 'plain
				       (org-element-property :format elem)))))
	   (key-ignore-p (and (eq elem-type 'keyword)
			      (not (memq (intern (downcase
						  (org-element-property :key elem)))
					 org-hidden-keywords)))))
      (if (and (memq elem-type org-appear-elements)
	       (< (point) elem-end)     ; Ignore post-element whitespace
	       (not link-ignore-p)	; Ignore plain and org-ref links
	       (not key-ignore-p))	; Ignore unhidden keywords
	  elem
	nil))))

(defun org-appear--parse-elem (elem)
  "Return bounds of element ELEM.
Return nil if element cannot be parsed."
  (let* ((elem-start (org-element-property :begin elem))
	 (elem-end (org-element-property :end elem))
	 (elem-content-start (org-element-property :contents-begin elem))
	 (elem-content-end (org-element-property :contents-end elem))
	 ;; Some elements have extra spaces at the end
	 ;; The number of spaces is stored in the post-blank property
	 (post-elem-spaces (org-element-property :post-blank elem))
         (elem-type (car elem))
         (elem-tag (cond ((memq elem-type '(bold
                                            italic
                                            underline
                                            strike-through
                                            verbatim
                                            code))
                          'emph)
                         ((memq elem-type '(subscript
                                            superscript))
                          'script)
                         ((eq elem-type 'entity)
                          'entity)
                         ((eq elem-type 'link)
                          'link)
                         ((eq elem-type 'keyword)
                          'keyword)
                         ((memq elem-type '(latex-fragment latex-environment))
                          (unless (cl-find-if (lambda (o) (overlay-get o 'org-overlay-type)) (overlays-at elem-start))
                            'latex))
                         (t nil)))
         (elem-end-real (- elem-end post-elem-spaces)))
    ;; Only sub/superscript elements are guaranteed to have
    ;; contents-begin and contents-end properties
    (when elem-tag
      `(:start ,elem-start
	       :end ,elem-end-real
	       :visible-start ,(pcase elem-tag
				 ('emph (1+ elem-start))
				 ('script elem-content-start)
				 ('link (or elem-content-start (+ elem-start 2))))
	       :visible-end ,(pcase elem-tag
			       ('emph (1- elem-end-real))
			       ('script elem-content-end)
			       ('link (or elem-content-end (- elem-end-real 2))))))))

(defun org-appear--show-with-lock (elem)
  "Show invisible parts of element ELEM.
When RENEW is non-nil, obtain element at point instead."
  ;; When called with timer, element might be different upon arrival

  (when-let* ((elem-at-point (org-appear--parse-elem elem))
              (elem-type (car elem))
              (start (plist-get elem-at-point :start))
              (end (plist-get elem-at-point :end)))
    ;; Call `font-lock-ensure' before unhiding to prevent `jit-lock-mode'
    ;; from refontifying the element region after changes in buffer
    (font-lock-ensure start (save-excursion (goto-char end) (point-at-bol 2)))
    (with-silent-modifications
      (cond ((memq elem-type '(latex-fragment latex-environment))
             (remove-text-properties start end '(composition nil invisible)))
            ((eq elem-type 'entity)
             (decompose-region start end))
            ((eq elem-type 'keyword)
             (remove-text-properties start end '(invisible org-link)))
            (t
             (let ((visible-start (plist-get elem-at-point :visible-start))
                   (visible-end (plist-get elem-at-point :visible-end)))
               (remove-text-properties start visible-start '(invisible org-link))
               (remove-text-properties visible-end end '(invisible org-link))))))))

(defun org-appear--hide-invisible (elem)
  "Silently add invisible property to invisible parts of element ELEM."
  (let* ((elem-at-point (org-appear--parse-elem elem))
	 (elem-type (car elem))
	 (start (plist-get elem-at-point :start))
	 (end (plist-get elem-at-point :end))
	 (visible-start (plist-get elem-at-point :visible-start))
	 (visible-end (plist-get elem-at-point :visible-end)))
    (when elem-at-point
      (with-silent-modifications
	(cond ((eq elem-type 'entity)
	       (compose-region start end (org-element-property :utf-8 elem)))
	      ((memq elem-type '(keyword latex-fragment latex-environment))
	       (font-lock-flush start end))
	      (t
	       (put-text-property start visible-start 'invisible 'org-link)
	       (put-text-property visible-end end 'invisible 'org-link))))
      ;; (font-lock-flush start end)
      ;; Call `font-lock-ensure' after flushing to prevent `jit-lock-mode'
      ;; from refontifying the next element entered
      ;; -> Seems like unncessary
      ;; (font-lock-ensure start end)
      )))

(provide 'org-appear)
;;; org-appear.el ends here
