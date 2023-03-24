;;; ox-feishu.el --- Feishu Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Schspa Shi <schspa@gmail.com>
;; Licensed under GNU GPL v3 or later.

;; This file is part of ox-feishu.

;; ox-feishu is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ox-feishu is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ox-feishu.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Schspa Shi <schspa@gmail.com>
;; URL: https://github.com/schspa/ox-feishu
;; Keywords: feishu, org, export, outlines
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (org "8.0"))

;;; Commentary:

;; This library implements a Feishu back-end for Org exporter.  Source
;; code snippets are exported for the GeSHi plugin.
;;
;; ox-feishu provides three different commands for export:
;;
;; - `ox-feishu-export-as-feishu' exports to a buffer named "*Org Feishu
;;   Export*".
;;
;; - `ox-feishu-export-to-kill-ring' does the same and additionally copies
;;   the exported buffer to the kill ring so that the generated Feishu
;;   is available in the Clipboard.
;;
;; - `ox-feishu-export-to-feishu' exports to a file with extension
;;   ".feishu".

;;; Code:

(require 'ox)

;;; Backend definition

                                        ; internal reminder: for Org format information see
                                        ; https://orgmode.org/worg/dev/org-element-api.html

;;; https://open.feishu.cn/document/ukTMukTMukTM/uUDN04SN0QjL1QDN/document-docx/docx-v1/document-block-children/create

(org-export-define-backend 'feishu
  '((bold . ox-feishu-bold)
    (center-block . ox-feishu-undefined)
    (clock . ox-feishu-undefined)
    (code . ox-feishu-code)
    (drawer . ox-feishu-undefined)
    (dynamic-block . ox-feishu-undefined)
    (entity . ox-feishu-entity)
    (example-block . ox-feishu-undefined)
    (export-block . ox-feishu-undefined)
    (export-snippet . ox-feishu-undefined)
    (fixed-width . ox-feishu-fixed-width)
    (footnote-definition . ox-feishu-footnote-definition)
    (footnote-reference . ox-feishu-footnote-reference)
    (headline . ox-feishu-headline)
    (horizontal-rule . ox-feishu-undefined)
    (inline-src-block . ox-feishu-undefined)
    (inlinetask . ox-feishu-undefined)
    (inner-template . ox-feishu-inner-template)
    (italic . ox-feishu-italic)
    (item . ox-feishu-item)
    ;; (keyword . ox-feishu-undefined) ;; don't fail, just skip keywords
    (latex-environment . ox-feishu-undefined)
    (latex-fragment . ox-feishu-undefined)
    (line-break . ox-feishu-line-break)
    (link . ox-feishu-link)
    (node-property . ox-feishu-undefined)
    (paragraph . ox-feishu-paragraph)
    (plain-list . ox-feishu-plain-list)
    (plain-text . ox-feishu-plain-text)
    (planning . ox-feishu-undefined)
    (property-drawer . ox-feishu-undefined)
    (quote-block . ox-feishu-quote-block)
    (radio-target . ox-feishu-undefined)
    (section . ox-feishu-section)
    (special-block . ox-feishu-undefined)
    (src-block . ox-feishu-geshi-block)
    (statistics-cookie . ox-feishu-undefined)
    (strike-through . ox-feishu-strike-through)
    (subscript . ox-feishu-undefined)
    (superscript . ox-feishu-undefined)
    (table . ox-feishu-table)
    (table-cell . ox-feishu-table-cell)
    (table-row . ox-feishu-table-row)
    (target . ox-feishu-undefined)
    (template . ox-feishu-template)
    (timestamp . ox-feishu-undefined)
    (underline . ox-feishu-underline)
    (verbatim . ox-feishu-verbatim)
    (verse-block . ox-feishu-undefined))
  :menu-entry
  '(?f "Export to Feishu"
       ((?B "As Feishu buffer" ox-feishu-export-as-feishu)
	    (?f "As Feishu file" ox-feishu-export-to-feishu)
	    (?b "As Feishu buffer and to clipboard" ox-feishu-export-to-kill-ring))))

;;; Helper methods

(defun ox-feishu--as-block (text)
  "Format TEXT as a block with leading and trailing newline."
  (concat "\n" text "\n"))

(defun ox-feishu--force-leading-newline (text)
  "Make TEXT start with exactly one newline."
  (replace-regexp-in-string "\\`\n*" "\n" text))

(defun ox-feishu--format-headline (text level)
  "Format TEXT as a headline of the given LEVEL."
  (let ((indent (cl-case level
		  (0 "")
		  (1 "# ")
		  (2 "== ")
		  (3 "+++ ")
		  (4 ":::: ")
		  (5 "----- ")
		  (t (user-error "Headline level `%s' is not defined yet" level)))))
    (concat
     (ox-feishu--put-in-tag
      "b" (ox-feishu--put-in-tag
	   "u" (concat indent text)))
     "\n\n")))

(defun ox-feishu--put-in-tag (tag contents &optional attributes)
  "Puts the BBcode tag TAG around the CONTENTS string.
Optional ATTRIBUTES for the tag can be given as an alist of
key/value pairs (both strings)."
  (let ((attribute-string (if attributes
			      (mapconcat (function (lambda (attribute)
						     (let ((key (car attribute))
							   (value (cadr attribute)))
						       (format " %s=\"%s\"" key value))))
					 attributes
					 "")
			    "")))
    (format "[%s%s]%s[/%s]" tag attribute-string contents tag)))

(defun ox-feishu--put-in-value-tag (tag contents value)
  "Puts the BBcode tag TAG around the CONTENTS string.
The VALUE is assigned directly to the tag instead of a normal
key/value pair."
  (format "[%s=%s]%s[/%s]" tag value contents tag))

(defun ox-feishu--fix-url (url)
  "Fix URL returned from `url-encode-url'.
Older versions of Emacs (eg. 24.3 used in the Travis CI minimal
image) prepend \"/\" to urls consisting only of an \"#anchor\"
part.  We don't want this, because we need relative anchors.  Fix
this the hard way."
  (if (string-prefix-p "/#" url)
      (substring url 1)
    url))

(defun ox-feishu--put-url (contents href)
  "Puts the CONTENTS inside a [url] tag pointing to HREF.
Automagically escapes the target URL."
  (let* ((target (ox-feishu--fix-url (url-encode-url (org-link-unescape href))))
	 (text   (or contents target)))
    (ox-feishu--put-in-value-tag "url" text target)))

(defun ox-feishu--remove-leading-newline (text)
  "Remove a leading empty line from TEXT."
  (replace-regexp-in-string "\\`\n" "" text))

(defun ox-feishu--remove-trailing-newline (text)
  "Remove the trailing newline from TEXT."
  (replace-regexp-in-string "\n\\'" "" text))

(defun ox-feishu--map-to-geshi-language (language)
  "Map LANGUAGE from Org to GeSHi."
  (cond ((string= language "elisp") "lisp")
	((string= language "shell") "bash")
	((string= language "sh")    "bash")
	((string= language "") "plaintext")
	(language)
	(t "plaintext")))

;;; Backend callbacks

(defun ox-feishu-bold (_bold contents _info)
  "Transcode a BOLD element from Org to Feishu.
CONTENTS is the bold text, as a string.  INFO is
  a plist used as a communication channel."
  (ox-feishu--put-in-tag "b" contents))

(defun ox-feishu-code (code _contents _info)
  "Transcode a CODE element from Org to Feishu.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  (ox-feishu--put-in-value-tag "font" (org-element-property :value code) "monospace"))

(defun ox-feishu-entity (entity _contents _info)
  "Transcode an ENTITY element from Org to Feishu.
CONTENTS is the definition itself.  INFO is a plist used as a
communication channel."
  (org-element-property :html entity))

(defun ox-feishu-geshi-block (code-block _contents info)
  "Transcode a CODE-BLOCK element from Org to Feishu GeSHi plugin.
CONTENTS is nil.  INFO is a plist holding
contextual information."
  (format "[geshi lang=%s]%s[/geshi]"
	  (ox-feishu--map-to-geshi-language (org-element-property :language code-block))
	  (ox-feishu--remove-trailing-newline
	   (org-export-format-code-default code-block info))))

(defun ox-feishu-fixed-width (fixed-width _contents _info)
  "Transcode a FIXED-WIDTH element from Org to Feishu.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (ox-feishu--put-in-tag "code"
		     (concat "\n" (org-element-property :value fixed-width))))

(defun ox-feishu-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to Feishu.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (eq (org-element-property :type footnote-reference) 'inline)
      (user-error "Inline footnotes not supported yet")
    (let ((n (org-export-get-footnote-number footnote-reference info)))
      (format "^%d " n))))

(defun ox-feishu-format-footnote-definition (fn)
  "Format the footnote definition FN."
  (let ((n (car fn))
	(def (cdr fn)))
    (format "^%d: %s" n def)))

(defun ox-feishu-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((org-major (string-to-number (car (split-string (org-release) "\\."))))
	 (fn-alist (if (> org-major 8)
		       (org-export-collect-footnote-definitions
			info (plist-get info :parse-tree))
		     (org-export-collect-footnote-definitions
		      (plist-get info :parse-tree) info)))
	 (fn-alist
	  (cl-loop for (n _label raw) in fn-alist collect
		   (cons n (org-trim (org-export-data raw info)))))
	 (text (mapconcat #'ox-feishu-format-footnote-definition fn-alist "\n")))
    (if fn-alist
	(concat "\n" (ox-feishu--format-headline "Footnotes" 0) text)
      "")))

(defun ox-feishu-headline (headline contents info)
  "Transcode HEADLINE element from Org to Feishu.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (let ((title (org-export-data (org-element-property :title headline) info))
	(level (org-export-get-relative-level headline info)))
    (if (org-element-property :footnote-section-p headline)
	""
      (concat
       (ox-feishu--format-headline title level)
       contents))))

(defun ox-feishu-inner-template (contents info)
  "Return body of document string after Feishu conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   contents
   (ox-feishu-footnote-section info)))

(defun ox-feishu-italic (_italic contents _info)
  "Transcode a ITALIC element from Org to Feishu.
CONTENTS is the italic text, as a string.  INFO is
  a plist used as a communication channel."
  (ox-feishu--put-in-tag "i" contents))

(defun ox-feishu-item (item contents info)
  "Transcode a ITEM element from Org to Feishu.
CONTENTS is the contents of the item, as a string.  INFO is
  a plist used as a communication channel."
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list))
	 (text (org-trim contents)))
    (concat
     "[*]"
     (pcase type
       (`descriptive
	(let ((term (let ((tag (org-element-property :tag item)))
		      (and tag (org-export-data tag info)))))
	  (concat
	   (ox-feishu--put-in-tag "i" (concat (org-trim term) ":"))
	   " "))))
     text
     "\n")))

(defun ox-feishu-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to Feishu.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  "[br]_[/br]\n")

(defun ox-feishu-link (link contents _info)
  "Transcode a LINK element from Org to Feishu.
CONTENTS is the contents of the link, as a string.  INFO is
  a plist used as a communication channel."
  (let ((type (org-element-property :type link))
	(path (org-element-property :path link))
	(raw  (org-element-property :raw-link link)))
    (cond
     ((string= type "fuzzy")
      (cond
       ((string-prefix-p "about:" raw)
	(ox-feishu--put-url contents raw))
       (t (user-error "Unknown fuzzy LINK type encountered: `%s'" raw))))
     ((member type '("http" "https"))
      (ox-feishu--put-url contents (concat type ":" path)))
     (t (user-error "LINK type `%s' not yet supported" type)))))

(defun ox-feishu-paragraph (_paragraph contents _info)
  "Transcode a PARAGRAPH element from Org to Feishu.
CONTENTS is the contents of the paragraph, as a string.  INFO is
  a plist used as a communication channel."
  (org-trim contents))

(defun ox-feishu-plain-list (plain-list contents _info)
  "Transcode a PLAIN-LIST element from Org to Feishu.
CONTENTS is the contents of the plain-list, as a string.  INFO is
  a plist used as a communication channel."
  (let ((type (org-element-property :type plain-list))
	(content-block (ox-feishu--as-block (org-trim contents))))
    (concat
     (pcase type
       (`descriptive (ox-feishu--put-in-tag "list" content-block))
       (`unordered (ox-feishu--put-in-tag "list" content-block))
       (`ordered (ox-feishu--put-in-value-tag "list" content-block "1"))
       (other (user-error "PLAIN-LIST type `%s' not yet supported" other)))
     "\n")))

(defun ox-feishu-plain-text (text _info)
  "Transcode a TEXT string from Org to Feishu.
INFO is a plist used as a communication channel."
  text)

(defun ox-feishu-quote-block (_quote-block contents _info)
  "Transcode a QUOTE-BLOCK element from Org to Feishu.
CONTENTS holds the contents of the block.  INFO is a plist used
as a communication channel."
  (ox-feishu--put-in-tag "quote" (ox-feishu--force-leading-newline contents)))

(defun ox-feishu-section (_section contents _info)
  "Transcode a SECTION element from Org to Feishu.
CONTENTS is the contents of the section, as a string.  INFO is a
  plist used as a communication channel."
  (org-trim contents))

(defun ox-feishu-strike-through (_strike-through contents _info)
  "Transcode a STRIKE-THROUGH element from Org to Feishu.
CONTENTS is the text with strike-through markup, as a string.
  INFO is a plist used as a communication channel."
  (ox-feishu--put-in-tag "s" contents))

(defun ox-feishu-table (_table contents _info)
  "Transcode a TABLE element from Org to Feishu.
CONTENTS contains the already rendered body of the table.  INFO
is a plist used as a communication channel."
  (ox-feishu--put-in-tag "table" contents))

(defun ox-feishu-table-row (_table-row contents _info)
  "Transcode a TABLE-ROW element from Org to Feishu.
CONTENTS contains the already rendered row content.  INFO is a
plist used as a communication channel."
  (if contents
      (ox-feishu--put-in-tag "tr" contents)
    ""))

(defun ox-feishu-table-cell (_table-cell contents _info)
  "Transcode a TABLE-CELL element from Org to Feishu.
CONTENTS contains the already rendered cell content.  INFO is a
plist used as a communication channel."
  (ox-feishu--put-in-tag "td" contents))

(defun ox-feishu-template (contents _info)
  "Return complete document string after Feishu conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  contents)

(defun ox-feishu-undefined (element &optional _contents _info)
  "Throw an error when an unsupported ELEMENT is encountered."
  (user-error "ELEMENT type `%s' not implemented yet" (car element)))

(defun ox-feishu-underline (_underline contents _info)
  "Transcode a UNDERLINE element from Org to Feishu.
CONTENTS is the underlined text, as a string.  INFO is
  a plist used as a communication channel."
  (ox-feishu--put-in-tag "u" contents))

(defun ox-feishu-verbatim (verbatim _contents _info)
  "Transcode a VERBATIM element from Org to Feishu.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  (ox-feishu--put-in-value-tag "font" (org-element-property :value verbatim) "monospace"))

;;; Export methods

;;;###autoload
(defun ox-feishu-export-as-feishu
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Feishu buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

TODO: document BODY-ONLY

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org Feishu Export*\"."
  (interactive)
  (org-export-to-buffer 'feishu "*Org Feishu Export*"
    async subtreep visible-only body-only ext-plist))

;;;###autoload
(defun ox-feishu-export-to-feishu
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Feishu file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

TODO: document BODY-ONLY

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension ".feishu")
	     (file (org-export-output-file-name extension subtreep))
	     (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'feishu file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun ox-feishu-export-to-kill-ring
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Feishu buffer and kill ring.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

TODO: document BODY-ONLY

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org Feishu Export*\" which is
automatically copied to the kill ring (Clipboard)."
  (interactive)
  (let ((oldval org-export-copy-to-kill-ring))
    (progn
      (setq org-export-copy-to-kill-ring t)
      (ox-feishu-export-as-feishu async subtreep visible-only body-only ext-plist)
      (setq org-export-copy-to-kill-ring oldval))))

;;; Register file

(provide 'ox-feishu)

;;; ox-feishu.el ends here
