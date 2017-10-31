;;; ox-s9y.el --- Serendipity HTML Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; TODO:
;; - export directly to X clipboard: export to buffer; select all; copy to clipboard; close buffer

;; see http://orgmode.org/worg/dev/org-element-api.html

(require 'ox)

(org-export-define-backend 's9y
  '((bold . org-s9y-bold)
    (center-block . org-s9y-undefined)
    (clock . org-s9y-undefined)
    (code . org-s9y-code)
    (drawer . org-s9y-undefined)
    (dynamic-block . org-s9y-undefined)
    (entity . org-s9y-undefined)
    (example-block . org-s9y-undefined)
    (export-block . org-s9y-undefined)
    (export-snippet . org-s9y-undefined)
    (fixed-width . org-s9y-undefined)
    (footnote-definition . org-s9y-undefined)
    (footnote-reference . org-s9y-undefined)
    (headline . org-s9y-headline)
    (horizontal-rule . org-s9y-undefined)
    (inline-src-block . org-s9y-undefined)
    (inlinetask . org-s9y-undefined)
    (italic . org-s9y-italic)
    (item . org-s9y-item)
    (keyword . org-s9y-undefined)
    (latex-environment . org-s9y-undefined)
    (latex-fragment . org-s9y-undefined)
    (line-break . org-s9y-undefined)
    (link . org-s9y-link)
    (node-property . org-s9y-undefined)
    (paragraph . org-s9y-paragraph)
    (plain-list . org-s9y-plain-list)
    (plain-text . org-s9y-plain-text)
    (planning . org-s9y-undefined)
    (property-drawer . org-s9y-undefined)
    (quote-block . org-s9y-undefined)
    (radio-target . org-s9y-undefined)
    (section . org-s9y-section)
    (special-block . org-s9y-undefined)
    (src-block . org-s9y-geshi-block)
    (statistics-cookie . org-s9y-undefined)
    (strike-through . org-s9y-undefined)
    (subscript . org-s9y-undefined)
    (superscript . org-s9y-undefined)
    (table . org-s9y-undefined)
    (table-cell . org-s9y-undefined)
    (table-row . org-s9y-undefined)
    (target . org-s9y-undefined)
    (template . org-s9y-template)
    (timestamp . org-s9y-undefined)
    (underline . org-s9y-undefined)
    (verbatim . org-s9y-undefined)
    (verse-block . org-s9y-undefined))
  :menu-entry
  '(?S "Export to Serendipity"
       ((?H "As HTML buffer" org-s9y-export-as-html)
	(?h "As HTML file" org-s9y-export-to-html))))

(defun org-s9y--put-in-tag (tag contents &optional parameters)
  "Puts the HTML tag TAG around the CONTENTS string.  Optional
PARAMETERS for the tag can be given as a string."
  (if (org-string-nw-p parameters)
      (format "<%s %s>%s</%s>" tag parameters contents tag)
    (format "<%s>%s</%s>" tag contents tag)))

(defun org-s9y-bold (bold contents info)
  "Transcode a BOLD element from Org to Serendipity.
CONTENTS is the bold text, as a string.  INFO is
  a plist used as a communication channel."
  (org-s9y--put-in-tag "strong" contents))

(defun org-s9y-code (code contents info)
  "Transcode a CODE element from Org to Serendipity.
CONTENTS nil.  INFO is a plist used as a communication channel."
  (org-s9y--put-in-tag "code" (org-element-property :value code)))

(defun org-s9y-geshi-block (code-block _contents info)
  "Transcode a CODE-BLOCK element from Org to Serendipity GeSHi plugin.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format "[geshi lang=\"%s\"]
%s[/geshi]"
	  (org-element-property :language code-block)
	  (org-export-format-code-default code-block info)))

(defun org-s9y-headline (headline contents info)
  "Transcode HEADLINE element from Org to Serendipity.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (let ((title (org-export-data (org-element-property :title headline) info))
	(level (org-export-get-relative-level headline info)))
    (concat
     (if (<= level 2)
	 (format "<!--  %s  -->" title)
       (org-s9y--put-in-tag (format "h%d" level) title))
     "\n"
     contents)))

(defun org-s9y-italic (italic contents info)
  "Transcode a ITALIC element from Org to Serendipity.
CONTENTS is the italic text, as a string.  INFO is
  a plist used as a communication channel."
  (org-s9y--put-in-tag "em" contents))

(defun org-s9y-item (item contents info)
  "Transcode a ITEM element from Org to Serendipity.
CONTENTS is the contents of the item, as a string.  INFO is
  a plist used as a communication channel."
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list)))
    (concat
     (pcase type
					; (`descriptive - tag comes from where?
       (other (org-s9y--put-in-tag "li" (org-trim contents))))
     "\n")))

(defun org-s9y-link (link contents info)
  "Transcode a LINK element from Org to Serendipity.
CONTENTS is the contents of the link, as a string.  INFO is
  a plist used as a communication channel."
  (let ((type (org-element-property :type link))
	(path (org-element-property :path link)))
    (cond
     ((string= type "todo")
      (org-s9y--put-in-tag "abbr" contents "title=\"Artikel folgt\""))
     (t (org-s9y--put-in-tag "a" contents (format "href=\"%s:%s\"" type path))))))

(defun org-s9y-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Serendipity.
CONTENTS is the contents of the paragraph, as a string.  INFO is
  a plist used as a communication channel."
  (org-trim contents))

(defun org-s9y-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Serendipity.
CONTENTS is the contents of the plain-list, as a string.  INFO is
  a plist used as a communication channel."
  (let ((type (org-element-property :type plain-list)))
    (concat
     (pcase type
       (`unordered (org-s9y--put-in-tag "ul" (org-trim contents)))
					; `ordered `descriptive
       (other (error "PLAIN-LIST type %s not yet supported" other)))
     "\n")))

(defun org-s9y-plain-text (text info)
  "Transcode a TEXT string from Org to Serendipity.
INFO is a plist used as a communication channel."
  text)

(defun org-s9y-section (_section contents _info)
  "Transcode a SECTION element from Org to Serendipity.
CONTENTS is the contents of the section, as a string.  INFO is a
  plist used as a communication channel."
  (org-s9y--put-in-tag "p" (org-trim contents)))

(defun org-s9y-template (contents info)
  "Return complete document string after Serendipity conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  contents)

(defun org-s9y-undefined (element &optional _contents _info)
  (error "element type `%s' not implemented yet" (car element)))

;;;###autoload
(defun org-s9y-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Serendipity HTML buffer.

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

Export is done in a buffer named \"*Org S9Y Export*\"."
  (interactive)
  (org-export-to-buffer 's9y "*Org S9Y Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (html-mode))))

;;;###autoload
(defun org-s9y-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an Serendipity HTML file.

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
  (let ((extension (concat "." (or (plist-get ext-plist :html-extension)
				   org-html-extension
				   "html")))
	(file (org-export-output-file-name extension subtreep))
	(org-export-coding-system org-html-coding-system))
    (org-export-to-file 's9y file
      async subtreep visible-only body-only ext-plist)))