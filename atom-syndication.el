;;; atom-syndication.el --- Elisp implementation of the atom syndication format.
;;
;; Author: David Maus <dmaus@ictsoc.de>
;; Keywords: standards, hypermedia
;;
;; Copyright (C) 2010 by David Maus
;;
;; This file is NOT part of Gnu Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; * About
;;
;; This library implements the atom syndication format as specified in
;; RFC 4287.  It provides a set of functions neccessary to create an
;; atom feed that complies as close as possible to the specs.
;;
;;; Code:
(defconst atom-syndication-attribute-spec-alist
  '((href . (".+"))
    (hreflang . ("^[[:alpha:]]\\{1,8\\}\\(-[[:alnum:]]\\{1,8\\}\\)*$"))
    (label . (".+"))
    (length . (".+"))
    (rel . (".+"))
    (scheme . (".+"))
    (src . (".+"))
    (term . (".+"))
    (title . (".+"))
    (type . ("^text$" "^html$" "^xhtml$" "^.+/.+$"))
    (uri . (".+"))
    (version  . (".+"))
    (xml:lang . ("^[[:alpha:]]\\{1,8\\}\\(-[[:alnum:]]\\{1,8\\}\\)*$"))
    (xml:base . (".+"))
    (xmlns . (".+")))
  "Alist with atom attributes.

Each cell is a cons with the attribute name in car and a list of
regular expressions in cdr.  For a value of to be considered
valid for the attribute in question at least one regular
expression must match a given value.")

(defconst atom-syndication-element-spec-alist
  '((category (term scheme label xml:lang xml:base) nil)
    (content (type src xml:lang xml:base) (".*"))
    (contributor () (".+"))
    (name () (".+"))
    (email () (".+"))
    (uri () (".+"))
    (author () (".+"))
    (generator (version uri xml:lang xml:base) (".+"))
    (icon (xml:lang xml:base) (".+"))
    (id (xml:lang xml:base) (".+"))
    (link (href rel type hreflang title length xml:lang xml:base) nil)
    (logo (xml:lang xml:base) (".+"))
    (published (xml:lang xml:base) (".+"))
    (rights (xml:lang xml:base) (".+"))
    (updated (xml:lang xml:base) (".+"))
    (subtitle (xml:lang xml:base) (".+"))
    (summary (xml:lang xml:base) (".+"))
    (title (type xml:lang xml:base) (".+"))
    (feed (xmlns xml:lang xml:base) (".+"))
    (entry (xml:lang xml:base) (".+")))
  "Alist with atom elements.

Each cell is a list with the element name as first and a list
of valid attributes as second element.  The third element is a
list of regular expressions for which at least one must match for
a given value to be considered valid.  If the element does not
take a value the list is empty.")

(defconst atom-syndication-container-spec-alist
  '((entry .
     ((author nil t)
      (category nil t)
      (content nil nil)
      (contributor nil t)
      (id t nil)
      (link nil t)
      (published nil nil)
      (rights nil nil)
      (source nil nil)
      (summary nil nil)
      (title t nil)
      (updated t nil)))
    (feed .
     ((author nil t)
      (category nil t)
      (contributor nil t)
      (generator nil nil)
      (icon nil nil)
      (link nil t)
      (logo nil nil)
      (rights nil nil)
      (subtitle nil nil)
      (title t nil)
      (updated t nil)
      (id t nil)))
    (source .
     ((author nil t)
      (category nil t)
      (generator nil nil)
      (icon nil nil)
      (id nil nil)
      (link nil t)
      (logo nil nil)
      (rights nil nil)
      (subtitle nil nil)
      (title nil nil)
      (updated nil nil))))
  "Alist with atom container elements.

Each cell is a cons with the container element name in car and an
alist with required and optional elements in cdr.  For each
required or optional element the first value (i.e. second cell of
the correspoding list) is t if the element is mandatory, nil
otherwise.  The second value (i.e. third cell of the correspoding
list) is t if the element can occur multiple times including zero
or nil otherwise.")

(defcustom atom-syndication-attribute-xtra-alist nil
  "Alist with additional attributes.

Same format as `atom-syndication-attribute-spec-alist'.  Both
lists are combined and the value lists of attributes appearing in
both lists are appended to each other."
  :group 'atom-syndication
  :type 'alist)

(defcustom atom-syndication-element-xtra-alist nil
  "Alist with additional elements.

Same format as `atom-syndication-element-spec-alist'.  Both lists
are combined and the value lists of attributes appearing in both
lists are appended to each other."
  :group 'atom-syndication
  :type 'alist)

(defcustom atom-syndication-container-xtra-alist nil
  "Alist with additional container elements.

Same format as `atom-syndication-container-spec-alist'.  Both
lists are combined and the values are combined using the logical
'or'."
  :group 'atom-syndication
  :type 'alist)

(defcustom atom-syndication-construct-text-html-function
  'atom-syndication-sanitize
  "Function to create html markup for text constructs.

The function is called with the text as argument and must return
the html encoded version with & and < replaced by their html
entities &amp; and &lt; respectively."
  :group 'atom-syndication
  :type 'function)

(defcustom atom-syndication-construct-text-xhtml-function
  'atom-syndication-simple-xhtml
  "Function to create xhtml markup for text constructs.

The function is called with the text as argument and must return
the xhtml encoded version.  For more information on the
neccessary encoding for xhtml text constructs see RFC4287,
section 3.1.1.3."
  :group 'atom-syndication
  :type 'function)

;;;; atom container elements
(defun atom-syndication-element-entry (attr elements)
  "Return atom entry element.

ELEMENTS is a list of metadata elements for entry.
Optional argument ATTR is an alist with additional attributes."
  (atom-syndication-container 'entry attr elements))

(defun atom-syndication-element-feed (attr elements)
  "Return atom feed element.

ELEMENTS is a list of metadata elements for feed.
Optional argument ATTR is an alist with additional attributes."
  (unless (memq (cons 'xmlns "http://www.w3.org/2005/Atom") attr)
    (setq attr (append
		(list (cons 'xmlns "http://www.w3.org/2005/Atom")) attr)))
  (atom-syndication-container 'feed attr elements))

(defun atom-syndication-element-source (attr elements)
  "Return atom source element.

ELEMENTS is a list of metadata elements for source.
Optional argument ATTR is an alist with additional attributes."
  (apply 'atom-syndication-container 'source elements attr))

(defun atom-syndication-container (which attr elements)
  "Return atom container.

WHICH is the symbol for the desired container element.
ELEMENTS is an alist with container's elements, values and
attributes.
Optional argument ATTR is an alist of attributes for the
container element."
  (let ((spec (assoc which (atom-syndication-combine-alists
			    atom-syndication-container-spec-alist
			    atom-syndication-container-xtra-alist
			    '(lambda (a b)
			       (if (or (eq a nil) (eq a t))
				   (or a b)
				 a)))))
	value)
    (unless spec
      (error "Invalid atom container element: %s" which))
    ;; check spec
    (dolist (spec_elm (cdr spec))
      (let ((nr (atom-syndication-count-keys-alist (car spec_elm) elements)))
	;; check if mandatory
	(when (and (nth 1 spec_elm) (= nr 0))
	  (error "Missing mandatory element for container: %s, %s"
		 (car spec_elm) which))
	;; check multiple occurences
	(when  (and (not (nth 2 spec_elm)) (> nr 1))
	  (error "Too much elements for container: %s, %s"
		 (car spec_elm) which))))
    ;; create container
    ;; FIXME
    (atom-syndication-element which attr elements)))

(defun atom-syndication-element-content (attr value
					      &optional type src)
  "Return atom content element.

VALUE is the content.
Optional argument TYPE is the content type.  If ommited, type
defaults to text.
Optional argument SRC is the url of the content.
Optional argument ATTR is an alist of additional attributes."
  (when (and src value)
    (error "Content element with src attribute must be empty: %s, %s"
	   src value))
  (when type
    (setq attr (append (list (cons 'type type)) attr)))
  (when src (setq attr (append (list (cons 'src src)) attr)))
  (atom-syndication-element 'content
	 attr
	 (if (member type '(text html xhtml))
	     (atom-syndication-construct-text value type)
	   (if value (atom-syndication-sanitize value) value))))

;;;; atom constructs
(defun atom-syndication-construct-person (name &optional email uri)
  "Return atom person construct.

NAME is the name of the person.
Optional argument EMAIL is the person's email address.
Optional argument URI is a uri."
  (concat
   (atom-syndication-element-name name)
   (when email (atom-syndication-element 'email email))
   (when uri (atom-syndication-element 'uri uri))))

(defun atom-syndication-construct-date (date)
  "Return atom date construct for DATE."
  (let* ((system-time-locale "C")
	 (zone (format-time-string "%z")))
    (concat
     (format-time-string "%Y-%m-%dT%H:%M:%S" date)
     (substring zone 0 3)
     ":"
     (substring zone 3))))

(defun atom-syndication-construct-text (text &optional type)
  "Return atom text construct for TEXT.

Optional argument TYPE can be the symbol 'text plain
text, 'html for html or 'xhtml for xhtml content.  If TYPE
is ommitted it defaults to 'text.  To create html and xhtml
content the functions in
`atom-syndication-construct-text-html-function' and
`atom-syndication-construct-text-xhtml-function' are called."
  (cond
   ((or (eq type nil) (eq type 'text))
    (atom-syndication-sanitize text))
   ((or (eq type 'html))
    (funcall atom-syndication-construct-text-html-function text))
   ((or (eq type 'xhtml))
    (funcall atom-syndication-construct-text-xhtml-function text))
   (t
    (error "Invalid type for text construct: %s" type))))

;;;; atom metadata elements
(defun atom-syndication-element-author (attr name &optional email uri)
  "Return atom author element.

NAME is the name of the author.
Optional argument EMAIL is the author's email address.
Optional argument URI is a uri.
Optional argument ATTR is an alist of additional attribues."
  (atom-syndication-element 'author
	 attr
	 (atom-syndication-construct-person name email uri)))

(defun atom-syndication-element-contributor (attr name
					     &optional email uri)
  "Return atom contributor element.

NAME is the name of the contributor.
Optional argument EMAIL is the contributor's email address.
Optional argument URI is a uri.
Optional argument ATTR is an alist of additional attribues."
  (atom-syndication-element 'contributor
	 attr
	 (atom-syndication-construct-person name email uri)))

(defun atom-syndication-element-summary (attr summary)
  "Return summary elment.

SUMMARY is a summary, abstract, or excerpt of an entry.
Optional argument ATTR is an alist of additional attributes."
  (let ((summary (atom-syndication-construct-text
		  summary (cdr (assoc 'type attr)))))
    (atom-syndication-element 'summary attr summary)))

(defun atom-syndication-element-subtitle (attr subtitle)
  "Return subtitle element.

SUBTITLE is a readable description or subtitle for a feed.
Optional argument ATTR is an alist of additional attributes."
  (let ((subtitle (atom-syndication-construct-text
		   subtitle (cdr (assoc 'type attr)))))
    (atom-syndication-element 'subtitle attr subtitle)))

(defun atom-syndication-element-rights (attr rights)
  "Return rights element.

RIGHTS is a string that that conveys information about rights held
in and over an entry or feed.
Optional argument ATTR is an alist of additional attributes."
  (let ((rights (atom-construct-text text (cdr (assoc 'type attr)))))
    (atom-syndication-element 'rights rights attr)))

(defun atom-syndication-element-logo (attr uri)
  "Return logo element.

URI is the url pointing to a logo for the feed.
Optional argument ATTR is an alist with additional attributes."
  (atom-syndication-element 'logo attr logo))

(defun atom-syndication-element-icon (attr uri)
  "Return icon element.

URI is the url pointing to an icon for the feed.
Optional argument ATTR is an alist with additional attributes."
  (atom-syndication-element 'icon attr icon))

(defun atom-syndication-element-contributor (attr name
					     &optional email uri)
  "Return contributer element.

NAME is the name of the contributer.
Optional argument EMAIL is the contributor's email address.
Optional argument URI is a uri.
Optional argument ATTR is an alist with additional attributes."
  (atom-syndication-element 'contributor
			    attr
			    (atom-syndication-construct-person name email uri)))

(defun atom-syndication-element-category (attr term
					  &optional scheme label)
  "Return category element.

TERM is a string that identifies the category to which the entry
or feed belongs.
Optional argument SCHEME is an IRI that identifies a
categorization scheme.
Optional argument LABEL provides a human-readable label for
display in end-user applications.
Optional argument ATTR is an alist with additional attributes."
  (when scheme (setq attr (append (list (cons 'scheme scheme)) attr)))
  (when label (setq attr (append (list (cons 'label label)) attr)))
  (setq attr (append (list (cons 'term term)) attr))
  (atom-syndication-element 'category attr nil))

(defun atom-syndication-element-title (attr title)
  "Return title metadata element.

TITLE is a string with the title.
Optional argument ATTR is an alist of atom attributes."
  (let ((title (atom-syndication-construct-text
		title (cdr (assoc 'type attr)))))
    (atom-syndication-element 'title attr title)))

(defun atom-syndication-element-updated (attr datetime)
  "Return updated metadata element.

DATETIME is a elisp time object.
Optional paramter ATTR is an alist of atom attributes."
  (let ((updated (atom-syndication-construct-date datetime)))
    (atom-syndication-element 'updated attr updated)))

(defun atom-syndication-element-published (attr datetime)
  "Return published metadata element.

DATETIME is a elisp time object.
Optional paramter ATTR is an alist of atom attributes."
  (let ((published (atom-syndication-construct-date datetime)))
    (atom-syndication-element 'published attr published)))

(defun atom-syndication-element-name (attr name)
  "Return name metadata element.

NAME is a person's name.
Optional argument ATTR is an alist of atom attributes."
  (atom-syndication-element 'name attr name))

(defun atom-syndication-element-email (attr email)
  "Return email metadata element.

EMAIL is an email address.
Optional argument ATTR is an alist of atom attributes."
  (atom-syndication-element 'email attr email))

(defun atom-syndication-element-id (attr id &rest)
  "Return id metadata element.

ID is a string with a unique identifier.
Optional argument ATTR is an alist of atom attributes."
  (atom-syndication-element 'id attr id))

(defun atom-syndication-element-link (attr href
				      &optional title rel type length hreflang)
  "Return link metadata element.

HREF is a string with the link target.
Optional argument TITLE is the link's title.
Optional argument REL is a string that indicates the link
relation type.
Optional argument TYPE is a symbol of the advisory media type
of the target.
Optional argument LENGTH is a string that indicates an advisory
length of the linked content in octets
Optional argument HREFLANG is a string that describes the
language of the resource pointed to by HREF.
Optional argument ATTR is an alist of atom attributes."
  (when title
    (setq attr (append (list (cons 'title title)) attr)))
  (when rel
    (setq attr (append (list (cons 'rel rel)) attr)))
  (when type
    (unless (string-match-p ".+/.+" (format "%s" type))
      (error "Invalid type for link element: %s" type))
    (setq attr (append (list (cons 'type type)) attr)))
  (when length
    (setq attr (append (list (cons 'length length)) attr)))
  (when hreflang
    (setq attr (append (list (cons 'hreflang hreflang)) attr)))
  (setq attr (append (list (cons 'href href)) attr))
  (atom-syndication-element 'link attr nil))

(defun atom-syndication-element-generator (attr name
					   &optional version uri)
  "Return generator metadata element.

NAME is the name of the generator.
Optional argument VERSION is the version string.
Optional argument URI is a url.
Optional argument ATTR is an alist of atom attributes."
  (when version
    (setq attr (append (list (cons 'version version)) attr)))
  (when uri
    (setq attr (append (list (cons 'uri uri)) attr)))
  (atom-syndication-element 'generator attr name))

(defun atom-syndication-element (name attr &optional value)
  "Return metadata element string.

NAME is symbol of element name.
ATTR is a list of cons with atom attributes, key in car and value
in cdr.
VALUE is string or symbol with element value."
  (let* ((spec (assoc name
		      (atom-syndication-combine-alists
		       atom-syndication-element-spec-alist
		       atom-syndication-element-xtra-alist)))
	 (spec_attr (nth 1 spec))
	 (spec_chkl (nth 2 spec)))
    (unless spec
      (error "Invalid metadata element: %s" name))
    ;; check attributes
    (when attr
      (dolist (a attr)
	(unless (memq (car a) spec_attr)
	  (error "Invalid attribute for element: %s, %s" (car a) name))))
    ;; check value
    (when (stringp value)
      (unless
	  (if (eq spec_chkl nil) (eq value nil)
	    (catch 'valid
	      (mapc '(lambda (pattern)
		       (when (string-match-p pattern (or value ""))
			 (throw 'valid t)))
		    spec_chkl)
	      nil))
	(error "Invalid value for element: %s, %s" value name)))
    (concat
     "<" (symbol-name name)
     (if attr (concat
	       " "
	       (mapconcat 'atom-syndication-attribute attr " ")))
     (if value
	 (concat ">"
		 (if (stringp value) value
		   (mapconcat
		    '(lambda (v)
		       (apply
			(intern
			 (format "atom-syndication-element-%s"
				 (car v)))
			(cdr v))) value ""))
		 "</" (symbol-name name))
       " /")
     ">")))

(defun atom-syndication-attribute (attr)
  "Return attribute string for ATTR.

ATTR is a cons with the attribute name in car and the value in
cdr."
  (let* ((name (car attr))
	 (value (format "%s" (cdr attr)))
	 (spec (assoc name (atom-syndication-combine-alists
			    atom-syndication-attribute-spec-alist
			    atom-syndication-attribute-xtra-alist))))
    (unless spec
      (error "Unknown attribute: %s" name))
    (unless
	(catch 'valid
	  (mapc '(lambda (pattern)
		   (when (string-match-p pattern value)
		     (throw 'valid t)))
		(cdr spec))
	  nil)
      (error "Invalid attribute value: %s, %s" value name))
    (format "%s=\"%s\"" name (atom-syndication-sanitize value))))

;;;; Misc functions
(defun atom-syndication-sanitize (text)
  "Sanitize TEXT for xml output.

Replace & and < by their html encoding entities."
  (replace-regexp-in-string
   "<" "&lt;"
   (replace-regexp-in-string
    "&" "&amp;" text)))

(defun atom-syndication-simple-xhtml (text)
  "Return xhtml markup of TEXT.

This function does no encoding whatsoever, just encloses TEXT
in a <xhtml:div> element."
  (concat
   "<xhtml:div>" (atom-syndication-sanitize text) "</xhtml:div>"))

(defun atom-syndication-count-keys-alist (key alist)
  "Return number of cells with KEY as car in ALIST."
  (let ((blist (assq-delete-all key (copy-alist alist))))
    (- (length alist) (length blist))))

(defun atom-syndication-combine-alists (a b &optional mergefunc)
  "Return combined alist of A and B.

A and B are alists with car as key and one or more lists as
values in cdr.  The combined list is an alist with all keys of A
and B.

Optional argument MERGEFUNC is the symbol of a function that
merges the values of the value lists.  The function must take the
two values as arguments and return whatever should be considered
a merge of the values.  All values of the value lists in A and B
are processed sequentially. missing values are passed as the
symbol nil to MERGEFUNC.  If MERGEFUNC is ommitted, the the value
lists are not processed but simply appended."
  (let ((list_a (copy-alist a))
	(list_b (copy-alist b))
	list_c def_a)
    ;; process all keys in list a
    (while (setq def_a (pop list_a))
      (let* ((key (pop def_a))
	     (def_b (cdr (assoc key list_b))))
	;; maybe remove key from list_b
	(setq list_b (assq-delete-all key list_b))
	(setq def_a
	      (mapcar '(lambda (val_a)
			 (if (not mergefunc)
			     (append (pop def_b) val_a)
			   (let* ((val_b (pop def_b))
				  (vlst_a (if (> (length val_a) (length val_b))
					      val_a val_b))
				  (vlst_b (if (> (length val_a) (length val_b))
					      val_b val_a)))
			     (mapcar '(lambda (val)
					(funcall mergefunc val (pop vlst_b)))
				     vlst_a)))) def_a))
	(setq def_a (append def_b def_a))
	(push (cons key def_a) list_c)))
    (setq list_c (append list_b list_c))
    list_c))

(eval-when-compile (require 'cl))

(provide 'atom-syndication)

;;; atom-syndication.el ends here
