;;; -*- Gerbil -*-
;;; (C) Daniel Szmulewicz
;;; gumbo ffi

;; compile: -ld-options "-lgumbo"

(import :std/srfi/1 :std/sugar)
(export html->sxml)

(extern
  gumbo-parse
  gumbo-destroy-output!
  gumbo-default-options
  gumbo-root
  gumbo-document
  gumbo-node-type
  gumbo-document-children
  gumbo-vector-length
  gumbo-vector-ref
  gumbo-vector-capacity
  gumbo-element-children
  gumbo-element-attributes
  gumbo-element-tag
  gumbo-element-original-tag-string-piece
  gumbo-attribute-name
  gumbo-attribute-value
  gumbo-attribute
  gumbo-node-text
  gumbo-normalized-tagname
  gumbo-tag-from-original-text
  GUMBO_NODE_DOCUMENT
  GUMBO_NODE_ELEMENT
  GUMBO_NODE_TEXT
  GUMBO_NODE_CDATA
  GUMBO_NODE_COMMENT
  GUMBO_NODE_WHITESPACE
  GUMBO_NODE_TEMPLATE
  GUMBO_TAG_UNKNOWN)

(begin-foreign
  (namespace ("danielsz/gumbo#"))
  (##namespace ("" define define-macro c-declare c-define-type c-lambda))

  (c-declare "#include <gumbo.h>")

  (define-macro (define-const symbol)
    (let* ((str (symbol->string symbol))
	   (ref (string-append "___return (" str ");")))
      `(define ,symbol
	 ((c-lambda () int ,ref)))))
  (define-macro (define-c-lambda id args ret #!optional (name #f))
    (let ((name (or name (##symbol->string id))))
      `(define ,id
	 (c-lambda ,args ,ret ,name))))

  (define-const GUMBO_NODE_DOCUMENT)
  (define-const GUMBO_NODE_ELEMENT)
  (define-const GUMBO_NODE_TEXT)
  (define-const GUMBO_NODE_CDATA)
  (define-const GUMBO_NODE_COMMENT)
  (define-const GUMBO_NODE_WHITESPACE)
  (define-const GUMBO_NODE_TEMPLATE)
  (define-const GUMBO_TAG_UNKNOWN)

  (c-define-type GumboOptions*
    (pointer "GumboOptions" (*GumboOptions)))
  (c-define-type GumboOutput*
    (pointer "GumboOutput" (*GumboOutput)))
  (c-define-type GumboVector*
    (pointer "GumboVector" (*GumboVector)))
  (c-define-type GumboNode*
    (pointer "GumboNode" (*GumboNode)))
  (c-define-type GumboAttribute*
    (pointer "GumboAttribute" (*GumboAttribute)))
  (c-define-type GumboStringPiece*
    (pointer "GumboStringPiece" (*GumboStringPiece)))
  (c-define-type GumboText*
    (pointer "GumboText" (*GumboText)))
  (c-define-type GumboElement*
    (pointer "GumboElement" (*GumboElement)))

  (define-c-lambda gumbo-parse (UTF-8-string) GumboOutput*
    "gumbo_parse")
  (define-c-lambda gumbo-destroy-output! (GumboOutput*) void
    "gumbo_destroy_output(&kGumboDefaultOptions,___arg1);")
  (define-c-lambda gumbo-root (GumboOutput*) GumboNode*
    "___return (___arg1->root);")
  (define-c-lambda gumbo-document (GumboOutput*) GumboNode*
    "___return (___arg1->document);")
  (define-c-lambda gumbo-node-text (GumboNode*) UTF-8-string
    "___return ((char*)___arg1->v.text.text);")
  (define-c-lambda gumbo-node-type (GumboNode*) int
    "___return (___arg1->type);")
  (define-c-lambda gumbo-document-children (GumboNode*) GumboVector*
    "___return (&___arg1->v.document.children);")
  (define-c-lambda gumbo-element-children (GumboNode*) GumboVector*
    "___return (&___arg1->v.element.children);")
  (define-c-lambda gumbo-element-attributes (GumboNode*) GumboVector*
    "___return (&___arg1->v.element.attributes);")
  (define-c-lambda gumbo-element-tag (GumboNode*) int
    "___return (___arg1->v.element.tag);")
  (define-c-lambda gumbo-element-original-tag-string-piece (GumboNode*) GumboStringPiece*
    "___return (&___arg1->v.element.original_tag);")
  (define-c-lambda gumbo-tag-from-original-text (GumboStringPiece*) void
    "gumbo_tag_from_original_text")
  (define-c-lambda gumbo-attribute (GumboNode*) GumboAttribute*
    "___return ((GumboAttribute*)(___arg1));")
  (define-c-lambda gumbo-attribute-name (GumboAttribute*) UTF-8-string
    "___return ((char*)___arg1->name);")
  (define-c-lambda gumbo-attribute-value (GumboAttribute*) UTF-8-string
    "___return ((char*)___arg1->value);")
  (define-c-lambda gumbo-vector-length (GumboVector*) unsigned-int
    "___return (___arg1->length);")
  (define-c-lambda gumbo-vector-ref (GumboVector* unsigned-int) GumboNode*
    "___return ((GumboNode*)___arg1->data[___arg2]);")
  (define-c-lambda gumbo-vector-capacity (GumboVector*) unsigned-int
    "___return (___arg1->capacity);")
  (define-c-lambda gumbo-string-piece-length (GumboStringPiece*) size_t
    "___return (___arg1->length);")
  (define-c-lambda gumbo-string-piece-data (GumboStringPiece*) (pointer void)
    "___return ((void*)___arg1->data);")
  (define-c-lambda gumbo-normalized-tagname (int) char-string
    "___return ((char*)gumbo_normalized_tagname(___arg1));")
  (##namespace ("")))

(define (gumbo-vector->list v)
  (list-tabulate (gumbo-vector-length v)
		 (cut gumbo-vector-ref v <>)))

(define (element-children node)
  (gumbo-vector->list (gumbo-element-children node)))

(define (document-children node)
  (gumbo-vector->list (gumbo-document-children node)))

(define (element-attributes node)
  (gumbo-vector->list (gumbo-element-attributes node)))

(define (node->sxml node)
  (let ((type (gumbo-node-type node)))
    ((cond
       ((= type GUMBO_NODE_DOCUMENT) document->sxml)
       ((= type GUMBO_NODE_ELEMENT) element->sxml)
       ((= type GUMBO_NODE_TEXT) gumbo-node-text)
       ((= type GUMBO_NODE_CDATA) cdata->sxml)
       ((= type GUMBO_NODE_COMMENT) comment->sxml)
       ((= type GUMBO_NODE_WHITESPACE) gumbo-node-text)
       ((= type GUMBO_NODE_TEMPLATE) element->sxml))
     node)))

(define (document->sxml node)
  `(*TOP* ,@(filter-map node->sxml (document-children node))))

(define (comment->sxml node)
  `(*COMMENT* ,(gumbo-node-text node)))

(define (cdata->sxml node)
  `(*CDATA* ,(gumbo-node-text node)))

(define (attribute->sxml a)
  (let ((a (gumbo-attribute a)))
    (list (string->symbol (gumbo-attribute-name a))
	  (gumbo-attribute-value a))))

(define (tag-name node)
  (let ((tag (gumbo-element-tag node)))
    (if (= GUMBO_TAG_UNKNOWN tag)
      'unknown-tag
      (string->symbol (gumbo-normalized-tagname tag)))))

(define (element->sxml node)
  `(,(tag-name node)
    ,@(let ((attrs (element-attributes node)))
        (if (null? attrs)
            '()
            (list (cons '@ (map attribute->sxml attrs)))))
    ,@(filter-map node->sxml (element-children node))))

(define (html->sxml str)
  (let ((output (gumbo-parse str)))
    (try
     (node->sxml (gumbo-document output))
     (finally (gumbo-destroy-output! output)))))
