(defpackage #:clog-typeahead
  (:use #:cl #:clog)
  (:export init-typeahead
	   attach-typeahead
	   start-test
	   test-match
	   clog-typeahead-element
	   create-typeahead-element
	   set-on-typeahead))

(in-package :clog-typeahead)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-typeahead-clement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-typeahead-element (clog-form-element)()
  (:documentation "CLOG Typeahead Element Object. The file:
/js/typeahead.jquery.js must be installed in your project /js directory."))

(defgeneric create-typeahead-element (clog-obj element-type
                                      &key name value label class
					hidden html-id)
  (:documentation "Create a new clog-typeahead-element as child of CLOG-OBJ."))

(defmethod create-typeahead-element ((obj clog:clog-obj) element-type
                                     &key (name nil)
                                       (value nil)
                                       (label nil)
                                       (class nil)
                                       (hidden nil)
                                       (html-id nil))
  (let ((new-obj (create-form-element obj element-type
				      :name name
				      :value value
				      :label label
				      :class class
				      :hidden hidden
				      :html-id html-id)))
    (attach-typeahead new-obj nil)
    (change-class new-obj 'clog-typeahead-element)))

(defgeneric set-on-typeahead (clog-obj handler
			      &key cancel-event one-time)
  (:documentation "Set a HANDLER for typeahead data queries on CLOG-OBJ.
 If HANDLER is nil unbind all event handlers. Handler is called with a
data parameter that is the current query, ie the text in the control.
The return value of this handler must be a list that will be the possible
values based on that query."))

(defmethod set-on-typeahead ((obj clog:clog-obj) handler
			     &key
			       (cancel-event nil)
			       (one-time     nil))
  (set-on-event-with-data obj "clog-typeahead-data"
			  (lambda (obj query)
					; clog['~A-ab'] contains the async function
					; set on typing to call with typeahead data
			    (js-execute obj (format nil "clog['~A-ab']([~{~A~}])"
						    (html-id obj)
						    (mapcar (lambda (s)
							      (format nil "'~A'," s))
							    (funcall handler obj query)))))
			  :cancel-event cancel-event
			  :one-time      one-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - typeahead bindings to typeahead.js
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-typeahead (obj)
  "Load the jQuery typeahead javascript plugin. Called on first
attach-typeahead automatically, so generally no need to call."
  (check-type obj clog:clog-obj)
  ;; Only init once
  (load-script (html-document (connection-data-item obj "clog-body"))
	       "/js/typeahead.jquery.js" :load-only-once t))

(defun attach-typeahead (obj data
			 &key (hint t) (highlight t)
			   (minimum-length 1)
			   (maximum-suggestions 10))
  "Attach typeahead to a form input. DATA is a list of possible values
or a handler function with data, data being the form input and the
handlers return value is list or nil to set handler with set-on-typeahead.
If HINT shows top suggestion as background text. If HIGHLIGHT query matches
within the suggestion are highlighted."
  ;; Load js library if not already loaded and check obj type
  (init-typeahead obj)
  ;; Handle the callback version of typeahead when DATA is a handler
  (when (typep data 'function)
    (set-on-typeahead obj data))
  ;; Setup typeahead
  (jquery-execute obj
    (format nil
	    "typeahead({hint: ~A, highlight: ~A, minLength: ~A},~
                       {limit: ~A, ~
                         ~A
                         }})"
	    (p-true-js hint)
	    (p-true-js highlight)
	    minimum-length
	    maximum-suggestions
	    (typecase data
	      (cons (format nil "source: function findMatches(q, cb){~
                         var matches=[];~
                         var sr=new RegExp(q,'i');~
                         var strs=[~{~A~}];~
                         $.each(strs,function(i,str){~
                           if (sr.test(str)){~
                             matches.push(str);~
                           }});~
                         cb(matches);"
			    (mapcar (lambda (s)
				      (format nil "'~A'," s))
				    data)))
	      ;; Use callback if data is nil or data is function
	      (t    (format nil "async: true, source: function findMatches(q, cb, ab){~
                         ~A.trigger('clog-typeahead-data',q);
                         cb([]);
                         clog['~A-ab']=ab;"
			    (jquery obj)
			    (html-id obj)))))))

(defun test-match (data choices)
  "Simple filter for typeahead"
  (let (matches)
    (dolist (c choices)
      (when (ppcre:scan data c)
        (push c matches)))
    matches))

(defun test-typeahead (body)
  (clog:debug-mode body)
  (create-style-block body :content
".typeahead,
.tt-query,
.tt-hint {
  width: 396px;
  height: 30px;
  padding: 8px 12px;
  font-size: 24px;
  line-height: 30px;
  border: 2px solid #ccc;
  -webkit-border-radius: 8px;
     -moz-border-radius: 8px;
          border-radius: 8px;
  outline: none;
}

.typeahead {
  background-color: #fff;
}

.typeahead:focus {
  border: 2px solid #0097cf;
}

.tt-query {
  -webkit-box-shadow: inset 0 1px 1px rgba(0, 0, 0, 0.075);
     -moz-box-shadow: inset 0 1px 1px rgba(0, 0, 0, 0.075);
          box-shadow: inset 0 1px 1px rgba(0, 0, 0, 0.075);
}

.tt-hint {
  color: #999
}

.tt-menu {
  width: 422px;
  margin: 12px 0;
  padding: 8px 0;
  background-color: #fff;
")
  (let ((element  (create-form-element body :text))
	(element2 (create-form-element body :text)))
    ;; Using element list
    (add-class element "typeahead")
    (attach-typeahead element '("one" "two" "three" "four"))
    ;; Using lisp function to return typeahead
    (add-class element2 "typeahead")
    (attach-typeahead element2 (lambda (obj data)
				 (declare (ignore obj))
				 (test-match data '("one" "two" "three" "four"))))))

(defun start-test ()
  (initialize 'test-typeahead
   :static-root (merge-pathnames "./www/"
		  (asdf:system-source-directory :clog-typeahead)))
  (open-browser))
