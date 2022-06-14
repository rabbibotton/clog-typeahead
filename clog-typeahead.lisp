(defpackage #:clog-typeahead
  (:use #:cl #:clog)
  (:export init-typeahead attach-typeahead start-test))

(in-package :clog-typeahead)

(defun init-typeahead (obj)
  "Load the jQuery typeahead plugin. Called on first attach-typeahead
automatically."
  (check-type obj clog:clog-obj)
  ;; Only init once
  (unless (connection-data-item obj "clog-typeahead-init")
    (setf (connection-data-item obj "clog-typeahead-init") t)
    (load-script (html-document (connection-data-item obj "clog-body"))
		 "/js/typeahead.jquery.js")))

(defun attach-typeahead (obj data
			 &key (hint t) (highlight t)
			   (minimum-length 1)
			   (maximum-suggestions 10))
  "Attach typeahead to a form input. DATA is a list of possible values
or a handler function with data, data being the form input and the
handlers return value is list. If HINT shows top suggestion as
background text. If HIGHLIGHT query matches within the suggestion are
highlighted."
  ;; Load js library if not already loaded and check obj type
  (init-typeahead obj)
  ;; handle the callback version of typeahead when DATA is a handler
  (when (typep data 'function)
    (set-on-event-with-data obj "clog-typeahead-data"
			    (lambda (obj d)
					; clog['~A-ab'] contains the async function
					; set on event to call with typeahead data
			      (js-execute obj (format nil "clog['~A-ab']([~{~A~}])"
						      (html-id obj)
						      (mapcar (lambda (s)
								(format nil "'~A'," s))
							      (funcall data obj d)))))))
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
	      (cons     (format nil "source: function findMatches(q, cb){~
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
	      (function (format nil "async: true, source: function findMatches(q, cb, ab){~
                         ~A.trigger('clog-typeahead-data',q);
                         cb([]);
                         clog['~A-ab']=ab;"
				(jquery obj)
				(html-id obj)))))))

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
				 (declare (ignore obj data))
					; idealy we should use data to determine
					; what list is returned
				 '("one" "two" "three" "four")))))
  
(defun start-test ()
  (initialize 'test-typeahead
   :static-root (merge-pathnames "./www/"
		  (asdf:system-source-directory :clog-typeahead)))
  (open-browser))
