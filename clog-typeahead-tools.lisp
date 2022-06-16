(in-package :clog-typeahead)

(progn
  (alexandria:appendf
   clog-tools::*supported-controls*
   (list `(:name           "input-typeahead"
	   :description    "Typeahead.js Input"
	   :clog-type      clog-typeahead:clog-typeahead-element
	   :create         clog:create-form-element ;; use the regular form element at design time
	   :create-type    :form
	   :create-param   :text
	   :create-value   ""
	   :setup          ,(lambda (control content control-record)
                              (declare (ignore content) (ignore control-record))
                              (setf (attribute control "data-on-typeahead")
				    "(clog-typeahead:test-match data `(\"one\" \"two\" \"three\"))"))
	   :on-setup       ,(lambda (control control-record)
                              (declare (ignore control control-record))
			      (format nil "(clog-typeahead:init-typeahead target) ~
			                       (clog-typeahead:attach-typeahead target nil)"))
           :events         ((:name        "on-typeahead"
                             :package     "clog-typeahead"
                             :parameters  "target data")
			    ,@clog-tools::*events-element*)
	   :properties     (,@clog-tools::*props-form-element*))))
  (format t "~%CLOG-TYPEAHEAD installed in CLOG Builder"))
