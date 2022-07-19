(in-package :clog-typeahead)

(progn
  (clog-tools:add-supported-controls
   (list `(;; unique name to control used to identify it the .clog xml
           :name           "input-typeahead"
           ;; how control appears in builder control list
           :description    "Typeahead.js Input"
           ;; the common lisp type of the control
           :clog-type      clog-typeahead:clog-typeahead-element
           ;; the create-function used to create the function
           ;; at _design time_ at run time only clog:attach-as-child is used
           ;; any initialization at _run time_ is done with :on-setup below.
           ;; in this case we just use a plain form input for design time.
           :create         clog:create-form-element
           ;; clog has the following create-types
           ;;   :base         - create
           ;;   :element      - create create-content
           ;;   :form         - create create-param create-value
           ;;   :text-area    - create create-value
           ;;   :custom-query - create (ask user for string)
           ;;   :custom       - create create-content
           :create-type    :form
           :create-param   :text
           :create-value   ""
           ;; setup the control at _design time_ and custom attributes
           :setup          ,(lambda (control content control-record)
                              (declare (ignore content) (ignore control-record))
                              (setf (attribute control "data-on-typeahead")
                                    "(clog-typeahead:test-match data `(\"one\" \"two\" \"three\"))"))
           ;; code to run at _run time_ after all controls attached to panel
           :on-setup       ,(lambda (control control-record)
                              (declare (ignore control control-record))
                              (format nil "(clog-typeahead:attach-typeahead target nil)"))
           ;; events handled
           :events         ((:name        "on-typeahead"
                             :package     "clog-typeahead"
                             :parameters  "target data")
                            ,@clog-tools::*events-element*)
           ;; properties handled
           :properties     (,@clog-tools::*props-form-element*))))
  (format t "~%CLOG-TYPEAHEAD installed in CLOG Builder"))
