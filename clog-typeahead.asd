(asdf:defsystem #:clog-typeahead
  :description "CLOG-Typeahead CLOG Builder Plugin"
  :author "david@botton.com"
  :license  "BSD"
  :version "0.1.0"
  :serial t
  :depends-on (#:clog)
  :components ((:file "clog-typeahead")))

(asdf:defsystem #:clog-typeahead/tools
  :depends-on (#:clog-typeahead #:clog/tools)
  :components ((:file "clog-typeahead-tools")))

