(asdf:defsystem #:clog-typeahead
  :description "New CLOG System"
  :author "some@one.com"
  :license  "BSD"
  :version "0.0.0"
  :serial t
  :depends-on (#:clog)
  :components ((:file "clog-typeahead")))

(asdf:defsystem #:clog-typeahead/tools
  :depends-on (#:clog-typeahead #:clog/tools)
  :components ((:file "clog-typeahead-tools")))

