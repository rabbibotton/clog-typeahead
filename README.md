# clog-typeahead
CLOG Extension and CLOG Builder Plug-in for https://github.com/twitter/typeahead.js

To load clog-typeahead and test:

```
(ql:quickload :clog-typeahead)
(clog-typeahead:start-test)
```

The first text box on the test is a static list test.

The second text supplies a lisp call back to handle
the query and the list of possible choices based on
the query.


To use with clog-builder and try it out:

```
(ql:quickload :clog-typeahead/tools)
(clog-tools:clog-builder :system :clog-typeahead)
```

Usage (not using CLOG Builder):

In you .asd file add to your :depends-on :clog-typeahead

Attach to any form control (usually form text inputs)
you wish to have a drop down box using:

```
 attach-typeahead (clog-obj data
			 &key (hint t) (highlight t)
			   (minimum-length 1)
			   (maximum-suggestions 10))
  "Attach typeahead to a form input. DATA is a list of possible values
or a handler function with data, data being the form input and the
handlers return value is list or nil to set handler with set-on-typeahead.
If HINT shows top suggestion as background text. If HIGHLIGHT query matches
within the suggestion are highlighted."
```

Usage with CLOG Builder:

In you .asd file add to your :depends-on :clog-typeahead
Copy clog-typeahead/www/js/typeahead.jquery.js to your
projects /www/js directory.

```
(ql:quickload :clog-typeahead/tools)
[load any other CLOG Builder plugins]
(clog-tools:clog-builder :system :YOUR-SYSTEM)
```

