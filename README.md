# clog-typeahead
CLOG Extension for https://github.com/twitter/typeahead.js

```
(ql:quickload :clog-typeahead)
(clog-typeahead:start-test)
```

The first text box on the test is a static list test.

The second text supplies a lisp call back to handle
the query and the list of possible choices based on
the query. In this case it just returns the same static
list -

Usage:

In you .asd file add to your :depends :clog-typeahead

Attach to any form control (usually form text inputs)
you wish to have a drop down box using:

```
 attach-typeahead (clog-obj data
			 &key (hint t) (highlight t)
			   (minimum-length 1)
			   (maximum-suggestions 10))
  "Attach typeahead to a form input. DATA is a list of possible values
or a handler function with data, data being the form input and the
handlers return value is list. If HINT shows top suggestion as
background text. If HIGHLIGHT query matches within the suggestion are
highlighted."
```
