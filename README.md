sschema
=======

A native xml parser for clojure - by which I mean the vast majority is written in clojure syntax.

The intention is to add support for:
* Push / Pull parsing
* Zipper 

Incomplete:
* Meet the spec http://www.w3.org/TR/xml/ (ie currently coded from memory need to check back to the spec)
* fix the simplistic processing instructions (known issue)
* handle different encodings
* handle namespaces in the event handler (sax stye perhaps)
* track position by column and line for error reporting
