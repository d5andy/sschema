(ns sschema.writer)

(defprotocol ParserWriter
  (prolog [writer attributes]
    "Prologs map of attributes")
  (elementStart [writer ^String name attributes]
    "Element name and map of attributes")
  (content [writer ^String value]
    "Content from element body")
  (whitespace [writer ^String value]
    "White space String read from element body")
  (commentText [writer ^String value])
  (elementEnd [writer name])
  (documentEnd [writer]))
 
(deftype StringParserWriter [^:unsynchronized-mutable output]
  ParserWriter
  (prolog [writer attributes]
    (set! output (conj (vec output) " :prolog " attributes)))
  (elementStart [writer name attributes]
    (set! output (conj (vec output) " :startElement \"" name "\" :attr " attributes)))
  (content [writer value]
    (when value
      (set! output (conj (vec output) " :content \"" value "\""))))
  (whitespace [writer value]
    (when value
      (set! output (conj (vec output) " :whitespace \"" value "\""))))
  (commentText [writer value]
           (set! output (conj (vec output) " :comment \"" value "\"")))
  (elementEnd [writer name]
    (set! output (conj (vec output) " :endElement \"" name "\"")))
  (documentEnd [writer]
    (set! output (conj (vec output) " :endDocument")))
  Object
  (toString [self] (when output
      (apply str output))))
