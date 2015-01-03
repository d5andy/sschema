(ns sschema.writer)

(defprotocol ParserWriter
  (prolog [writer attributes])
  (elementStart [writer name attributes])
  (content [writer value])
  (whitespace [writer value])
  (elementEnd [writer name])
  (dump [writer]))
 
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
  (elementEnd [writer name]
    (set! output (conj (vec output) " :endElement \"" name "\"")))
  (dump [writer]
    (when output
      (apply str output))))
