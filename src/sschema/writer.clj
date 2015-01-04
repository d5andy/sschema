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

(def debug false)
(defn- debug
  [^::Writer writer]
  (when debug  (println (.toString writer))))

(deftype StringParserWriter [^:unsynchronized-mutable output]
  ParserWriter
  (prolog [writer attributes]
    (set! output (conj (vec output) " :prolog " attributes))
    (debug writer))
  (elementStart [writer name attributes]
    (set! output (conj (vec output) " :startElement \"" name "\" :attr " attributes))
    (debug writer))
  (content [writer value]
    (when value
      (set! output (conj (vec output) " :content \"" value "\"")))
    (debug writer))
  (whitespace [writer value]
    (when value
      (set! output (conj (vec output) " :whitespace \"" value "\"")))
   (debug writer)) 
  (commentText [writer value]
    (set! output (conj (vec output) " :comment \"" value "\""))
    (debug writer))
  (elementEnd [writer name]
    (set! output (conj (vec output) " :endElement \"" name "\""))
    (debug writer))
  (documentEnd [writer]
    (set! output (conj (vec output) " :endDocument"))
    (debug writer))
  Object
  (toString [self] (when output
      (apply str output))))
