(ns sschema.core
  (:import (java.io InputStream))
  (:require [clojure.test :refer :all])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))
 
;;(refresh)
 
;;;;;; reader stuff
 
(defprotocol Reader
  (read-char [reader]
    "Returns the next char from the Reader, nil if the end of stream has been reached")
  (peek-char [reader]
        "Returns the next char from the Reader without removing it from the reader stream"))
 
(deftype InputStreamReader [^InputStream is ^:unsynchronized-mutable ^"[B" buf]
  Reader
  (read-char [reader]
    (if buf
      (let [c (aget buf 0)]
        (set! buf nil)
        (char c))
      (let [c (.read is)]
        (when (>= c 0)
          (char c)))))
  (peek-char [reader]
    (when-not buf
      (set! buf (byte-array 1))
      (when (== -1 (.read is buf))
        (set! buf nil)))
    (when buf
      (char (aget buf 0)))))
 
(defn ^::Reader input-stream-reader
  "Creates an InputStreamReader from an InputStream"
  [^InputStream is]
  (InputStreamReader. is nil))
 
(defn skip-char
  [^::Reader reader]
  (read-char reader)
  reader)
 
(defn ^InputStream  make-inputstream
  [^String theStr]
  (->> (.getBytes theStr) (java.io.ByteArrayInputStream.)))
  
;;;;; char processing
 
(def char_int {:a (int \a), :A (int \A), :z (int \z), :Z (int \Z),  :0 (int \0), :9 (int \9)})
 
(defn between?
  [lower, upper, ^Integer value]
  (let [lessThanEqualTo (>= (upper char_int) value)
        moreThanEqualTo (<= (lower char_int) value)]
    (and lessThanEqualTo moreThanEqualTo)))
 
(defn isAlphaNumeric?
  ([] false)
  ([^Character ch]
     (if (nil? ch) false
         (let [intValue (int ch)]
           (or
            (or (between? :a :z intValue)
                (between? :A :Z intValue))
            (between? :0 :9 intValue))))
     ))
  
(defn isWhitespace?
  ([]  false)
  ([ch] (if (nil? ch) false (Character/isWhitespace ch))))
  
(defn isValidNameChar?
  [^Character ch]
  (if (isAlphaNumeric? ch)
    true
    (or (= \- ch) (= \_ ch) (= \# ch))))
 
;; seek functions
 
(defn seek
  [^InputStreamReader reader, match?, continueSeek?, munch]
  (loop []
    (let [peekCh (peek-char reader)]
      (cond
       (match? peekCh) (munch reader)
       (continueSeek? peekCh) (do
                                (read-char reader)
                                (recur))))))
 
(defn seekCharIgnoreWhitespace
[^InputStreamReader reader, ch, munch]
(seek reader #(= ch %) isWhitespace? munch))
 
;;;;;
 
(defprotocol ParserWriter
  (attributes [writer values])
  (elementStart [writer name])
  (elementEnd [writer name])
  (dump [writer])
  )
 
(deftype StringParserWriter [ ^:unsynchronized-mutable output]
    ParserWriter
    (attributes [writer values]
      (set! output (conj (vec output) " :attr " values)))
    (elementStart [writer name]
      (set! output (conj (vec output) "{:tag " name)))
    (elementEnd [writer name]
      (set! output (conj (vec output) " }")))
    (dump [writer] (apply str output)))
 
;;(ns-unmap *ns* 'test-StringParserWriter)
;;(ns-unmap *ns* 'test-seek)
;;(ns-unmap *ns* 'test-input-stream-reader)
;;(ns-unmap *ns* 'test-parseName)
;;(ns-unmap *ns* 'test-processAttribute)
;;(ns-unmap *ns* 'test-processAtributes)
 
;;; parser stuff
 
(def ^:dynamic *name*)
(defn parseName
  [^::Reader reader]
  (binding [*name* []]
    (loop [ch (peek-char reader)]
      (cond
       (isValidNameChar? ch) (let [readCh (read-char reader)]
                               (set! *name* (conj *name* readCh))
                               (recur (peek-char reader)))))
    (apply str *name*)))

;;; attribute processing
 
(def processQuotedAttributeValue
  (fn [^::Reader reader]
     (let [attributeValue (parseName (skip-char reader))]
       (skip-char reader)
       attributeValue)))
 
(def processAttributeValue
  (fn [^::Reader reader]
    (seekCharIgnoreWhitespace (skip-char reader) \" processQuotedAttributeValue)))
 
(defn processAttribute
  [^::Reader reader ]
  (let [attributeName (parseName reader)
        attributeValue (seekCharIgnoreWhitespace reader \= processAttributeValue)]
    {attributeName attributeValue}))
 
(deftest test-processAttribute
  (testing "parseName"
    (is (= {"name" "value"}
           (let [reader (input-stream-reader (make-inputstream "name =\"value\" "))]
             (processAttribute reader))
           )))
  )
 
(def ^:dynamic *attributes*)
(defn processAttributes
  [^::Reader reader]
  (binding  [*attributes* {}]
    (loop []
      (let [ch (peek-char reader)]
        (cond
         (isWhitespace? ch) (do
                              (read-char reader)
                              (recur))
         (isValidNameChar? ch) (let [attribute (processAttribute reader)]
                                 (set! *attributes* (conj attribute *attributes*))
                                 (recur))
         )))
    *attributes*
    ))
 
(defn parseElement
  [^::Reader reader ^::ParserWriter writer]
  (let [elementName (parseName reader)]
    (elementStart writer elementName)
    (loop []
      (let [ch (peek-char reader)]
        (cond
         (isWhitespace? ch) (do
                              (read-char reader)
                              (recur))
         (isValidNameChar? ch) (let [elementAttributes (processAttributes reader)]
                                 (attributes writer elementAttributes)
                                 (recur))
         (= \/ ch) (elementEnd writer elementName)
         (= \> ch) "process content or children?"))
      )))
  
(comment
(defn remove
  [^InputStreamReader reader]
  (loop [ch (peek-char reader)]
      (cond
       (nil? ch) "done"
       (= \> ch) "skipped"
       :else (recur (read-char reader)))))
 
(defn parserXml
  [^InputStreamReader reader]
  (loop []
    (let [ch (read-char reader)]
      (cond
       (nil? ch) "done"
       (isWhitespace? ch) (recur)
       (= \< ch) (do
                   (let [peek (peek-char reader)]
                     (cond
                      (= \? peek) (remove reader)
                      (= \! peek) (remove reader)
                      (= \/ peek) (remove reader)
                      :else (do
                              (println "alright")
                              (parseElement reader))))
                   (println "blah")
                   (recur))
       :else (recur)
       ))))
 
(parserXml (input-stream-reader (java.io.ByteArrayInputStream. (.getBytes "<?xml><bean attr=\"fdf\"><child/></bean>"))))
  )
