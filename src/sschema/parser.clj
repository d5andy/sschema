(ns sschema.parser
  (:require [sschema.char :refer :all])  
  (:require [sschema.reader :refer :all])
  (:require [sschema.writer :refer :all])
  (:require [clojure.test :refer :all])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))

(defn throw-parse-error
  [tag, ^String msg]
  (throw (IllegalArgumentException. (str tag": "msg))))

(defn cdata-seq
  [^::Reader reader buf]
  (let [ch (read-char reader)]
    (when ch
      (cond
        (= \> ch) (if (= '(\] \]) buf)
                    nil
                    (cons ch (lazy-seq (cdata-seq reader nil))))
        (= \] ch) (cons ch (lazy-seq (cdata-seq reader (conj (vec buf) ch))))
        :else (cons ch (lazy-seq (cdata-seq reader nil)))))))

(defn parseCdata
  [^::Reader reader]
  (apply str (-> (cdata-seq reader nil) butlast butlast)))

(defn comment-seq
  [^::Reader reader buf]
  (let [ch (read-char reader)]
    (when ch
      (cond
        (= \> ch) (if (= [\- \-] buf)
                      nil
                      (throw (IllegalArgumentException. "close tag in comment body")))
        (= \- ch) (cons ch (lazy-seq (comment-seq reader (conj (vec buf) ch))))
        :else (cons ch (lazy-seq (comment-seq reader nil)))))))

(defn parseComment
  [^::Reader reader]
  (apply str (-> (comment-seq reader nil) butlast butlast)))

(defn process-attributes
  [^::Reader reader]
  (parse-whitespace reader)
  (when-not (isCtrlChar? (peek-char reader))
    (let [attributeName (parse-name reader)]
      (when attributeName
        (parse-whitespace reader)
        (if (= \= (peek-char reader))
          (let [attributeValue (do (parse-whitespace (skip-char reader)) (parse-quoted reader))]
                (if attributeValue
                  (cons {attributeName attributeValue} (lazy-seq (process-attributes reader)))
                  (throw (IllegalArgumentException. "unbalanced attributes no value"))))
          (throw (IllegalArgumentException. "unbalanced attributes no equals")))))))

(defn ^String parseElement
  [^::Reader reader]
  (let [elementName (parse-name reader)
        attributes (into {} (process-attributes reader))]
    {:tag elementName :attr attributes}))


(defn determineTagType
  [^::Reader reader]
  (let [ctrl-str (parse-ctrl reader)
        peek (peek-char reader)]
    (cond
      (= "<" ctrl-str) (if (isValidNameChar? peek)
                         :elementStart
                         (throw-parse-error :StartTag " Invalid element name"))
      (= "</" ctrl-str) :elementEndTag
      (= "<?" ctrl-str) (if (= "xml" (parse-name reader))
                          :prologStart
                          (throw-parse-error :Prolog " Unsupported Processing Instruction"))
      (= "<!--" ctrl-str) :commentStart
      (= "<![" ctrl-str)  (if (and (= "CDATA" (parse-name reader))
                                   (= \[ (read-char reader)))
                            :cdata
                            (throw-parse-error :CDATA " Illegal definition of tag"))
      (= ">" ctrl-str) :closingTag
      (= "?>" ctrl-str) :prologEnd
      (= "/>" ctrl-str) :elementEnd
      (nil? peek) :nil
      (isWhitespace? peek) :whitespace
      (isValidNameChar? peek) :text
      :else (throw-parse-error :Empty " no tag defined"))))

(defn determineTagTypeIgnoreWhitespace
  [^::Reader reader]
  (let [type (determineTagType reader)]
      (if (= :whitespace type)
        (determineTagTypeIgnoreWhitespace (skip-char reader))
        type)))

(defn parseChildren
  [^::Reader reader, ^::ParserWriter writer, parentElementName]
  (let [type (determineTagType reader)]
    (condp = type
      :whitespace (do (content writer (parse-whitespace reader))
                      (parseChildren reader writer parentElementName))
      :cdata (do (content writer (parseCdata reader))
                 (parseChildren reader writer parentElementName))
      :text (do (content writer (parse-text reader))
                (parseChildren reader writer parentElementName))
      :commentStart (do (parseComment reader)
                        (parseChildren reader writer parentElementName))
      :elementEndTag (let [endTagElementName (parse-name reader)
                           type (determineTagType reader)]
                       (if (and (= endTagElementName parentElementName) (= :closingTag type))
                         (elementEnd writer parentElementName)
                         (throw-parse-error :EndTag " Element Close ParentTag Mismatch")))
      :elementStart (let [childElement (parseElement reader)
                          closingType (determineTagTypeIgnoreWhitespace reader)]
                      (elementStart writer (:tag childElement) (:attr childElement))
                      (condp = closingType
                        :closingTag (do
                                      (parseChildren reader writer (:tag childElement))
                                      (parseChildren reader writer parentElementName))
                        :elementEnd (do
                                      (elementEnd writer (:tag childElement))
                                      (parseChildren reader writer parentElementName))
                        (throw-parse-error :EndTag " Expected close tag"))))))

(defn parseDocumentEnd
  [^::Reader reader, ^::ParserWriter writer]
  (let [type (determineTagTypeIgnoreWhitespace reader)]
    (condp = type
      :commentStart (do
                      (commentText writer (parseComment reader))
                      (parseDocumentEnd reader writer))
      :nil (documentEnd writer)
      (throw-parse-error :DocumentEnd " Unexpected characters at end of document."))))

(defn parseRoot
  [^::Reader reader, ^::ParserWriter writer, type]
  (condp = type
    :whitespace (do (read-char reader)
                    (parseRoot reader writer (determineTagType reader)))
    :commentStart (do (commentText writer (parseComment reader))
                      (parseRoot reader writer (determineTagType reader)))
    :elementStart (let [element (parseElement reader)
                        closingType (determineTagTypeIgnoreWhitespace reader)]
                    (elementStart writer (:tag element) (:attr element))
                    (condp = closingType
                      :closingTag (do
                                    (parseChildren reader writer (:tag element))
                                    (parseDocumentEnd reader writer))
                      :elementEnd (do
                                    (elementEnd writer (:tag element))
                                    (parseDocumentEnd reader writer))
                      (throw-parse-error :EndTag " Expected Close tag")))
    (throw-parse-error :StartTag (str  " Unexpected tag in root "type "."))))

(defn parseProlog
  [^::Reader reader ^::ParserWriter writer]
  (let [type (determineTagType reader)]
    (if (= type :prologStart)
      (let [attributes (into {} (process-attributes reader))
            type (determineTagTypeIgnoreWhitespace reader)]
        (if (= :prologEnd type)
          (let [nxtType (determineTagTypeIgnoreWhitespace reader)]
            (prolog writer attributes)
            (parseRoot reader writer nxtType))
          (throw-parse-error :DocumentStart (str  " Unexpected tag at start of document "type"."))))
      (parseRoot reader writer type))))
