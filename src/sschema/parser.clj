(ns sschema.parser
  (:require [sschema.char :refer :all])  
  (:require [sschema.reader :refer :all])
  (:require [sschema.writer :refer :all])
  (:require [clojure.test :refer :all])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))
 
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

(defn skipUntilNextTag
  [^::Reader reader]
  (loop []
    (let [ch (peek-char reader)]
      (when-not (or (nil? ch) (= \< ch))
        (do
          (skip-char reader)
          (recur)))))
  reader)

(def ^:dynamic *cdata*)
(defn parseCData
  [^::Reader reader]
  (binding [*cdata* []]
    (loop []
      (let [ch (peek-char reader)]
        (when-not (or (nil? ch) (= \] (last *cdata*) ch))
          (do (set! *cdata* (conj *cdata* (read-char reader))) (recur))
          )
        ))
    (str \" (apply str (drop-last  *cdata*)) \")
    ))

(defn parseCommentOrCData
  [^::Reader reader]
  (if (= \[ (peek-char reader))
    (let [readName (parseName (skip-char reader))]
      (if (and ( = "CDATA" readName) (= \[ (peek-char reader)))
        (parseCData (skip-char reader))
        (do (skipUntilNextTag reader) nil)
        ))
    (do (skipUntilNextTag reader) nil)
    ))

(defn parseElementBody
  [^::Reader reader ^::ParserWriter writer]
  (loop []
    (let [ch (peek-char reader)]
      (cond
        (isWhitespace? ch) (do
                              (read-char reader)
                              (recur))
        (isValidNameChar? ch) (content writer (parseName reader))
        ))
    ))

(def ^:dynamic *elementName*)
(def ^:dynamic *attributes*)
(defn ^String parseElement
  [^::Reader reader]
  (binding [*elementName* (parseName reader)
            *attributes* {}]
    (loop []
      (let [ch (peek-char reader)]
        (cond
          (isWhitespace? ch) (do
                               (read-char reader)
                               (recur))
          (isValidNameChar? ch) (set! *attributes* (processAttributes reader)))))
    {:tag *elementName* :attr *attributes*}))

;;;;

(defn determineTagType
  [^::Reader reader]
  (let [ch (peek-char reader)]
    (cond
      (nil? ch) :nil
      (isWhitespace? ch) :whitespace
      (isValidNameChar? ch) :text
      (= \> ch) :closingTag
      (= \/ ch) (if (= \> (-> reader skip-char peek-char))
                  :elementEnd
                  :illegal)
      (= \? ch) (if (= \> (-> reader skip-char peek-char))
                  :prologEnd
                  :illegal)
      (= \< ch) (let [peek (-> reader skip-char peek-char)]
                  (cond
                    (= \? peek) (if (= "xml" (parseName (skip-char reader)))
                                  :prologStart
                                  :illegal)
                    (= \! peek) (let [peek (-> reader skip-char peek-char)]
                                  (cond
                                    (= \- peek) (if (= \- (-> reader skip-char peek-char))
                                             :commentStart
                                             :illegal)
                                    (= \[ peek) (if (= "CDATA" (parseName (skip-char reader)))
                                                  (if (= \[ (peek-char reader))
                                                    :cdata
                                                    :illegal)
                                                  :illegal)
                                    :else :illegal))
                    (= \/ peek) :elementEndTag
                    (isValidNameChar? peek) :elementStart
                    :else :illegal))
      :else :illegal)))

(defn determineTagTypeIgnoreWhitespace
  [^::Reader reader]
  (let [type (determineTagType reader)]
      (if (= :whitespace type)
        (determineTagTypeIgnoreWhitespace (skip-char reader))
        type)))

;;;;

(defn parseCdata
  [^::Reader reader]
  )

(defn parseComment
  [^::Reader reader]
  )

(defn parseText
  [^::Reader reader]
  )

(defn throwUnexpectedTypeException
  [^String type]
  (throw (IllegalArgumentException. (str "Unexpected type " type))))

(defn parseChildren
  [^::Reader reader, ^::ParserWriter writer, parentElementName]
  (let [type (determineTagTypeIgnoreWhitespace reader)]
    (condp = type
      :cdata (content writer (parseCdata reader))
      :text (content writer (parseText reader))
      :commentStart (parseComment reader)
      :elementEndTag (let [endTagElementName (parseName reader)]
                       (if (= endTagElementName parentElementName)
                         (elementEnd writer parentElementName)
                         (throw (IllegalArgumentException. (str "Element Close Mismtach")))))
      :elementStart (let [childElementName (parseElement reader writer)
                          closingType (determineTagTypeIgnoreWhitespace reader)]
                      (condp = closingType
                        :closingTag (parseChildren (skip-char reader) writer childElementName)
                        :elementEnd (elementEnd writer childElementName)
                        (throwUnexpectedTypeException type)))
      )))

(defn parseDocumentEnd
  [^::Reader reader, ^::ParserWriter writer]
  ())

(defn parseRoot
  [^::Reader reader, ^::ParserWriter writer, type]
  (condp = type
    :whitespace (do (read-char reader)
                    (parseRoot reader writer (determineTagType reader)))
    :commentStart (do (parseComment reader)
                      (parseRoot reader writer (determineTagType reader)))
    :elementStart (let [element (parseElement reader)
                        closingType (determineTagTypeIgnoreWhitespace reader)]
                    (elementStart writer (:tag element) (:attr element))
                    (condp = closingType
                      :closingTag (do
                                    (parseChildren (skip-char reader) writer (:tag element))
                                    (parseDocumentEnd (skip-char reader) writer))
                      :elementEnd (parseDocumentEnd (skip-char reader) writer)
                      (throwUnexpectedTypeException type)))
    (throwUnexpectedTypeException type)))
  

(defn parseProlog
  [^::Reader reader ^::ParserWriter writer]
  (let [type (determineTagType reader)]
    (if (= type :prologStart)
      (let [attributes (processAttributes reader)
            type (determineTagTypeIgnoreWhitespace reader)]
        (if (= :prologEnd type)
          (let [nxtType (determineTagTypeIgnoreWhitespace (skip-char reader))]
            (prolog writer attributes)
            (parseRoot reader writer nxtType))
          (throwUnexpectedTypeException type)))
      (parseRoot reader writer type))))
