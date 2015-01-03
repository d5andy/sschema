(ns sschema.parser
  (:require [sschema.char :refer :all])  
  (:require [sschema.reader :refer :all])
  (:require [sschema.writer :refer :all])
  (:require [clojure.test :refer :all])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))
 
;;; attribute processing
 
(def processQuotedAttributeValue
  (fn [^::Reader reader]
     (let [attributeValue (parse-name (skip-char reader))]
       (skip-char reader)
       attributeValue)))
 
(def processAttributeValue
  (fn [^::Reader reader]
    (seekCharIgnoreWhitespace (skip-char reader) \" processQuotedAttributeValue)))
 
(defn processAttribute
  [^::Reader reader ]
  (let [attributeName (parse-name reader)
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
    (let [readName (parse-name (skip-char reader))]
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
        (isValidNameChar? ch) (content writer (parse-name reader))
        ))
    ))

(def ^:dynamic *elementName*)
(def ^:dynamic *attributes*)
(defn ^String parseElement
  [^::Reader reader]
  (binding [*elementName* (parse-name reader)
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

(defn ctrl-seq
  [^::Reader reader]
  (let [peek (peek-char reader)]
    (when (isCtrlChar? peek)
      (cons (read-char reader)
            (lazy-seq (if (= \> peek) 
                        nil
                        (ctrl-seq reader)))))))

(defn determineTagType
  [^::Reader reader]
  (let [ctrl-str (apply str (ctrl-seq reader))
        peek (peek-char reader)]
    (cond
      (= "<" ctrl-str) (if (isValidNameChar? peek)
                         :elementStart
                         :illegal)
      (= "</" ctrl-str) :elementEndTag
      (= "<?" ctrl-str) (if (= "xml" (parse-name reader))
                          :prologStart
                          :illegal)
      (= "<!--" ctrl-str) :commentStart
      (= "<![" ctrl-str)  (if (and (= "CDATA" (parse-name reader))
                                   (= \[ (peek-char reader)))
                            :cdata
                            :illegal)
      (= ">" ctrl-str) :closingTag
      (= "?>" ctrl-str) :prologEnd
      (= "/>" ctrl-str) :elementEnd
      (nil? peek) :nil
      (isWhitespace? peek) :whitespace
      (isValidNameChar? peek) :text
      :else :illegal
      )))

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
      :elementEndTag (let [endTagElementName (parse-name reader)]
                       (if (= endTagElementName parentElementName)
                         (elementEnd writer parentElementName)
                         (throw (IllegalArgumentException. (str "Element Close Mismtach")))))
      :elementStart (let [childElement (parseElement reader)
                          closingType (determineTagTypeIgnoreWhitespace reader)]
                      (elementStart writer (:tag childElement) (:attr childElement))
                      (condp = closingType
                        :closingTag (parseChildren reader writer (:tag childElement))
                        :elementEnd (do
                                      (elementEnd writer (:tag childElement))
                                      (parseChildren reader writer parentElementName))
                        (throwUnexpectedTypeException type))))))

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
                                    (parseChildren reader writer (:tag element))
                                    (parseDocumentEnd (skip-char reader) writer))
                      :elementEnd (do
                                    (elementEnd writer (:tag element))
                                    (parseDocumentEnd (skip-char reader) writer))
                      (throwUnexpectedTypeException type)))
    (throwUnexpectedTypeException type)))
  

(defn parseProlog
  [^::Reader reader ^::ParserWriter writer]
  (let [type (determineTagType reader)]
    (if (= type :prologStart)
      (let [attributes (processAttributes reader)
            type (determineTagTypeIgnoreWhitespace reader)]
        (if (= :prologEnd type)
          (let [nxtType (determineTagTypeIgnoreWhitespace reader)]
            (prolog writer attributes)
            (parseRoot reader writer nxtType))
          (throwUnexpectedTypeException type)))
      (parseRoot reader writer type))))
