(ns sschema.parser
  (:require [sschema.char :refer :all])  
  (:require [sschema.reader :refer :all])
  (:require [sschema.writer :refer :all])
  (:require [clojure.test :refer :all]))

(defn read-tag
  [^::Reader reader]
  (let [ctrl-str (parse-ctrl reader)
        peek (peek-char reader)]
    (cond
      (= "<" ctrl-str)        (if (isValidNameChar? peek)
                                :elementStart
                                (throw-parse-error reader :StartTag " Invalid element name"))
      (= "</" ctrl-str)       :elementEndTag
      (= "<?" ctrl-str)       :processingInstructionStartTag
      (= "<!--" ctrl-str)     :commentStart
      (= "<![" ctrl-str)      (if (and (= "CDATA" (parse-name reader))
                                       (= \[ (read-char reader)))
                                :cdata
                                (throw-parse-error reader :CDATA " Illegal definition of tag"))
      (= ">" ctrl-str)        :closingTag
      (= "?>" ctrl-str)       :processingInstructionCloseTag
      (= "/>" ctrl-str)       :elementEnd
      (nil? peek)             :nil
      (isWhitespace? peek)    :whitespace
      (isValidNameChar? peek) :text
      :else (throw-parse-error reader :Empty " no tag defined"))))

(defn read-tag-skip-whitespace
  [^::Reader reader]
  (let [type (read-tag reader)]
      (if (= :whitespace type)
        (read-tag-skip-whitespace (skip-char reader))
        type)))

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
                  (throw-parse-error reader :Attribute " unbalanced attributes no value")))
          (throw-parse-error reader :Attribute " unbalanced attributes no equals"))))))

(defn- process-processing-instruction
  [^::Reader reader, ^::Writer writer allowXml?]
  (let [name (parse-name reader)
        attributes (into {} (process-attributes reader))
        endTag (read-tag-skip-whitespace reader)]
    (if-not (= :processingInstructionCloseTag endTag)
      (throw-parse-error reader :processingInstructionCloseTag " wrong close tag "endTag)
      (if (and (not allowXml?) (= "xml" name))
        (throw-parse-error reader :ProcessingInstruction "Prolog must be first")
        (processingInstruction writer name attributes)))))

(defn- process-element
  [^::Reader reader, ^::ParserWriter writer, childFunc, finalFunc]
  (let [tag (parse-name reader)
        attr (into {} (process-attributes reader))        
        closingType (read-tag-skip-whitespace reader)]
    (elementStart writer tag attr)
    (condp = closingType
      :closingTag (childFunc reader writer tag)
      :elementEnd (elementEnd writer tag)
      (throw-parse-error reader :EndTag " Expected close tag"))
    (finalFunc reader writer)))

(defn- process-comment
  [^::Reader reader, ^::ParserWriter writer]
  (commentText writer (parseComment reader)))

(defn- process-document-end
  [^::Reader reader, ^::ParserWriter writer]
  (let [type (read-tag-skip-whitespace reader)]
    (condp = type
      :commentStart (do
                      (commentText writer (parseComment reader))
                      (process-document-end reader writer))
      :nil (documentEnd writer)
      (throw-parse-error reader :DocumentEnd " Unexpected characters at end of document."))))

(defn- process-common
  [^::Reader reader, ^::ParserWriter writer, type]
  (condp = type
    :whitespace                   (do (content writer (parse-whitespace reader))
                                      true)
    :processingInstrutionStartTag (do (process-processing-instruction reader writer false)
                                      true)
    :cdata                        (do (content writer (parseCdata reader))
                                      true)
    :commentStart                 (do (process-comment reader writer)
                                      true)
    false))

(defn- process-body
  [^::Reader reader, ^::ParserWriter writer, parentElementName]
  (let [type (read-tag reader)]
    (if (process-common reader writer type)
      (process-body reader writer parentElementName)
      (condp = type
        :text          (do (content writer (parse-text reader))
                           (process-body reader writer parentElementName))
        :elementEndTag (let [endTagElementName (parse-name reader)
                             type (read-tag reader)]
                         (if (and (= endTagElementName parentElementName) (= :closingTag type))
                           (elementEnd writer parentElementName)
                           (throw-parse-error reader :EndTag " Element Close ParentTag Mismatch")))
        :elementStart  (process-element reader writer
                                        #(process-body %1 %2 %3)
                                        #(process-body %1 %2 parentElementName))
        (throw-parse-error reader :StartTag (str  " Unexpected tag in child "type "."))))))

(defn- process-document
  [^::Reader reader, ^::ParserWriter writer, ^String type]
  (if (process-common reader writer type)
    (process-document reader writer (read-tag reader))
    (condp = type
      :elementStart (process-element reader writer
                                     #(process-body %1 %2 %3)
                                     #(process-document-end %1 %2))
      (throw-parse-error reader :StartTag (str  " Unexpected tag in root "type ".")))))

(defn- process-prolog
  [^::Reader reader ^::writer writer]
  (let [type (read-tag reader)]
    (if (= :processingInstructionStartTag type)
      (do (process-processing-instruction reader writer true)
          (read-tag reader))
      type)))

(defn- parseStart
  [^::Reader reader ^::ParserWriter writer]
  (let [type (process-prolog reader writer)]
    (process-document reader writer type)))

(defn parseXml
  [^::Reader reader ^::ParserWriter writer]
  (parseStart reader writer))
