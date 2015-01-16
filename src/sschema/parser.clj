(ns sschema.parser
  (:require [sschema.char :refer :all])  
  (:require [sschema.reader :refer :all])
  (:require [sschema.writer :refer :all])
  (:require [clojure.test :refer :all])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))

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


(defn determineTagType
  [^::Reader reader]
  (let [ctrl-str (parse-ctrl reader)
        peek (peek-char reader)]
    (cond
      (= "<" ctrl-str) (if (isValidNameChar? peek)
                         :elementStart
                         (throw-parse-error reader :StartTag " Invalid element name"))
      (= "</" ctrl-str) :elementEndTag
      (= "<?" ctrl-str) :processingInstructionStartTag
      (= "<!--" ctrl-str) :commentStart
      (= "<![" ctrl-str)  (if (and (= "CDATA" (parse-name reader))
                                   (= \[ (read-char reader)))
                            :cdata
                            (throw-parse-error reader :CDATA " Illegal definition of tag"))
      (= ">" ctrl-str) :closingTag
      (= "?>" ctrl-str) :processingInstructionCloseTag
      (= "/>" ctrl-str) :elementEnd
      (nil? peek) :nil
      (isWhitespace? peek) :whitespace
      (isValidNameChar? peek) :text
      :else (throw-parse-error reader :Empty " no tag defined"))))

(defn determineTagTypeIgnoreWhitespace
  [^::Reader reader]
  (let [type (determineTagType reader)]
      (if (= :whitespace type)
        (determineTagTypeIgnoreWhitespace (skip-char reader))
        type)))

(defn parse-element
  [^::Reader reader, ^::ParserWriter writer, childFunc, finalFunc]
  (let [tag (parse-name reader)
        attr (into {} (process-attributes reader))        
        closingType (determineTagTypeIgnoreWhitespace reader)]
    (elementStart writer tag attr)
    (condp = closingType
      :closingTag (childFunc reader writer tag)
      :elementEnd (elementEnd writer tag)
      (throw-parse-error reader :EndTag " Expected close tag"))
    (finalFunc reader writer)))

(defn parse-comment
  [^::Reader reader, ^::ParserWriter writer, nextFunc]
  (commentText writer (parseComment reader))
  (nextFunc reader writer))

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
      :commentStart (parse-comment reader writer #(parseChildren %1 %2 parentElementName))
      :elementEndTag (let [endTagElementName (parse-name reader)
                           type (determineTagType reader)]
                       (if (and (= endTagElementName parentElementName) (= :closingTag type))
                         (elementEnd writer parentElementName)
                         (throw-parse-error reader :EndTag " Element Close ParentTag Mismatch")))
      :elementStart (parse-element reader writer
                                   #(parseChildren %1 %2 %3)
                                   #(parseChildren %1 %2 parentElementName))
      (throw-parse-error reader :StartTag (str  " Unexpected tag in child "type ".")))))

(defn parseDocumentEnd
  [^::Reader reader, ^::ParserWriter writer]
  (let [type (determineTagTypeIgnoreWhitespace reader)]
    (condp = type
      :commentStart (do
                      (commentText writer (parseComment reader))
                      (parseDocumentEnd reader writer))
      :nil (documentEnd writer)
      (throw-parse-error reader :DocumentEnd " Unexpected characters at end of document."))))

(defn parseProcessingInstruction
  [^::Reader reader, ^::Writer writer allowXml?]
  (let [name (parse-name reader)
        attributes (into {} (process-attributes reader))
        endTag (determineTagTypeIgnoreWhitespace reader)]
    (if-not (= :processingInstructionCloseTag endTag)
      (throw-parse-error reader :processingInstructionCloseTag " wrong close tag "endTag)
      (if (and (not allowXml?) (= "xml" name))
        (throw-parse-error reader :ProcessingInstruction "Prolog must be first")
        (processingInstruction writer name attributes)))))

(defn parseDocument
  [^::Reader reader, ^::ParserWriter writer, ^String type]
  (condp = type
    :processingInstrutionStartTag (do (parseProcessingInstruction reader writer false)
                                      (parseDocument reader writer (determineTagType reader)))
    :whitespace (do (parse-whitespace reader)
                    (parseDocument reader writer (determineTagType reader)))
    :commentStart (parse-comment reader writer
                                 #(parseDocument %1 %2 (determineTagType %1)))
    :elementStart (parse-element reader writer
                                 #(parseChildren %1 %2 %3)
                                 #(parseDocumentEnd %1 %2))
    (throw-parse-error reader :StartTag (str  " Unexpected tag in root "type "."))))

(defn processProlog
  [^::Reader reader ^::writer writer]
  (let [type (determineTagType reader)]
    (if (= :processingInstructionStartTag type)
      (do (parseProcessingInstruction reader writer true)
          (determineTagType reader))
      type)))

(defn parseStart
  [^::Reader reader ^::ParserWriter writer]
  (let [type (processProlog reader writer)]
    (parseDocument reader writer type)))

(defn parseXml
  [^::Reader reader ^::ParserWriter writer]
  (parseStart reader writer))
