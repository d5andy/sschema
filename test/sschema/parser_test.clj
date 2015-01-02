(ns sschema.parser-test
  (:import (java.io InputStream))
  (:require [clojure.test :refer :all])
  (:require [sschema.parser :refer :all])
  (:require [sschema.reader :refer :all])
  (:require [sschema.writer :refer :all])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))

(deftest test-processAttribute
  (testing "parseName"
    (is (= {"name" "value"}
           (let [reader (input-stream-reader (make-inputstream "name =\"value\" "))]
             (processAttribute reader))
           )))
  )

(deftest test-processAttributes
  (testing "parseName"
    (is (= {"name" "value" "sec" "ond"}
           (let [reader (input-stream-reader (make-inputstream "name =\"value\" sec=\"ond\""))]
             (processAttributes reader)
             )))))

(deftest test-skipUntilNextTag
  (testing "skipToTag"
    (is (= \<
           (peek-char (skipUntilNextTag (input-stream-reader (make-inputstream "blah--><balh/>")))))))
  (testing "skipToEnd"
    (is (nil?
         (peek-char (skipUntilNextTag (input-stream-reader (make-inputstream "blah-->")))))))
  )

(deftest test-parseCData
  (testing "parseTilEndOfCData"
    (is (= "blah")
        (parseCData (input-stream-reader (make-inputstream "blah]]"))))))

(deftest test-parseCommentOrCData
  (testing "parseCData"
    (is (= "\" were \""
           (parseCommentOrCData (input-stream-reader (make-inputstream "[CDATA[ were ]]d"))))))
  (testing "parseComment"
    (is (nil?
         (parseCommentOrCData (input-stream-reader (make-inputstream "-- blh -->")))))))

(defn createWriter
  []
  (sschema.writer.StringParserWriter. nil))

(comment 
  (deftest test-parseTagData
    (testing "commentTagData"
      (is (nil?
           (let [writer (createWriter)
                 reader (input-stream-reader (make-inputstream "!-- comment -->"))]
             (parseTagData reader writer)
             (dump writer)
             ))))
    (testing "cdataTagData"
      (is (= ":content \" Hello \"" 
             (let [writer (createWriter)
                   reader (input-stream-reader (make-inputstream "![CDATA[ Hello ]]>"))]
               (parseTagData reader writer)
               (dump writer)
               ))))
    (testing "elementEndTagData"
      (is (= " }"
             (let [writer (createWriter)
                   reader (input-stream-reader (make-inputstream "/endof>"))]
               (parseTagData reader writer)
               (dump writer)
               )))
      )
    (testing "elementStartTagData"
      (is (= "{:tag newele }"
             (let [writer (createWriter)
                   reader (input-stream-reader (make-inputstream "newele/>"))]
               (parseTagData reader writer)
               (dump writer)
               ))))
    ))

(deftest test-parseElementBody
  (is (= " :content \"were\""
         (let [writer (createWriter)
               reader (input-stream-reader (make-inputstream " were "))]
           (parseElementBody reader writer)
           (dump writer)
           ))))

(deftest test-processElement
  (testing "parseSingleElement"
    (is (= {:tag "bean" :attr {"name" "value" "sec" "ond"}} 
           (-> "bean name =\"value\" sec=\"ond\"/>" make-inputstream input-stream-reader parseElement)))))

(deftest test-determineTagType
  (testing "nil"
    (is (= :nil (-> "" make-inputstream input-stream-reader determineTagType))))
  (testing "whitespace"
    (is (= :whitespace (-> " " make-inputstream input-stream-reader determineTagType))))
  (testing "text"
    (is (= :text (-> "a" make-inputstream input-stream-reader determineTagType))))
  (testing "closingTag"
    (is (= :closingTag (-> ">" make-inputstream input-stream-reader determineTagType))))
  (testing "elementEnd"
    (is (= :elementEnd (-> "/>" make-inputstream input-stream-reader determineTagType))))
  (testing "prologStart"
    (is (= :prologStart (-> "<?xml" make-inputstream input-stream-reader determineTagType))))
  (testing "prologEnd"
    (is (= :prologEnd (-> "?>" make-inputstream input-stream-reader determineTagType))))
  (testing "comment"
    (is (= :commentStart (-> "<!--" make-inputstream input-stream-reader determineTagType))))
  (testing "cdata"
    (is (= :cdata (-> "<![CDATA[" make-inputstream input-stream-reader determineTagType))))
  (testing "elementEndTag"
    (is (= :elementEndTag (-> "</" make-inputstream input-stream-reader determineTagType))))
  (testing "elementStart"
    (is (= :elementStart (-> "<a" make-inputstream input-stream-reader determineTagType)))))

(deftest test-determineTagTypeIgnoreWhitespace
  (testing "whitespaceBeforeTag"
    (is (= :closingTag (-> " >" make-inputstream input-stream-reader determineTagTypeIgnoreWhitespace))))
  (testing "no_whitespaceBeforeTag"
    (is (= :elementEnd (-> "/>" make-inputstream input-stream-reader determineTagTypeIgnoreWhitespace)))))

(deftest test-parseProlog
  (testing "attributes"
    (is (= " :prolog {\"encoding\" \"UTF-16\"} :startElement \"bean\" :attr {}"
           (let [reader (-> "<?xml encoding=\"UTF-16\" ?><bean/>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseProlog reader writer)
             (dump writer)))))
  (testing "no-attributes"
    (is (= " :prolog {} :startElement \"bean\" :attr {}"
           (let [reader (-> "<?xml ?><bean/>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseProlog reader writer)
             (dump writer)))))
  )

(run-tests)
