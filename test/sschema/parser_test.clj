(ns sschema.parser-test
  (:import (java.io InputStream))
  (:require [clojure.test :refer :all])
  (:require [sschema.parser :refer :all])
  (:require [sschema.reader :refer :all])
  (:require [sschema.writer :refer :all])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))

(defn createWriter
  []
  (sschema.writer.StringParserWriter. nil))

(deftest test-processElement
  (testing "parseSingleElement"
    (is (= {:tag "bean" :attr {"name" "value" "sec" "ond"}} 
           (-> "bean name=\"value\" sec=\"ond\"/>" make-inputstream input-stream-reader parseElement)))))

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
    (is (= " :prolog {\"encoding\" \"UTF-16\"} :startElement \"bean\" :attr {} :endElement \"bean\" :endDocument"
           (let [reader (-> "<?xml encoding=\"UTF-16\"?><bean/>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseProlog reader writer)
             (dump writer)))))
  (testing "no-attributes"
    (is (= " :prolog {} :startElement \"bean\" :attr {} :startElement \"child\" :attr {} :endElement \"child\" :endElement \"bean\" :endDocument"
           (let [reader (-> "<?xml?><bean><child/></bean>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseProlog reader writer)
             (dump writer)))))
  (testing "text"
    (is (= " :prolog {} :startElement \"bean\" :attr {} :content \"aaa\" :startElement \"child\" :attr {} :endElement \"child\" :endElement \"bean\" :endDocument"
           (let [reader (-> "<?xml ?><bean>aaa<child/></bean>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseProlog reader writer)
             (dump writer)))))
  (testing "comment"
    (is (= " :prolog {} :startElement \"bean\" :attr {} :startElement \"child\" :attr {} :endElement \"child\" :endElement \"bean\" :endDocument"
           (let [reader (-> "<?xml ?><bean><!-- dsad --><child/></bean>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseProlog reader writer)
             (dump writer)))))
  (testing "cdata"
    (is (= " :prolog {} :startElement \"bean\" :attr {} :content \"dsad<>\" :startElement \"child\" :attr {} :endElement \"child\" :endElement \"bean\" :endDocument"
           (let [reader (-> "<?xml ?><bean><![CDATA[dsad<>]]><child/></bean>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseProlog reader writer)
             (dump writer)))))
  (testing "whitespace"
    (is (= " :prolog {} :startElement \"bean\" :attr {} :content \"  \" :startElement \"child\" :attr {} :endElement \"child\" :endElement \"bean\" :endDocument"
           (let [reader (-> "<?xml ?><bean>  <child/></bean>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseProlog reader writer)
             (dump writer)))))
  )

(run-tests)
