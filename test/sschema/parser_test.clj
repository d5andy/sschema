(ns sschema.parser-test
  (:import (java.io InputStream))
  (:require [clojure.test :refer :all])
  (:require [sschema.parser :refer :all])
  (:require [sschema.reader :refer :all])
  (:require [sschema.writer :refer :all])
  (:require [clojure.tools.namespace.repl :as tns :refer [refresh]]))

(tns/refresh)

(defn createWriter []
  (sschema.writer.StringParserWriter. nil))

(comment  (deftest test-processElement
            (testing "parseSingleElement"
              (is (= {:tag "bean" :attr {"name" "value" "sec" "ond"}} 
                     (-> "bean name=\"value\" sec=\"ond\"/>" make-inputstream input-stream-reader parseElement))))))

(deftest test-read-tag
  (testing "nil"
    (is (= :nil (-> "" make-inputstream input-stream-reader read-tag))))
  (testing "whitespace"
    (is (= :whitespace (-> " " make-inputstream input-stream-reader read-tag))))
  (testing "text"
    (is (= :text (-> "a" make-inputstream input-stream-reader read-tag))))
  (testing "closingTag"
    (is (= :closingTag (-> ">" make-inputstream input-stream-reader read-tag))))
  (testing "elementEnd"
    (is (= :elementEnd (-> "/>" make-inputstream input-stream-reader read-tag))))
  (testing "prologStart"
    (is (= :processingInstructionStartTag (-> "<?xml" make-inputstream input-stream-reader read-tag))))
  (testing "prologEnd"
    (is (= :processingInstructionCloseTag (-> "?>" make-inputstream input-stream-reader read-tag))))
  (testing "comment"
    (is (= :commentStart (-> "<!--" make-inputstream input-stream-reader read-tag))))
  (testing "cdata"
    (is (= :cdata (-> "<![CDATA[" make-inputstream input-stream-reader read-tag))))
  (testing "elementEndTag"
    (is (= :elementEndTag (-> "</" make-inputstream input-stream-reader read-tag))))
  (testing "elementStart"
    (is (= :elementStart (-> "<a" make-inputstream input-stream-reader read-tag)))))

(deftest test-read-tag-skip-whitespace
  (testing "whitespaceBeforeTag"
    (is (= :closingTag (-> " >" make-inputstream input-stream-reader read-tag-skip-whitespace))))
  (testing "no_whitespaceBeforeTag"
    (is (= :elementEnd (-> "/>" make-inputstream input-stream-reader read-tag-skip-whitespace)))))

(deftest test-parseXml
  (testing "attributes"
    (is (= " :prolog {\"encoding\" \"UTF-16\"} :startElement \"bean\" :attr {} :endElement \"bean\" :endDocument"
           (let [reader (-> "<?xml encoding=\"UTF-16\"?><bean/>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "no-attributes"
    (is (= " :prolog {} :startElement \"bean\" :attr {} :startElement \"child\" :attr {} :endElement \"child\" :endElement \"bean\" :endDocument"
           (let [reader (-> "<?xml?><bean><child/></bean>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "text"
    (is (= " :prolog {} :startElement \"bean\" :attr {} :content \"aaa\" :startElement \"child\" :attr {} :endElement \"child\" :endElement \"bean\" :endDocument"
           (let [reader (-> "<?xml ?><bean>aaa<child/></bean>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "comment"
    (is (= " :prolog {} :startElement \"bean\" :attr {} :comment \" dsad \" :startElement \"child\" :attr {} :endElement \"child\" :endElement \"bean\" :endDocument"
           (let [reader (-> "<?xml ?><bean><!-- dsad --><child/></bean>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "cdata"
    (is (= " :prolog {} :startElement \"bean\" :attr {} :content \"dsad<>\" :startElement \"child\" :attr {} :endElement \"child\" :endElement \"bean\" :endDocument"
           (let [reader (-> "<?xml ?><bean><![CDATA[dsad<>]]><child/></bean>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)

             (.toString writer)))))
  (testing "whitespace"
    (is (= " :prolog {} :startElement \"bean\" :attr {} :content \"  \" :startElement \"child\" :attr {} :endElement \"child\" :endElement \"bean\" :endDocument"
           (let [reader (-> "<?xml ?><bean>  <child/></bean>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "everything"
    (is (= " :prolog {} :startElement \"bean\" :attr {} :startElement \"child\" :attr {} :content \"somtxt \" :startElement \"kid\" :attr {\"attr\" \"lala\"} :content \"this is <allowed/>\" :endElement \"kid\" :content \" \" :endElement \"child\" :content \"extratxt\" :endElement \"bean\" :comment \" end of it \" :endDocument"
           (let [reader (-> "<?xml ?><bean><child>somtxt <kid attr = \"lala\"><![CDATA[this is <allowed/>]]></kid> </child>extratxt</bean><!-- end of it -->" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "whitespace before element name"
    (is (thrown-with-msg? IllegalArgumentException #":StartTag"
           (let [reader (-> "<?xml ?>< bean><child>somtxt <kid attr = \"lala\"><![CDATA[this is <allowed/>]]></kid> </child>extratxt</bean><!-- end of it -->" make-inputstream input-stream-reader )
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "prolog after root"
    (is (thrown-with-msg? IllegalArgumentException #":processingInstructionStartTag"
           (let [reader (-> "<bean><?xml ?><child>somtxt <kid attr = \"lala\"><![CDATA[this is <allowed/>]]></kid> </child>extratxt</bean><!-- end of it -->" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "element end before child"
    (is (thrown-with-msg? IllegalArgumentException #":EndTag"
           (let [reader (-> "<bean><child></bean></child>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "text after end of root"
    (is (thrown-with-msg? IllegalArgumentException #":DocumentEnd"
           (let [reader (-> "<bean><child></child></bean>sdf" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "element after end of root"
    (is (thrown-with-msg? IllegalArgumentException #":DocumentEnd"
           (let [reader (-> "<bean><child></child></bean><kid/>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "attribute unbalanced"
    (is (thrown-with-msg? IllegalArgumentException #":Attribute"
           (let [reader (-> "<bean><child attr=\"asd\" attr2></child></bean>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "attribute unbalanced no value"
    (is (thrown-with-msg? IllegalArgumentException #":Attribute"
           (let [reader (-> "<bean><child attr=\"asd\" attr2 = ></child></bean>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "element name illegal"
    (is (thrown-with-msg? IllegalArgumentException #":EndTag"
           (let [reader (-> "<bean><ch<ild></child></bean>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "text contains CTRL character"
    (is (thrown-with-msg? IllegalArgumentException #":Empty"
           (let [reader (-> "<bean><child>d&asd</child></bean>" make-inputstream input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  (testing "malformed cdata"
    (is (thrown-with-msg? IllegalArgumentException #":CDATA"
           (let [reader (-> "<bean><child><![CATA[dsa?<<>>///]></child></bean>" make-inputstream input-stream-reader indexing-input-stream-reader)
                 writer (createWriter)]
             (parseXml reader writer)
             (.toString writer)))))
  )

(run-tests)
