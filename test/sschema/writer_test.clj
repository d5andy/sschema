(ns sschema.writer-test
  (:require [clojure.test :refer :all])
  (:require [sschema.writer :refer :all])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))


(deftest test-StringParserWriter
  (testing "addingAndDumping"
    (is (= " :startElement \"bean\" :attr {\"blah\" \"value\"}"
           (let [writer (sschema.writer.StringParserWriter. nil)]
             (elementStart writer "bean" {"blah" "value"})
             (.toString  writer)))))
  (testing "empty content"
    (is (nil?
         (let [writer (sschema.writer.StringParserWriter. nil)]
           (content writer nil)
           (.toString  writer)))))
  (testing "empty with dump"
    (is (nil?
         (.toString ( sschema.writer.StringParserWriter. nil)))))
  )
(run-tests)

