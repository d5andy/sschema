(ns sschema.reader-test
  (:require [clojure.test :refer :all])
  (:require [sschema.reader :refer :all])
  (:require [sschema.char :refer :all])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))

(deftest test-parse-name
  (testing "parse-name"
    (is (= "bean"
           (parse-name (input-stream-reader (make-inputstream "bean>"))))))
  (testing "parse-name-nil"
    (is (= "bean"
           (parse-name (input-stream-reader (make-inputstream "bean"))))))
  (testing "parse-name-whitespace"
    (is (= "bean"
           (parse-name (input-stream-reader (make-inputstream "bean "))))))
  )

(deftest test-parse-text
  (testing "parse-text"
    (is (= "qewr"
           (parse-text (-> "qewr<" make-inputstream input-stream-reader)))))
  (testing "parse-text-nil"
    (is (= "qewr"
           (parse-text (-> "qewr" make-inputstream input-stream-reader)))))
  )

(deftest test-parse-whitespace
  (testing "parse-whitespace"
    (is (= "  "
           (parse-whitespace (-> "  q" make-inputstream input-stream-reader)))))
  (testing "parse-text-nil"
    (is (= "    "
           (parse-whitespace (-> "    " make-inputstream input-stream-reader)))))
  )

(deftest test-parse-quoted
  (testing "parse-whitespace"
    (is (= "a"
           (parse-quoted (-> "\"a\"" make-inputstream input-stream-reader)))))
  (testing "parse-nil"
    (is (= ""
           (parse-quoted (-> "\"\"" make-inputstream input-stream-reader)))))
  )

(run-tests)
