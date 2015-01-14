(ns sschema.reader-test
  (:require [clojure.test :refer :all])
  (:require [sschema.reader :refer :all])
  (:require [sschema.char :refer :all])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))

(deftest test-throw-parse-error
  (let [rdr (-> "s\newline asdf" make-inputstream input-stream-reader)
        irdr (indexing-input-stream-reader rdr)]
    (testing "indexing-rdr line numbers"
      (is (thrown-with-msg? IllegalArgumentException #"line 1 column 1"
                            (throw-parse-error irdr :tag "msg"))))
    (testing "non-indexing-rdr without line numbers"
      (is (thrown-with-msg? IllegalArgumentException #"Error occurred :tag: msg"
                            (throw-parse-error rdr :tag "msg"))))
    (testing "only tag and message"
      (is (thrown-with-msg? IllegalArgumentException #"Error occurred :tag: message"
                            (throw-parse-error :tag "message"))))))

(deftest test-IndexingReader
  (testing "multiline-read"
    (let [rdr (-> "s\newline asdf" make-inputstream input-stream-reader indexing-input-stream-reader)]
      (is (= [1,1] [(get-column-number rdr) (get-line-number rdr)]))
      (is (= [1,2] (do (doto rdr read-char read-char)
                       [(get-column-number rdr) (get-line-number rdr)])))
      (is (= [3,2] (do (doto rdr read-char read-char)
                       [(get-column-number rdr) (get-line-number rdr)])))
      )
    ))

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
  (testing "parse-text-nil"
    (is (= "qe,[[wr"
           (parse-text (-> "qe,[[wr" make-inputstream input-stream-reader)))))
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
