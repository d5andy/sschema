(ns sschema.reader-test
  (:require [clojure.test :refer :all])
  (:require [sschema.reader :refer :all])
  (:require [sschema.char :refer :all])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))

(deftest test-input-stream-reader
  (testing "readFirst"
    (is (= \b
           (->> (make-inputstream "b") (input-stream-reader) (read-char)))))
  (testing "readEndOfStreamIsNil"
    (is (nil?
         (->> (make-inputstream "") (input-stream-reader) (read-char)))))
  (testing "peekFirst"
    (is (= \b
           (->> (make-inputstream "b") (input-stream-reader) (peek-char)))))
  (testing "peekFirstReadFirst"
    (is (= \b
           (let [reader (->> (make-inputstream "b") (input-stream-reader))]
             (peek-char reader) (read-char reader)))))
  (testing "peekTwiceSeeFirst"
    (is (= \b
           (let [reader (->> (make-inputstream "b") (input-stream-reader))]
             (peek-char reader) (peek-char reader)))))
  (testing "peekTwiceReadOnce"
    (is (= \b
           (let [reader (->> (make-inputstream "bar") (input-stream-reader))]
             (peek-char reader) (peek-char reader) (read-char reader)))))
  (testing "peekEndOfStreamIsNil"
    (is (nil?
         (->> (make-inputstream "") (input-stream-reader) (read-char)))))
  )

(deftest test-seek
  (testing "seekAlphaNumeric"
    (is (=\b
         (seekCharIgnoreWhitespace
          (input-stream-reader (make-inputstream "  b"))
          \b #(read-char %)))))
  (testing "seekStopAtEndOfStream"
    (is (nil?
           (seek (input-stream-reader (make-inputstream "  "))
                 isAlphaNumeric?
                 isWhitespace?
                 #(read-char %)))))
  (testing "seekEmpty"
    (is (nil?
           (seek (input-stream-reader (make-inputstream ""))
                 isAlphaNumeric?
                 isWhitespace?
                 #(read-char %))))))

(deftest test-parse-name
  (testing "parse-name"
    (is (= "bean"
           (parse-name (input-stream-reader (make-inputstream "bean>")))))))

(run-tests)
