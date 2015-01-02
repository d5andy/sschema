(ns sschema.char-test
  (:require [clojure.test :refer :all])
  (:require [sschema.char :refer :all])
  (:require [clojure.tools.namespace.repl :refer [refresh]]))

(deftest test-isAlphaNumberic
  (testing "NumberisTrue"
    (is (every? true? (map #(isAlphaNumeric? %)  (seq "0123456789")))))
  (testing "AlphaisTrue"
    (is (every? true? (map #(isAlphaNumeric? %) (seq "abcdefghijklmnopqrstuwvxyzABCDEFGHIJKLMNOPQRSTUWVXYZ")))))
  (testing "WhitespaceisFalse"
    (is (false? (isAlphaNumeric? (first (char-array " "))))))
  (testing "EmptyisFalse"
    (is (false? (isAlphaNumeric? (first (char-array ""))))))
  )

(deftest test-isWhitespace
  (testing "EmptyisFalse"
    (is (false? (isWhitespace?))))
  (testing "EmptyStringisFalse"
    (is (false? (isWhitespace? (first (char-array ""))))))
  (testing "SpaceIsTrue"
    (is (true? (isWhitespace? (first (char-array " "))) )))
  (testing "LetterIsTrue"
    (is (true? (isWhitespace? (first (char-array " "))))))
  (testing "NewlineIsTrue"
    (is (true? (isWhitespace? (first (char-array "\n"))))))
  )

(deftest test-isValidNameChar
  (testing "dashSlashHashIsTrue"
    (is (every? true? (map #(isValidNameChar? %) (seq "-_#")))))
  (testing "AlphaIsTrue"
    (is (every? true? (map #(isValidNameChar? %) (seq "abcdefghijklmnopqrstuwvxyzABCDEFGHIJKLMNOPQRSTUWVXYZ")))))
  (testing "numericIsTrue"
    (is (every? true? (map #(isValidNameChar? %) (seq "0123456789")))))
  (testing "someOtherStuffIsFalse"
    (is (every? false? (map #(isValidNameChar? %) (seq ":.$?<>")))))
  )
(run-tests)
