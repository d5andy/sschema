(ns sschema.core-test
  (:import (java.io InputStream))
  (:require [clojure.test :refer :all])
  (:require [sschema.core :refer :all])
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
                 #(read-char %)))))
  )

(deftest test-StringParserWriter
  (testing "addingAndDumping"
    (is (= " :attr {\"blah\" \"value\"}"
           (let [writer (sschema.core.StringParserWriter. nil)]
             (attributes writer {"blah" "value"})
             (dump writer)))))
  (testing "empty content"
    (is (nil?
         (let [writer (sschema.core.StringParserWriter. nil)]
           (content writer nil)
           (dump writer)))))
  (testing "empty with dump"
    (is (nil?
         (dump (sschema.core.StringParserWriter. nil)))))
  )

(deftest test-parseName
  (testing "parseName"
    (is (= "bean"
           (parseName (input-stream-reader (make-inputstream "bean>")))))))

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

(deftest test-parseTagData
  (testing "commentTagData"
    (is (nil?
           (let [writer (sschema.core.StringParserWriter. nil)
                 reader (input-stream-reader (make-inputstream "!-- comment -->"))]
             (parseTagData reader writer)
             (dump writer)
             ))))
  (testing "cdataTagData"
    (is (= ":content \" Hello \"" 
           (let [writer (sschema.core.StringParserWriter. nil)
                 reader (input-stream-reader (make-inputstream "![CDATA[ Hello ]]>"))]
             (parseTagData reader writer)
             (dump writer)
             ))))
  (testing "elementEndTagData"
    (is (= " }"
         (let [writer (sschema.core.StringParserWriter. nil)
               reader (input-stream-reader (make-inputstream "/endof>"))]
           (parseTagData reader writer)
           (dump writer)
           )))
)
  (testing "elementStartTagData"
    (is (= "{:tag newele }"
           (let [writer (sschema.core.StringParserWriter. nil)
                 reader (input-stream-reader (make-inputstream "newele/>"))]
             (parseTagData reader writer)
             (dump writer)
             ))))
  )

(deftest test-parseElementBody
  (is (= ":content were"
         (let [writer (sschema.core.StringParserWriter. nil)
               reader (input-stream-reader (make-inputstream " were "))]
           (parseElementBody reader writer)
           (dump writer)
           ))))

(deftest test-processElement
  (testing "parseSingleElement"
    (is (= "{:tag bean :attr {\"name\" \"value\", \"sec\" \"ond\"} }"
           (let [writer (sschema.core.StringParserWriter. nil)
                 reader (input-stream-reader (make-inputstream "bean name =\"value\" sec=\"ond\"/>"))]
             (parseElement reader writer)
             (dump writer)))))
  (testing "parseNestedElement"
    (is (= "{:tag bean :attr {\"name\" \"value\", \"sec\" \"ond\"} :children { }"
           (let [writer (sschema.core.StringParserWriter. nil)
                 reader (input-stream-reader (make-inputstream "bean name =\"value\" sec=\"ond\"><nested/></bean>"))]
             (parseElement reader writer)
             (dump writer)))))
  )
(run-tests)
