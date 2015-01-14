(ns sschema.reader
  (:require [sschema.char :refer :all])
  (:import (java.io InputStream)))

(defmacro ^:private update! [what f]
    (list 'set! what (list f what)))

(defprotocol Reader
  (read-char [reader]
    "Returns the next char from the Reader, nil if the end of stream has been reached")
  (peek-char [reader]
    "Returns the next char from the Reader without removing it from the reader stream"))

(defprotocol IndexingReader 
  (get-column-number [reader])
  (get-line-number [reader]))
 
(deftype InputStreamReader [^InputStream is ^:unsynchronized-mutable ^"[B" buf]
  Reader
  (read-char [reader]
    (if buf
      (let [c (aget buf 0)]
        (set! buf nil)
        (char c))
      (let [c (.read is)]
        (when (>= c 0)
          (char c)))))
  (peek-char [reader]
    (when-not buf
      (set! buf (byte-array 1))
      (when (== -1 (.read is buf))
        (set! buf nil)))
    (when buf
      (char (aget buf 0)))))

(defn- normalize-newline [rdr ch]
  (if (identical? \return ch)
    (let [c (peek-char rdr)]
      (when (or (identical? \formfeed c)
                (identical? \newline c))
        (read-char rdr))
      \newline)
        ch))

(deftype IndexingInputStreamReader
    [^::Reader rdr ^:unsynchronized-mutable column  ^:unsynchronized-mutable line]
  Reader
  (read-char [reader]
    (when-let [ch (read-char rdr)]
      (let [ch (normalize-newline rdr ch)]
        (when (newline? ch)
          (set! column 0)
          (update! line inc))
        (update! column inc)
        ch)))
  (peek-char [reader]
    (peek-char rdr))
  IndexingReader
  (get-line-number [reader]
    (int line))
  (get-column-number [reader]
    (int column)))

(defn ^::IndexingInputStreamReader indexing-input-stream-reader
  [^::Reader rdr]
  (IndexingInputStreamReader. rdr 1 1))
 
(defn ^::Reader input-stream-reader
  "Creates an InputStreamReader from an InputStream"
  [^InputStream is]
  (InputStreamReader. is nil))
 
(defn ^::Reader skip-char
  "Reads the next char from the reader and returns the reader."
  [^::Reader reader]
  (read-char reader)
  reader)
 
(defn ^InputStream  make-inputstream
  [^String theStr]
  (->> (.getBytes theStr) (java.io.ByteArrayInputStream.)))

;;;

(defn at-line-column-number
  [line col]
  (when (and line col) (str "At line "line " column "col " ")))

(defn throw-parse-error
  ([tag, ^String msg]
     (throw-parse-error nil nil tag msg))
  ([rdr,  tag, ^String msg]
     (if (satisfies? IndexingReader rdr)
       (throw-parse-error (get-line-number rdr) (get-column-number rdr) tag msg)
       (throw-parse-error tag msg)))
  ([line col tag msg]
    (throw (IllegalArgumentException. (str "Error occurred "(at-line-column-number col line) tag": "msg)))))

(defn- match-seq
  [^::Reader reader, match?]
  (when (match? (peek-char reader))
    (cons (read-char reader) (lazy-seq (match-seq reader match?)))))

(defn ^String parse-name
  "Returns a string of valid name chars read from the reader, 
  stops when the chars read are no longer valid name chars."
  [^::Reader reader]
  (apply str (match-seq reader isValidNameChar?)))

(defn ^String parse-whitespace
  "Returns a string of whitespace chars read from the reader,
  stops when the chars read are no longer valid whitespace."
  [^::Reader reader]
  (apply str (match-seq reader isWhitespace?)))

(defn ^String parse-text
  "Returns a string of valid text read from the reader,
  stop when the chars are xml control characters."
  [^::Reader reader]
  (apply str (match-seq reader #(when % (isValidTextChar? %)))))

(defn- quote-seq
  [^::Reader reader]
  (let [ch (read-char reader)]
    (if (nil? ch)
      (throw-parse-error reader :Quote " Missing end quote")
      (when-not (= \" ch)
        (cons ch (lazy-seq (quote-seq reader)))))))

(defn ^String parse-quoted
  "Returns are string read from the reader,
  stops when the quote character is the next char."
  [^::Reader reader]
  (when (= \" (peek-char reader))
    (apply str (quote-seq (skip-char reader)))))

(defn- ctrl-seq
  [^::Reader reader]
  (let [peek (peek-char reader)]
    (when (isCtrlChar? peek)
      (cons (read-char reader)
            (lazy-seq (if (= \> peek) 
                        nil
                        (ctrl-seq reader)))))))

(defn parse-ctrl
  "Return a string of Xml control character, stops when either 
  end tag is encountered or non control character is the next char."
  [^::Reader reader]
  (apply str (ctrl-seq reader)))

(defn- cdata-seq
  [^::Reader reader buf]
  (let [ch (read-char reader)]
    (when ch
      (cond
        (= \> ch) (if (= '(\] \]) buf)
                    nil
                    (cons ch (lazy-seq (cdata-seq reader nil))))
        (= \] ch) (cons ch (lazy-seq (cdata-seq reader (conj (vec buf) ch))))
        :else (cons ch (lazy-seq (cdata-seq reader nil)))))))

(defn parseCdata
  [^::Reader reader]
  (apply str (-> (cdata-seq reader nil) butlast butlast)))

(defn- comment-seq
  [^::Reader reader buf]
  (let [ch (read-char reader)]
    (when ch
      (cond
        (= \> ch) (if (= [\- \-] buf)
                      nil
                      (throw-parse-error reader :Comment " close tag in comment body"))
        (= \- ch) (cons ch (lazy-seq (comment-seq reader (conj (vec buf) ch))))
        :else (cons ch (lazy-seq (comment-seq reader nil)))))))

(defn parseComment
  [^::Reader reader]
  (apply str (-> (comment-seq reader nil) butlast butlast)))
