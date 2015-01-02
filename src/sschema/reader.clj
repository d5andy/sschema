(ns sschema.reader
  (:require [sschema.char :refer :all])
  (:import (java.io InputStream)))

(defprotocol Reader
  (read-char [reader]
    "Returns the next char from the Reader, nil if the end of stream has been reached")
  (peek-char [reader]
        "Returns the next char from the Reader without removing it from the reader stream"))
 
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
 
(defn ^::Reader input-stream-reader
  "Creates an InputStreamReader from an InputStream"
  [^InputStream is]
  (InputStreamReader. is nil))
 
(defn skip-char
  [^::Reader reader]
  (read-char reader)
  reader)
 
(defn ^InputStream  make-inputstream
  [^String theStr]
  (->> (.getBytes theStr) (java.io.ByteArrayInputStream.)))

;; seek functions
(defn seek
  [^::Reader reader, match?, continueSeek?, munch]
  (loop []
    (let [peekCh (peek-char reader)]
      (cond
        (match? peekCh) (munch reader)
        (continueSeek? peekCh) (do
                                 (read-char reader)
                                 (recur))))))
 
(defn seekCharIgnoreWhitespace
  [^::Reader reader, ch, munch]
  (seek reader #(= ch %) isWhitespace? munch))
 
;;; parser stuff
 
(def ^:dynamic *name*)
(defn parseName
  [^::Reader reader]
  (binding [*name* []]
    (loop [ch (peek-char reader)]
      (cond
        (isValidNameChar? ch) (let [readCh (read-char reader)]
                                (set! *name* (conj *name* readCh))
                                (recur (peek-char reader)))))
    (apply str *name*)))
