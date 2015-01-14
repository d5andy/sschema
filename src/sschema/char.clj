(ns sschema.char)

(def char_int {:a (int \a), :A (int \A), :z (int \z), :Z (int \Z),  :0 (int \0), :9 (int \9)})
 
(defn between?
  [lower, upper, ^Integer value]
  (let [lessThanEqualTo (>= (upper char_int) value)
        moreThanEqualTo (<= (lower char_int) value)]
    (and lessThanEqualTo moreThanEqualTo)))
 
(defn isAlphaNumeric?
  [^Character ch]
  (when ch
    (let [intValue (int ch)]
      (or
       (or (between? :a :z intValue)
           (between? :A :Z intValue))
       (between? :0 :9 intValue)))))
  
(defn isWhitespace?
  [ch]
  (when ch (Character/isWhitespace ch)))

(defn isValidNameChar?
  [^Character ch]
  (if (isAlphaNumeric? ch)
    true
    (or (= \- ch) (= \_ ch) (= \# ch))))

(defn isValidTextChar?
  [^Character ch]
  (when ch
    (not (or (= \& ch) (= \< ch) (= \> ch)))))

(def ctrl-char-int #{(int \<) (int \>) (int \/) (int \!) (int \[) (int \]) (int \?) (int \-)})

(defn isCtrlChar?
  [^Character ch]
  (when ch (contains? ctrl-char-int (int ch))))

(defn newline?
  "Checks whether the character is a newline"
  [^Character ch]
  (or (identical? \newline ch)
      (nil? ch)))
