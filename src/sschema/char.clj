(ns sschema.char)

(def char_int {:a (int \a), :A (int \A), :z (int \z), :Z (int \Z),  :0 (int \0), :9 (int \9)})
 
(defn between?
  [lower, upper, ^Integer value]
  (let [lessThanEqualTo (>= (upper char_int) value)
        moreThanEqualTo (<= (lower char_int) value)]
    (and lessThanEqualTo moreThanEqualTo)))
 
(defn isAlphaNumeric?
  ([] false)
  ([^Character ch]
     (if (nil? ch) false
         (let [intValue (int ch)]
           (or
            (or (between? :a :z intValue)
                (between? :A :Z intValue))
            (between? :0 :9 intValue))))
     ))
  
(defn isWhitespace?
  ([]  false)
  ([ch] (if (nil? ch) false (Character/isWhitespace ch))))
  
(defn isValidNameChar?
  [^Character ch]
  (if (isAlphaNumeric? ch)
    true
    (or (= \- ch) (= \_ ch) (= \# ch))))
