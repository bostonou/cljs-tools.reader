(ns cljs.tools.reader.impl.commons
  (:require [cljs.tools.reader.reader-types :refer [peek-char]]
            [cljs.tools.reader.impl.utils :refer [numeric?]]
            [goog.string :as gstring]))

(defn number-literal?
  "Checks whether the reader is at the start of a number literal"
  [reader initch]
  (or (gstring/isNumeric initch)
      (and (or (identical? \+ initch) (identical?  \- initch))
           (gstring/isNumeric (peek-char reader)))))

(def int-pattern #"^([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|0([0-7]*[8-9]+[0-7]*)|0[bB]([0-1]+))$")
#_(def ratio-pattern #"([-+]?[0-9]+)/([0-9]+)")
(def float-pattern #"^[-+]?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?$")

(defn- match-int
  [m]
  (if (aget m 2)
    0
    (let [negate? (= "-" (aget m 1))
          [n-str radix] (cond
                          (aget m 3) [(aget m 3) 10]
                          (aget m 4) [(aget m 4) 16]
                          (aget m 5) [(aget m 5) 8]
                          (aget m 6) [(aget m 6) 10]
                          (aget m 7) [(aget m 7) 2]
                          :else [nil nil])]
      (when n-str
        (let [n-int (js/parseInt n-str radix)]
          (if negate?
            (* -1 n-int)
            n-int))))))

#_(defn- match-ratio
  [^Matcher m]
  (let [^String numerator (.group m 1)
        ^String denominator (.group m 2)
        numerator (if (.startsWith numerator "+")
                    (subs numerator 1)
                    numerator)]
    (/ (-> numerator   BigInteger. BigInt/fromBigInteger Numbers/reduceBigInt)
       (-> denominator BigInteger. BigInt/fromBigInteger Numbers/reduceBigInt))))

(defn match-number [s]
  ;;js RegExp with "g" are stateful
  (let [int-matcher (js/RegExp. (.-source int-pattern) "g")]
    (if-let [int-matches (.exec int-matcher s)]
      (match-int int-matches)
      (let [float-matcher (js/RegExp. (.-source float-pattern) "g")]
        (if (.test float-matcher s)
          (js/parseFloat s)
          #_(let [ratio-matcher (js/RegExp. (.-source ratio-pattern) "g")]
            (when-let [ratio-matches (.exec ratio-matcher)]
              (match-ratio ratio-matches))))))))

(defn parse-symbol
  "Parses a string into a vector of the namespace and symbol"
  [token]
  (when-not (or (= "" token)
                (.endsWith token ":")
                (.startsWith token "::"))
    (let [ns-idx (.indexOf token "/")]
      (if-let [ns (and (pos? ns-idx)
                       (subs token 0 ns-idx))]
        (let [ns-idx (inc ns-idx)]
          (when-not (== ns-idx (count token))
            (let [sym (subs token ns-idx)]
              (when (and (not (numeric? (nth sym 0)))
                         (not (= "" sym))
                         (not (.endsWith ns ":"))
                         (or (= sym "/")
                             (== -1 (.indexOf sym "/"))))
                [ns sym]))))
        (when (or (= token "/")
                  (== -1 (.indexOf token "/")))
          [nil token])))))