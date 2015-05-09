(ns cljs.tools.reader.impl.commons
  (:require [cljs.tools.reader.reader-types :refer [peek-char]]
            [goog.string :as gstring]))

(defn number-literal?
  "Checks whether the reader is at the start of a number literal"
  [reader initch]
  (or (gstring/isNumeric initch)
      (and (or (identical? \+ initch) (identical?  \- initch))
           (gstring/isNumeric (peek-char reader)))))

(def int-pattern #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?")
#_(def ratio-pattern #"([-+]?[0-9]+)/([0-9]+)")
(def float-pattern #"([-+]?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?")

(defn- match-int
  [^Matcher m]
  (if (.group m 2)
    (if (.group m 8) 0N 0)
    (let [negate? (= "-" (.group m 1))
          a (cond
              (.group m 3) [(.group m 3) 10]
              (.group m 4) [(.group m 4) 16]
              (.group m 5) [(.group m 5) 8]
              (.group m 7) [(.group m 7) (Integer/parseInt (.group m 6))]
              :else        [nil nil])
          ^String n (a 0)
          radix (int (a 1))]
      (when n
        (let [bn (BigInteger. n radix)
              bn (if negate? (.negate bn) bn)]
          (if (.group m 8)
            (BigInt/fromBigInteger bn)
            (if (< (.bitLength bn) 64)
              (.longValue bn)
              (BigInt/fromBigInteger bn))))))))

#_(defn- match-ratio
  [^Matcher m]
  (let [^String numerator (.group m 1)
        ^String denominator (.group m 2)
        numerator (if (.startsWith numerator "+")
                    (subs numerator 1)
                    numerator)]
    (/ (-> numerator   BigInteger. BigInt/fromBigInteger Numbers/reduceBigInt)
       (-> denominator BigInteger. BigInt/fromBigInteger Numbers/reduceBigInt))))

(defn- match-float
  [^String s ^Matcher m]
  (if (.group m 4)
    (BigDecimal. ^String (.group m 1))
    (Double/parseDouble s)))

(defn match-number [^String s]
  (let [int-matcher (.matcher int-pattern s)]
    (if (.matches int-matcher)
      (match-int int-matcher)
      (let [float-matcher (.matcher float-pattern s)]
        (if (.matches float-matcher)
          (match-float s float-matcher)
          #_(let [ratio-matcher (.matcher ratio-pattern s)]
            (when (.matches ratio-matcher)
              (match-ratio ratio-matcher))))))))