(ns cljs.tools.reader-test
  (:refer-clojure :exclude [read-string])
  (:require
    [cljs.test :as t :refer-macros [deftest is run-tests]]
    [cljs.tools.reader :refer [read-string]]))

;;==============================================================================
;; common_tests.clj
;;==============================================================================

(deftest read-integer
  (is (== 42 (read-string "42")))
  (is (== +42 (read-string "+42")))
  (is (== -42 (read-string "-42")))

  (is (== 0 (read-string "0")))

  (is (== 042 (read-string "042")))
  (is (== +042 (read-string "+042")))
  (is (== -042 (read-string "-042")))

  ;;hex
  (is (== 0x42e (read-string "0x42e")))
  (is (== +0x42e (read-string "+0x42e")))
  (is (== -0x42e (read-string "-0x42e")))

  ;;oct
  (is (== 511 (js/parseInt "777" 8) (read-string "0777")))
  (is (== -511 (js/parseInt "-777" 8) (read-string "-0777")))
  (is (== 1340 (js/parseInt "02474" 8) (read-string "02474")))
  (is (== -1340 (js/parseInt "-02474" 8) (read-string "-02474")))

  ;;parse oct as decimal
  (is (== 888 (js/parseInt "0888" 10) (read-string "0888")))
  (is (== -888 (js/parseInt "-0888" 10) (read-string "-0888")))
  (is (== 4984 (js/parseInt "04984" 10) (read-string "04984")))
  (is (== -4984 (js/parseInt "-04984" 10) (read-string "-04984")))

  ;;binary
  (is (== 2147483648
          (js/parseInt "10000000000000000000000000000000" 2)
          (read-string "0b10000000000000000000000000000000")))
  (is (== -2147483648
          (js/parseInt "-10000000000000000000000000000000" 2)
          (read-string "-0b10000000000000000000000000000000")))
  (is (== 2139095040
          (js/parseInt "01111111100000000000000000000000" 2)
          (read-string "0b01111111100000000000000000000000")))
  (is (== -2139095040
          (js/parseInt "-01111111100000000000000000000000" 2)
          (read-string "-0b01111111100000000000000000000000")))
  (is (== 8388607
          (js/parseInt "00000000011111111111111111111111" 2)
          (read-string "0B00000000011111111111111111111111")))
  (is (== -8388607
          (js/parseInt "-00000000011111111111111111111111" 2)
          (read-string "-0B00000000011111111111111111111111")))
)

(deftest read-floating
  (is (== 42.23 (read-string "42.23")))
  (is (== +42.23 (read-string "+42.23")))
  (is (== -42.23 (read-string "-42.23")))

  (is (== 42.2e3 (read-string "42.2e3")))
  (is (== +42.2e+3 (read-string "+42.2e+3")))
  (is (== -42.2e-3 (read-string "-42.2e-3")))
)

#_(deftest read-ratio
  (is (== 4/2 (read-string "4/2")))
  (is (== 4/2 (read-string "+4/2")))
  (is (== -4/2 (read-string "-4/2"))))

(deftest read-symbol
  (is (= 'foo (read-string "foo")))
  (is (= 'foo/bar (read-string "foo/bar")))
  (is (= '*+!-_? (read-string "*+!-_?")))
  (is (= 'abc:def:ghi (read-string "abc:def:ghi")))
  (is (= 'abc.def/ghi (read-string "abc.def/ghi")))
  (is (= 'abc/def.ghi (read-string "abc/def.ghi")))
  (is (= 'abc:def/ghi:jkl.mno (read-string "abc:def/ghi:jkl.mno")))
  (is (instance? cljs.core/Symbol (read-string "alphabet")))
  (is (= "foo//" (str (read-string "foo//"))))
  (is (js/isNaN (read-string "NaN"))) ;; not sure if this should be js/NaN
  (is (= js/Number.POSITIVE_INFINITY (read-string "Infinity"))) ;; not sure if this should be js version of Infinity
  (is (= js/Number.POSITIVE_INFINITY (read-string "+Infinity"))) ;; not sure if this should be js version of Infinity
  (is (= js/Number.NEGATIVE_INFINITY (read-string "-Infinity"))) ;; not sure if this should be js version of Infinity
)

(enable-console-print!)
(run-tests)
