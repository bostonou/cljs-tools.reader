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

(enable-console-print!)
(run-tests)
