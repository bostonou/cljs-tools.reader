(ns ^:skip-wiki cljs.tools.reader.impl.utils
  (:refer-clojure :exclude [char])
  (:require [goog.string :as gstring])
  (:import [goog.string StringBuffer]))

(defn char [x]
  (when x
    (cljs.core/char x)))

(defn ex-info? [ex]
  (instance? ExceptionInfo ex))

(defn whitespace?
  "Checks whether a given character is whitespace"
  [ch]
  (when ch
    (or (gstring/isSpace ch)
        (identical? \,  ch))))

(defn newline?
  "Checks whether the character is a newline"
  [c]
  (or (identical? \newline c)
      (nil? c)))