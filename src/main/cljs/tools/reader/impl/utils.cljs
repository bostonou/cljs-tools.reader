(ns ^:skip-wiki cljs.tools.reader.impl.utils
  (:refer-clojure :exclude [char]))

(defn char [x]
  (when x
    (cljs.core/char x)))
