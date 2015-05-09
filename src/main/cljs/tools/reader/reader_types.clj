(ns cljs.tools.reader.reader-types)

(defmacro ^:private update! [what f]
  (list 'set! what (list f what)))
