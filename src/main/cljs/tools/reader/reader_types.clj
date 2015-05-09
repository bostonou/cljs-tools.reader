(ns cljs.tools.reader.reader-types)

(defmacro ^:private update! [what f]
  (list 'set! what (list f what)))

(defmacro log-source
  "If reader is a SourceLoggingPushbackReader, execute body in a source
  logging context. Otherwise, execute body, returning the result."
  [reader & body]
  `(if (and (cljs.tools.reader.reader-types/source-logging-reader? ~reader)
            (not (cljs.tools.reader.impl.utils/whitespace?
                   (cljs.tools.reader.reader-types/peek-char ~reader))))
     (cljs.tools.reader.reader-types/log-source* ~reader (^:once fn* [] ~@body))
     (do ~@body)))
