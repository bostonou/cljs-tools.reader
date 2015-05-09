(ns ^{:doc "A cljs reader in cljs"
      :author "Boston"}
  cljs.tools.reader
  (:require [cljs.tools.reader.reader-types :refer [string-push-back-reader]]))

(defn- read*
  ([reader eof-error? sentinel opts pending-forms]
    (read* reader eof-error? sentinel nil opts pending-forms))
  ([reader eof-error? sentinel return-on opts pending-forms]
    (when (= :unknown *read-eval*)
      (reader-error "Reading disallowed - *read-eval* bound to :unknown"))))

(defn read
  "Reads the first object from an IPushbackReader.
   Returns the object read. If EOF, throws if eof-error? is true.
   Otherwise returns sentinel. If no stream is provided, *in* will be used.

   Opts is a persistent map with valid keys:
    :read-cond - :allow to process reader conditionals, or
                 :preserve to keep all branches
    :features - persistent set of feature keywords for reader conditionals
    :eof - on eof, return value unless :eofthrow, then throw.
           if not specified, will throw

   ***WARNING***
   Note that read can execute code (controlled by *read-eval*),
   and as such should be used only with trusted sources.

   To read data structures only, use cljs.tools.reader.edn/read

   Note that the function signature of cljs.tools.reader/read and
   cljs.tools.reader.edn/read is not the same for eof-handling"
  {:arglists '([] [reader] [opts reader] [reader eof-error? eof-value])}
  #_([] (read *in* true nil))
  ([reader] (read reader true nil))
  ([{eof :eof :as opts :or {eof :eofthrow}} reader] (read* reader (= eof :eofthrow) eof nil opts (LinkedList.)))
  ([reader eof-error? sentinel] (read* reader eof-error? sentinel nil {} (LinkedList.))))

(defn read-string
  "Reads one object from the string s.
   Returns nil when s is nil or empty.

   ***WARNING***
   Note that read-string can execute code (controlled by *read-eval*),
   and as such should be used only with trusted sources.

   To read data structures only, use clojure.tools.reader.edn/read-string

   Note that the function signature of clojure.tools.reader/read-string and
   clojure.tools.reader.edn/read-string is not the same for eof-handling"
  ([s]
   (read-string {} s))
  ([opts s]
   (when (and s (not (identical? s "")))
     (read opts (string-push-back-reader s)))))