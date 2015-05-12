(ns ^{:doc "A cljs reader in cljs"
      :author "Boston"}
  cljs.tools.reader
  (:require [cljs.tools.reader.reader-types :refer [string-push-back-reader
                                                    reader-error
                                                    indexing-reader?
                                                    get-line-number
                                                    get-column-number
                                                    get-file-name
                                                    peek-char
                                                    read-char
                                                    unread]]
            [cljs.tools.reader.impl.commons :refer [number-literal?
                                                    match-number
                                                    parse-symbol]]
            [cljs.tools.reader.impl.utils :refer [ex-info? whitespace?
                                                  numeric?]]
            [goog.string :as gstring])
  (:require-macros [cljs.tools.reader.reader-types :refer [log-source]])
  (:import goog.string.StringBuffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare ^:private read*
         macros dispatch-macros
         ^:dynamic *read-eval*
         ^:dynamic *data-readers*
         ^:dynamic *default-data-reader-fn*
         ^:dynamic *suppress-read*
         default-data-readers)

(defn- macro-terminating? [ch]
  (case ch
    (\" \; \@ \^ \` \~ \( \) \[ \] \{ \} \\) true
    false))

(defn- read-token
  "Read in a single logical token from the reader"
  [rdr initch]
  (if-not initch
    (reader-error rdr "EOF while reading")
    (loop [sb (StringBuffer.) ch initch]
      (if (or (whitespace? ch)
              (macro-terminating? ch)
              (nil? ch))
        (do (when ch
              (unread rdr ch))
            (str sb))
        (recur (.append sb ch) (read-char rdr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- char->code [ch base]
  (let [code (js/parseInt ch base)]
    (if (js/isNaN code)
      -1
      code)))

(defn- read-unicode-char
  ([token offset length base]
   (let [l (+ offset length)]
     (when-not (== (count token) l)
       (throw (js/Error. (str "Invalid unicode character: \\" token))))
     (let [code (char->code (subs token offset) base)]
       (if (= code -1)
         (throw (js/Error. (str "Invalid unicode character: \\" token)))
         ;;if we recur like original code, we can know the exact digit that was wrong
         #_(throw (js/Error. (str "Invalid digit: " (nth token i))))
         (js/String.fromCharCode code)))))

  ([rdr initch base length exact?]
   (loop [i 1 uc (char->code initch base)]
     (if (== uc -1)
       (throw (js/Error. (str "Invalid digit: " initch)))
       (if-not (== i length)
         (let [ch (peek-char rdr)]
           (if (or (whitespace? ch)
                   (macros ch)
                   (nil? ch))
             (if exact?
               (throw (js/Error.
                        (str "Invalid character length: " i ", should be: " length)))
               (js/String.fromCharCode uc))
             (let [d (char->code ch base)]
               (read-char rdr)
               (if (== d -1)
                 (throw (js/Error. (str "Invalid digit: " ch)))
                 (recur (inc i) (+ d (* uc base)))))))
         (js/String.fromCharCode uc))))))

(def ^:private ^:const upper-limit (.charCodeAt \uD7ff 0))
(def ^:private ^:const lower-limit (.charCodeAt \uE000 0))

(defn- valid-octal? [token base]
  (<= (js/parseInt token base) 0377))

(defn- read-char*
  "Read in a character literal"
  [rdr backslash opts pending-forms]
  (let [ch (read-char rdr)]
    (if-not (nil? ch)
      (let [token (if (or (macro-terminating? ch)
                          (whitespace? ch))
                    (str ch)
                    (read-token rdr ch))
            token-len (count token)]
        (cond

          (== 1 token-len)  (nth token 0)

          (= token "newline") \newline
          (= token "space") \space
          (= token "tab") \tab
          (= token "backspace") \backspace
          (= token "formfeed") \formfeed
          (= token "return") \return

          (gstring/caseInsensitiveStartsWith token "u")
          (let [c (read-unicode-char token 1 4 16)
                ic (.charCodeAt c 0)]
            (if (and (> ic upper-limit)
                     (< ic lower-limit))
              (reader-error rdr "Invalid character constant: \\u" c)
              c))

          (gstring/caseInsensitiveStartsWith token "o")
          (let [len (dec token-len)]
            (if (> len 3)
              (reader-error rdr "Invalid octal escape sequence length: " len)
              (let [offset 1
                    base 8
                    uc (read-unicode-char token offset len base)]
                (if-not (valid-octal? (subs token offset) base)
                  (reader-error rdr "Octal escape sequence must be in range [0, 377]")
                  uc))))

          :else (reader-error rdr "Unsupported character: \\" token)))
      (reader-error rdr "EOF while reading character"))))

(defonce ^:private READ_EOF (js/Object.))
(defonce ^:private READ_FINISHED (js/Object.))

(defn ^:private starting-line-col-info [rdr]
  (when (indexing-reader? rdr)
    [(get-line-number rdr) (int (dec (get-column-number rdr)))]))

(defn ^:private ending-line-col-info [rdr]
  (when (indexing-reader? rdr)
    [(get-line-number rdr) (get-column-number rdr)]))

(defn- read-number
  [rdr initch]
  (loop [sb (.append (StringBuffer.) initch)
         ch (read-char rdr)]
    (if (or (whitespace? ch) (macros ch) (nil? ch))
      (let [s (str sb)]
        (unread rdr ch)
        (or (match-number s)
            (reader-error rdr "Invalid number format [" s "]")))
      (recur (doto sb (.append ch)) (read-char rdr)))))

(defn- escape-char [sb rdr]
  (let [ch (read-char rdr)]
    (case ch
      \t "\t"
      \r "\r"
      \n "\n"
      \\ "\\"
      \" "\""
      \b "\b"
      \f "\f"
      \u (let [ch (read-char rdr)]
           (if (== -1 (char->code ch 16) #_(Character/digit (int ch) 16))
             (reader-error rdr "Invalid unicode escape: \\u" ch)
             (read-unicode-char rdr ch 16 4 true)))
      (if (numeric? ch)
        (let [uc (read-unicode-char rdr ch 8 3 false)]
          (if-not (valid-octal? ch 8)
            (reader-error rdr "Octal escape sequence must be in range [0, 377]")
            uc))
        (reader-error rdr "Unsupported escape character: \\" ch)))))

(defn- read-string*
  [reader _ opts pending-forms]
  (loop [sb (StringBuffer.)
         ch (read-char reader)]
    (case ch
      nil (reader-error reader "EOF while reading string")
      \\ (recur (doto sb (.append (escape-char sb reader)))
                (read-char reader))
      \" (str sb)
      (recur (doto sb (.append ch)) (read-char reader)))))

(defn- read-symbol
  [rdr initch]
  (let [[line column] (starting-line-col-info rdr)]
    (when-let [token (read-token rdr initch)]
      (case token
        ;; special symbols
        "nil" nil
        "true" true
        "false" false
        "/" '/
        "NaN" js/NaN
        "-Infinity" js/Number.NEGATIVE_INFINITY
        ("Infinity" "+Infinity") js/Number.POSITIVE_INFINITY

        (or (when-let [p (parse-symbol token)]
              (with-meta (symbol (p 0) (p 1))
                         (when line
                           (merge
                             (when-let [file (get-file-name rdr)]
                               {:file file})
                             (let [[end-line end-column] (ending-line-col-info rdr)]
                               {:line line
                                :column column
                                :end-line end-line
                                :end-column end-column})))))
            (reader-error rdr "Invalid token: " token))))))

(defn- macros [ch]
  (case ch
    \" read-string*
;    \: read-keyword
;    \; read-comment
;    \' (wrapping-reader 'quote)
;    \@ (wrapping-reader 'clojure.core/deref)
;    \^ read-meta
;    \` read-syntax-quote ;;(wrapping-reader 'syntax-quote)
;    \~ read-unquote
;    \( read-list
;    \) read-unmatched-delimiter
;    \[ read-vector
;    \] read-unmatched-delimiter
;    \{ read-map
;    \} read-unmatched-delimiter
    \\ read-char*
;    \% read-arg
;    \# read-dispatch
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *read-eval*
  "Defaults to true.

   ***WARNING***
   This setting implies that the full power of the reader is in play,
   including syntax that can cause code to execute. It should never be
   used with untrusted sources. See also: clojure.tools.reader.edn/read.

   When set to logical false in the thread-local binding,
   the eval reader (#=) and *record/type literal syntax* are disabled in read/load.
   Example (will fail): (binding [*read-eval* false] (read-string \"#=(* 2 21)\"))

   When set to :unknown all reads will fail in contexts where *read-eval*
   has not been explicitly bound to either true or false. This setting
   can be a useful diagnostic tool to ensure that all of your reads
   occur in considered contexts."
  true)

(defn- read*
  ([reader eof-error? sentinel opts pending-forms]
    (read* reader eof-error? sentinel nil opts pending-forms))
  ([reader eof-error? sentinel return-on opts pending-forms]
    (when (= :unknown *read-eval*)
      (reader-error "Reading disallowed - *read-eval* bound to :unknown"))
   (try
     (loop []
       (log-source reader
         (if (seq pending-forms)
           (.remove ^List pending-forms 0)
           (let [ch (read-char reader)]
             (cond
               (whitespace? ch) (recur)
               (nil? ch) (if eof-error? (reader-error reader "EOF") sentinel)
               (= ch return-on) READ_FINISHED
               (number-literal? reader ch) (read-number reader ch)
               :else (let [f (macros ch)]
                       (if f
                         (let [res (f reader ch opts pending-forms)]
                           (if (identical? res reader)
                             (recur)
                             res))
                         (read-symbol reader ch))))))))
     (catch js/Error e
       (if (ex-info? e)
         (let [d (ex-data e)]
           (if (= :reader-exception (:type d))
             (throw e)
             (throw (ex-info (.getMessage e)
                             (merge {:type :reader-exception}
                                    d
                                    (if (indexing-reader? reader)
                                      {:line   (get-line-number reader)
                                       :column (get-column-number reader)
                                       :file   (get-file-name reader)}))
                             e))))
         (throw (ex-info (.getMessage e)
                         (merge {:type :reader-exception}
                                (if (indexing-reader? reader)
                                  {:line   (get-line-number reader)
                                   :column (get-column-number reader)
                                   :file   (get-file-name reader)}))
                         e)))))))

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
  ([{eof :eof :as opts :or {eof :eofthrow}} reader] (read* reader (= eof :eofthrow) eof nil opts (list)))
  ([reader eof-error? sentinel] (read* reader eof-error? sentinel nil {} (list))))

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