(ns ^{:doc "Protocols and default Reader types implementation"
      :author "Boston"}
  cljs.tools.reader.reader-types
  (:refer-clojure :exclude [char read-line])
  (:require [cljs.tools.reader.impl.utils :refer [char newline?]])
  (:require-macros [cljs.tools.reader.reader-types :refer [update!]])
  (:import goog.string.StringBuffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Reader
  (read-char [reader]
    "Returns the next char from the Reader, nil if the end of stream has been reached")
  (peek-char [reader]
    "Returns the next char from the Reader without removing it from the reader stream"))

(defprotocol IPushbackReader
  (unread [reader ch]
    "Pushes back a single character on to the stream"))

(defprotocol IndexingReader
  (get-line-number [reader]
    "Returns the line number of the next character to be read from the stream")
  (get-column-number [reader]
    "Returns the column number of the next character to be read from the stream")
  (get-file-name [reader]
    "Returns the file name the reader is reading from, or nil"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader deftypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype StringReader
  [s s-len ^:unsynchronized-mutable s-pos]
  Reader
  (read-char [_reader]
    (when (> s-len s-pos)
      (let [r (nth s s-pos)]
        (update! s-pos inc)
        r)))
  (peek-char [_reader]
    (when (> s-len s-pos)
      (nth s s-pos))))

(deftype PushbackReader
  [rdr buf buf-len ^:unsynchronized-mutable buf-pos]
  Reader
  (read-char [_reader]
    (char
      (if (< buf-pos buf-len)
        (let [r (aget buf buf-pos)]
          (update! buf-pos inc)
          r)
        (read-char rdr))))
  (peek-char [_reader]
    (char
      (if (< buf-pos buf-len)
        (aget buf buf-pos)
        (peek-char rdr))))
  IPushbackReader
  (unread [_reader ch]
    (when ch
      (if (zero? buf-pos) (throw (js/Error. "Pushback buffer is full")))
      (update! buf-pos dec)
      (aset buf buf-pos ch))))

(defn- normalize-newline [rdr ch]
  (if (identical? \return ch)
    (let [c (peek-char rdr)]
      (when (or (identical? \formfeed c)
                (identical? \newline c))
        (read-char rdr))
      \newline)
    ch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source Logging support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn merge-meta
  "Returns an object of the same type and value as `obj`, with its
metadata merged over `m`."
  [obj m]
  (let [orig-meta (meta obj)]
    (with-meta obj (merge m (dissoc orig-meta :source)))))

(defn- peek-source-log
  "Returns a string containing the contents of the top most source
logging frame."
  [source-log-frames]
  (let [current-frame @source-log-frames]
    (subs (str ^StringBuffer (:buffer current-frame))
          (:offset current-frame))))

(defn- log-source-char
  "Logs `char` to all currently active source logging frames."
  [source-log-frames char]
  (when-let [^StringBuffer buffer (:buffer @source-log-frames)]
    (.append buffer char)))

(defn- drop-last-logged-char
  "Removes the last logged character from all currently active source
logging frames. Called when pushing a character back."
  [source-log-frames]
  (when-let [^StringBuffer buffer (:buffer @source-log-frames)]
    (let [s (str buffer)]
      (.set buffer (subs s 0 (dec (count s)))))))

(deftype SourceLoggingPushbackReader
  [rdr ^:unsynchronized-mutable line ^:unsynchronized-mutable column
   ^:unsynchronized-mutable line-start? ^:unsynchronized-mutable prev
   ^:unsynchronized-mutable prev-column file-name source-log-frames]
  Reader
  (read-char [_reader]
    (when-let [ch (read-char rdr)]
      (let [ch (normalize-newline rdr ch)]
        (set! prev line-start?)
        (set! line-start? (newline? ch))
        (when line-start?
          (set! prev-column column)
          (set! column 0)
          (update! line inc))
        (update! column inc)
        (log-source-char source-log-frames ch)
        ch)))

  (peek-char [_reader]
    (peek-char rdr))

  IPushbackReader
  (unread [_reader ch]
    (if line-start?
      (do (update! line dec)
          (set! column prev-column))
      (update! column dec))
    (set! line-start? prev)
    (when ch
      (drop-last-logged-char source-log-frames))
    (unread rdr ch))

  IndexingReader
  (get-line-number [_reader] (int line))
  (get-column-number [_reader] (int column))
  (get-file-name [_reader] file-name))

(defn log-source*
  [reader f]
  (let [frame (.-source-log-frames ^SourceLoggingPushbackReader reader)
        ^StringBuffer buffer (:buffer @frame)
        new-frame (assoc-in @frame [:offset] (.getLength buffer))]
    (let [ret (f)]
      (if (instance? IMeta ret)
        (merge-meta ret {:source (peek-source-log new-frame)})
        ret))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fast check for provided implementations
(defn indexing-reader?
  "Returns true if the reader satisfies IndexingReader"
  [rdr]
  (satisfies? IndexingReader rdr))

(defn string-reader
  "Creates a StringReader from a given string"
  ([s]
   (StringReader. s (count s) 0)))

(defn string-push-back-reader
  "Creates a PushbackReader from a given string"
  ([s]
   (string-push-back-reader s 1))
  ([s buf-len]
   (PushbackReader. (string-reader s) (object-array buf-len) buf-len buf-len)))

(defn source-logging-reader?
  [rdr]
  (instance? SourceLoggingPushbackReader rdr))

(defn reader-error
  "Throws an ExceptionInfo with the given message.
   If rdr is an IndexingReader, additional information about column and line number is provided"
  [rdr & msg]
  (throw (ex-info (apply str msg)
                  (merge {:type :reader-exception}
                         (when (indexing-reader? rdr)
                           (merge
                             {:line (get-line-number rdr)
                              :column (get-column-number rdr)}
                             (when-let [file-name (get-file-name rdr)]
                               {:file file-name})))))))
