(ns ^{:doc "Protocols and default Reader types implementation"
      :author "Boston"}
  cljs.tools.reader.reader-types
  (:refer-clojure :exclude [char read-line])
  (:require [cljs.tools.reader.impl.utils :refer [char]])
  (:require-macros [cljs.tools.reader.reader-types :refer [update!]]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fast check for provided implementations
(defn indexing-reader?
  "Returns true if the reader satisfies IndexingReader"
  [rdr]
  (or (instance? IndexingReader rdr)
      #_(instance? LineNumberingPushbackReader rdr)
      (and (not (instance? PushbackReader rdr))
           (not (instance? StringReader rdr))
           #_(not (instance? InputStreamReader rdr))
           (satisfies? IndexingReader rdr))))

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
