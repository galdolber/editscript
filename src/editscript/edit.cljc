;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns ^:no-doc editscript.edit
  #?(:clj (:import [clojure.lang PersistentVector IPersistentList IPersistentMap
                    IPersistentSet IPersistentVector MapEntry]
                   [java.util Map$Entry])))

(defprotocol IEdit
  (add-data [this path value])
  (delete-data [this path])
  (replace-data [this path value])
  (replace-str [this path ops level]))

(defprotocol IEditScript
  (combine [this that]
    "Concate that editscript onto this editscript, return the new editscript")
  (get-edits [this] "Report the edits as a vector"))

(defprotocol IType
  (get-type [this] "Return a type keyword, :val, :map, :lst, etc."))

(defn nada
  "A special type means 'not present'"
  []
  (reify IType
    (get-type [_] :nil)))

#?(:clj
   (extend-protocol IType
     IPersistentList
     (get-type [_] :lst)

     IPersistentMap
     (get-type [_] :map)

     IPersistentVector
     (get-type [_] :vec)

     IPersistentSet
     (get-type [_] :set)

     Map$Entry
     (get-type [_] :val)

     MapEntry
     (get-type [_] :val)

     nil
     (get-type [_] :val)

     String
     (get-type [_] :str)

     Object
     (get-type [_] :val))

   :cljs
   (extend-protocol IType
     List
     (get-type [_] :lst)

     EmptyList
     (get-type [_] :lst)

     Cons
     (get-type [_] :lst)

     PersistentArrayMap
     (get-type [_] :map)

     PersistentHashMap
     (get-type [_] :map)

     PersistentTreeMap
     (get-type [_] :map)

     PersistentVector
     (get-type [_] :vec)

     Subvec
     (get-type [_] :vec)

     MapEntry
     (get-type [_] :val)

     PersistentHashSet
     (get-type [_] :set)

     PersistentTreeSet
     (get-type [_] :set)

     nil
     (get-type [_] :val)

     string
     (get-type [_] :str)

     default
     (get-type [_] :val)))

(deftype ^:no-doc EditScript [^:unsynchronized-mutable ^PersistentVector edits]

  IEdit
  (add-data [this path value]
    (locking this
      (set! edits (conj edits [path :+ value]))))
  (delete-data [this path]
    (locking this
      (set! edits (conj edits [path :-]))))
  (replace-data [this path value]
    (locking this
      (set! edits (conj edits [path :r value]))))
  (replace-str [this path ops level]
    (locking this
      (set! edits (conj edits [path
                               (case level
                                 :character :s
                                 :word      :sw
                                 :line      :sl)
                               ops]))))

  IEditScript
  (combine [_ that]
    (EditScript. (into edits (get-edits that))))
  (get-edits [_] edits)

  #?(:cljs IEquiv)
  #?(:cljs
     (-equiv [_ other]
             (and (instance? EditScript other)
                  (= edits (get-edits other)))))
  #?(:cljs IHash)
  #?(:cljs (-hash [_] (hash edits)))

  Object
  #?(:cljs (equiv [this other] (-equiv this other)))
  #?(:clj (equals [_ other] (and (instance? EditScript other)
                                 (= edits (get-edits other)))))
  #?(:clj (hashCode [_] (hash edits))))

(defn- valid-str-edits?
  [data level]
  (and (vector? data)
       (every? (fn [x]
                 (or (nat-int? x)
                     (and (vector? x)
                          (= 2 (count x))
                          (let [[op y] x]
                            (and
                              (#{:- :r :+} op)
                              (case op
                                :-      (nat-int? y)
                                (:+ :r) (case level
                                          :s        (string? y)
                                          (:sl :sw) (vector? y))))))))
               data)))

(defn- valid-edit?
  [edit]
  (when (vector? edit)
    (let [c (count edit)]
      (when (< 1 c 4)
        (let [[path op data] edit]
          (and (vector? path)
               (#{:- :r :+ :s :sw :sl} op)
               (if (= :- op) (nil? data) (= c 3))
               (if (#{:s :sw :sl} op)
                 (valid-str-edits? data op)
                 true)))))))

(defn valid-edits?
  [edits]
  (when (vector? edits)
    (if (seq edits)
      (every? valid-edit? edits)
      true)))

(defn edits->script
  "Create an EditScript instance from a vector of edits, like those obtained
  through calling `get-edits` on an EditScript"
  [edits]
  (assert (valid-edits? edits) "Not a vector of valid edits")
  (->EditScript edits))

(defn edit-script? [x]
  (instance? EditScript x))


#?(:clj (defmethod print-method EditScript
          [x ^java.io.Writer writer]
          (print-method (get-edits x) writer))
   :cljs (extend-protocol IPrintWithWriter
           EditScript
           (-pr-writer [o writer _]
             (write-all writer (str (get-edits o))))))
