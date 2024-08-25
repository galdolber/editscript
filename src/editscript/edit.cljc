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
  #?(:clj (:import [clojure.lang IPersistentList IPersistentMap
                    IPersistentSet IPersistentVector MapEntry]
                   [java.util Map$Entry])))

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

(defn add-data [edits path value]
  (conj! edits [path :+ value]))

(defn delete-data [edits path]
  (conj! edits [path :-]))

(defn replace-data [edits path value]
  (conj! edits [path :r value]))

(defn replace-str [edits path ops level]
  (conj! edits [path
                (case level
                  :character :s
                  :word      :sw
                  :line      :sl)
                ops]))


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
