(ns editscript.test
  (:require  [doo.runner :refer-macros [doo-tests]]
             [editscript.core-test]
             [editscript.util.pairing-test]
             [editscript.diff.a-star-test]))

(doo-tests 'editscript.util.pairing-test
           'editscript.diff.a-star-test
           'editscript.core-test)
