(ns routine-sequence-qc.core-test
  (:require [cljs.test :refer [deftest is testing]]
            [routine-sequence-qc.core :as core]))


(deftest null-test
  (testing "Placeholder. Should always pass."
    (is (= 1 1))))
