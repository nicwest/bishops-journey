(ns wtf.styx.chess-journey.tests
  [:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [wtf.styx.chess-journey.core :refer [bishop-path]]])


(deftest test-bishop-path
  (testing "single move paths"
    (is (= (bishop-path [0 0] [7 7]) [[7 7]])))
  (testing "two move paths"))

(run-tests)
