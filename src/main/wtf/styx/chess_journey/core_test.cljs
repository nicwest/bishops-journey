(ns wtf.styx.chess-journey.core-test
  [:require [cljs.test :refer-macros [deftest is testing]]
            [wtf.styx.chess-journey.core :as core]])


(defn human-bishop-legal-moves
  [sq]
  (into #{} (map core/coordinates (core/bishop-legal-moves sq))))

(defn human-bishop-path
  [sq1 sq2]
  (into [] (map #(into [] (map core/coordinates %))
                (core/bishop-path 
                  (core/coordinates->coll sq1) 
                  (core/coordinates->coll sq2)))))

(deftest test-bishop-legal-moves
  (testing "bishop on a1"
    (is (= (human-bishop-legal-moves [0 0]) #{"b2" "c3" "d4" "e5" "f6" "g7" "h8"})))
  (testing "bishop on d4"
    (is (= (human-bishop-legal-moves [3 3]) #{"c3" "b2" "a1" "e5" "f6" "g7" "h8" "c5" "b6" "a7" "e3" "f2" "g1"}))))

(deftest test-bishop-path
  (testing "single move paths"
    (is (= (human-bishop-path "a1" "h8") [["h8"]]))
    (is (= (human-bishop-path "d2" "a5") [["a5"]])))
  (testing "two move paths"
    (is (= (human-bishop-path "a1" "e7") [["f6" "e7"]])))
  (testing "two two move paths"
    (is (= (human-bishop-path "c7" "e7") [["d6" "e7"] ["d8" "e7"]]))
    )
  )
