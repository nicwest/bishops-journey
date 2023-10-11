(ns wtf.styx.chess-journey.core
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [goog.string :as gstring]
            [goog.string.format]
            [clojure.browser.event :as event]
            [re-frame.core :as rf]
            [clojure.string :refer [join]]))

(def files ["a" "b" "c" "d" "e" "f" "g" "h"])
(def files-speakable ["A" "B" "C" "D" "E" "F" "G" "H"])

(defn coordinates->coll
  [s]
  (let [x (first s)
        y (second s)]
    [(.indexOf files x) (- (int (char y)) 1)]))

(defn coordinates
  [[x y]]
  (gstring/format "%s%d" (nth files x) (+ 1 y)))

(defn coordinates-speakable
  [[x y]]
  (gstring/format "%s:%d" (nth files-speakable x) (+ 1 y)))

(defn dark? [[x y]] (even? (+ x y)))
(defn light? [sq] (not (dark? sq)))

(def dark-color "#2e2929")
(def light-color "#bdbab1")

(def black-color "#000000")
(def white-color "#FFFFFF")

(def red-color "red")
(def orange-color "orange")


(defn square
  [sq piece]
    [:div {:key (coordinates sq)
           :style {:background-color (if (dark? sq) dark-color light-color)
                   :color (if (dark? sq) white-color black-color)
                   :width "12.5%"
                   :height "12.5%"
                   :text-align "center"
                   :position "relative"
                   }}
     (when piece
       [:img {:src "/imgs/wB.svg" :style {:z-index 100
                                          :position "absolute"
                                          :top 0
                                          :left 0}}])])

(def squares (doall (for [ y (range 0 8) x (range 0 8)] [x (- 7 y)])))
(def dark-squares (filter dark? squares))
(def light-squares (filter light? squares))


(defn bishop-legal-moves
  [sq]
  (let [squares (if (dark? sq) dark-squares light-squares)
        [start-x start-y] sq]
    (into #{} (filter 
                (fn [[end-x end-y]]
                  (let [dx (abs (- end-x start-x))
                        dy (abs (- end-y start-y))]
                    (and (= dx dy) (> dx 0))))
                squares))))

(defn bishop-path
  "this is pretty badly implemented but 'it works' (tm)"
  [s1 s2]
  (let [moves (bishop-legal-moves s1)]
    (if (contains? moves s2)
      [[s2]]
      (let [moves (filter #(contains? (bishop-legal-moves %) s2) moves)]
        (map #(concat [%] [s2]) moves)))))

(defn lines
  []
  (let [moves @(rf/subscribe [:moves])]
    (when (> (count moves ) 1)
      (let [m (/ 400 8)
            off (/ m 2)
            [s1 s2]  (take 2 (rseq moves))
            coll->svg (fn [[x y]] [(+ (* x m) off) (- (- 400 (* y m)) off)])
            paths (map (fn [path]
                         (let [path (concat [s1] path)]
                           (map coll->svg path)))
                       (bishop-path s1 s2))]
        [:svg {:style {:position "absolute"
                       :width "400px"
                       :height "400px"
                       :z-index 10}}
         (for [path paths]
           (let [color (if (= path (first paths)) red-color orange-color)]
           [:<> {:key (str path)}
            (for [[x y] path]
              [:circle {:key (str x y) :r 3 :cx x :cy y :fill color}])
            [:path {:stroke-width 2
                    :stroke color
                    :fill "none"
                    :d (join " " 
                             (map (fn [sq]
                                    (gstring/format "%s %d,%d" 
                                                    (if (= (first path) sq) "M" "L")
                                                    (first sq)
                                                    (second sq))) path)
                             )}]]))]))))

(defn board
  []
  (let [sq @(rf/subscribe [:sq])]
    [:div {:id "board"
           :style {:width "400px"
                   :min-width "400px"
                   :height "400px"
                   :display "flex"
                   :flex-wrap "wrap"
                   :background black-color
                   :padding "20px"
                   :outline "4px solid"
                   :outline-color white-color
                   :outline-offset "-20px"
                   :position "relative" }}


     (lines)
     (for [xy squares]
       (square xy (= xy sq))) ]))

(defn deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))

(defn button
  [options text]
  [:button (deep-merge {:style {:color black-color
                                :background-color light-color
                                :border 0
                                :font-size "2rem"
                                :font-family "monospace"
                                ; :padding "1rem 2rem"
                                :display "block"
                                }}
                       options) text])

(defn tracker
  []
  (let [moves @(rf/subscribe [:moves])
        move-count (count moves)]
    [:div {:style {:color white-color
                   :font-family "monospace"
                   :padding "10px 7px"
                   :display "flex"
                   :flex-direction "column"
                   :gap "5px"
                   :max-width "400px"}}
     [:div {:style {:display "flex"
                    :gap "5px"}}
      (button {:on-click #(rf/dispatch [:move])} "Move")
      (button {:on-click #(rf/dispatch [:say-path true])} "Path")]
     [:div 
      (doall 
        (map-indexed 
          (fn [i move] 
            (let [s (gstring/format "%d. %s" (- move-count i) (coordinates move))]
              [:div {:key s} s]))
          (take 22 (reverse moves))))]]))

(defn listener
  []
  (.addEventListener js/window "keydown"
                     (fn [e]
                       (case (.-key e)
                         " " (do (rf/dispatch [:move]) false)
                         "p" (do (rf/dispatch [:say-path true]) false)
                         "q" (do (rf/dispatch [:say-path false]) false)
                         true))))


(rf/reg-event-db 
  :initialize-db
  (fn [_ _]
    {:sq [0 0] :moves [[0 0]]}))

(rf/reg-fx
  :speak
  (fn [text]
    (let [utterance (new js/SpeechSynthesisUtterance)]
      (set! (.-lang utterance) "en-gb")
      (set! (.-text utterance) text)
      (js/speechSynthesis.speak utterance))))

(rf/reg-event-fx
  :speak-debug
  (fn [_ [_ text]]
  {:speak text}))

(rf/reg-event-fx
  :move
  (fn [{:keys [db]} [_ _]]
    (let [current-sq (:sq db)
          moves (:moves db)
          legal (filter #(not= % current-sq) (if (dark? current-sq) dark-squares light-squares))
          new-sq (rand-nth legal)]
      {:db (assoc db :sq new-sq :moves (conj moves new-sq))
       :speak (coordinates-speakable new-sq)})))

(rf/reg-event-fx
  :say-path
  (fn [{:keys [db]} [_ first?]]
    (if (< (count (:moves db)) 2)
      {}
      (let [[current prev] (->> (:moves db)
                               (reverse)
                               (take 2))
            paths (bishop-path prev current)
            alternate? (> (count paths) 1)
            path (if 
                   (or first? (not alternate?))
                   (first paths)
                   (second paths))
            moves (map coordinates-speakable path)
            moves (if alternate? (concat moves [(if first? "path 1" "path 2")]) moves)]
        {:speak (join " ... " moves)}))))

(rf/reg-sub
  :sq
  (fn [db _]
    (get-in db [:sq])))

(rf/reg-sub
  :moves
  (fn [db _]
    (get-in db [:moves])))

(defn description
  []
  [:div  {:style {:color white-color
                  :font-family "monospace"
                  :padding "10px 7px"
                  :max-width "500px"
                  }}
   [:p "this is a shitty tool to help your chess visualisation"]
   [:ol
    [:li "look at the board (the bishop always starts on a1)"]
    [:li "shut your eyes"]
    [:li "either press the " [:strong "Move"]  " button or the " [:strong "space bar"]]
    [:li "you will hear a move"]
    [:li "move the piece in your head using only legal bishop moves"]
    [:li "if you want to check your answer you can:"
     [:ul
      [:li "open your eyes and look at the lines on the board"]
      [:li "press the " [:strong "Path"] " button"]
      [:li "press the " [:strong "P"] " or " [:strong "Q"] " keys. (each key will give you a different path if there a multiple paths)"] ]] ]])


(defn app
  []
  [:<>
   [:div {:style {:display "flex"
                  :gap "5px"
                  :flex-wrap "wrap"}}
    (board)
    (tracker)
    (description)]])

(defn ^:dev/after-load start
  []
  (rd/render [app]
             (.getElementById js/document "app"))
  )

(defn ^:export init
  []
  (rf/dispatch [:initialize-db])
  (listener)
  (start))
