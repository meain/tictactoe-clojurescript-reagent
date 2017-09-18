(ns ttt.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

; (println "Session started!")

;; 0 - blank
;; 1 - player one has played ( user )
;; 2 - player two has played ( computer )
(defn make-board "Creates a new board. n denotes the size." [n]
  (vec (repeat n (vec (repeat n 0)))))

(def board-size 3)
(defonce app-state 
  (atom {:text ":meain"
         :board (make-board board-size)
         ;; none win lose draw
         :win "none"}))

(defn print-state []
  (println (:board @app-state)))
; (println (range(count (:board @app-state))))

(defn check-state []
  (let [board (:board @app-state)
        remaining (for [i (range board-size)
                        j (range board-size)
                        :when (= (get-in board [i j]) 0)]
                    [i j])]
        (if (= (count remaining) 0)
          (swap! app-state assoc :win "draw"))
        ))

(defn computer-move []
  ;; choose a random unplayed block
  (let [board (:board @app-state)
        remaining (for [i (range board-size)
                        j (range board-size)
                        :when (= (get-in board [i j]) 0)]
                    [i j])
        move (rand-nth remaining)
        path (into [:board] move)]
    (swap! app-state assoc-in path 2)
    )
  )

(defn block [color i j]
  [:div {:style {:background-color color
                 :width "100px"
                 :height "100px"
                 :border "5px solid #fff"}
         :on-click (fn [e]
                     (if (= 0 (get-in @app-state [:board i j]))
                     ; (swap! app-state assoc-in [:board i j] (inc (get-in @app-state [:board i j])))))}])
                     ((swap! app-state assoc-in [:board i j] 1)
                     (check-state)
                     (if (not= (:win @app-state) "draw")
                       (computer-move)
                       (check-state))
                       )))}])

(defn blank [i j]
  (block "#f5f5f5" i j))
(defn cross [i j]
  (block "#FF7043" i j))
(defn circle [i j]
  (block "#FFEE58" i j))

(defn render-board []
[:div {:style {:display "flex" :flex-wrap "wrap"}}
    (doall(for [i (range board-size)
          j (range board-size)]
      (case (get-in @app-state [:board i j])
        0 ^{:key (str i "-" j)} [blank i j]
        1 ^{:key (str i "-" j)} [cross i j]
        2 ^{:key (str i "-" j)} [circle i j]
        )))
    ])

(defn app []
  [:div {:style {:text-align "center"}}
   [:div
    [:h1 {:style {:display "block" :float "left"}} (:text @app-state)]
    [:h1 {:style{:background-color "#f5f5f5"
                 :display "block"
                 :float "right"}} "tic-tac-toe"]]
   [:div.clearfix {:style {:clear "both"}}]
   [:h3 {:style {:width "100%" :text-align "center"}} "Let us play some "
    [:code {:style {:font-family "cursive"}} "tic-tac-toe"] " now"]
   [:center [:div {:style {:font-size "20px"}} (:win @app-state)]]
   [:div.play-area {:style {:width (str (* 110 board-size) "px")
                            :height (str (* 110 board-size) "px")
                            :background-color "#ded"
                            :cursor "pointer"
                            :display "inline-block"}}
    [render-board]
    ]
   [:center
    [:button {:on-click (fn [e] 
                          (swap! app-state assoc :board (make-board board-size))
                          (swap! app-state assoc :win "none")
                          )
              :style {:font-size "30px"
                      :font-family "monaco, monospace"
                      :margin-top "20px"}} "New Game"]]])

(reagent/render-component [app]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (prn (:board @app-state))
  )
