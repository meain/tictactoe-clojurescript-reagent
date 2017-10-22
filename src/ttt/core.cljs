(ns ttt.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

;; 0 - blank
;; 1 - player one has played ( user )
;; 2 - player two has played ( computer )
(defn make-board "Creates a new board. n denotes the size." [n]
  (vec (repeat n (vec (repeat n 0)))))

(def board-size 3)  ;; probably should have defined inside app-state
(defonce app-state
  (atom {:text ":game"
         :board (make-board board-size)
         ;; none win lose draw
         :win "none"}))

(defn check-win "Check for win and lose conditions" [user computer]
  (if (or (some #(= board-size %) (for [freq (frequencies (for [el user] (first el)))] (second freq)))
          (some #(= board-size %) (for [freq (frequencies (for [el user] (second el)))] (second freq)))
          (= board-size (get-in (frequencies (for [el user] (= (first el) (second el)))) [true]))
          (= board-size (get-in (frequencies (for [el user] (= (first el) (- (- board-size (second el)) 1)))) [true])))
    (swap! app-state assoc :win "win"))
  (if (or (some #(= board-size %) (for [freq (frequencies (for [el computer] (first el)))] (second freq)))
          (some #(= board-size %) (for [freq (frequencies (for [el computer] (second el)))] (second freq)))
          (= board-size (get-in (frequencies (for [el computer] (= (first el) (second el)))) [true]))
          (= board-size (get-in (frequencies (for [el computer] (= (first el) (- (- board-size (second el)) 1)))) [true])))
    (swap! app-state assoc :win "lose"))
  )

(defn check-state []
  (let [board (:board @app-state)
        remaining (for [i (range board-size)
                        j (range board-size)
                        :when (= (get-in board [i j]) 0)]
                    [i j])
        user (for [i (range board-size)
                   j (range board-size)
                   :when (= (get-in board [i j]) 1)]
               [i j])
        computer (for [i (range board-size)
                       j (range board-size)
                       :when (= (get-in board [i j]) 2)]
                   [i j])]
    (if (= (count remaining) 0)
      (swap! app-state assoc :win "draw"))
    (check-win user computer)
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
  (check-state)
  )

(defn block [color i j]
  [:div {:style {:background-color color
                 :width "100px"
                 :height "100px"
                 :border "5px solid #fff"}
         :on-click (fn [e]
                     (if (and (= 0 (get-in @app-state [:board i j])) (= (:win @app-state) "none"))
                       ((swap! app-state assoc-in [:board i j] 1)
                        (check-state)
                        (if (= (:win @app-state) "none")
                          (computer-move)
                          ))))}])

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
