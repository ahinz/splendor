(ns splendor.core
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [cljs.core.async :refer [chan <!]]
            [reagent.core :as r]
            [splendor.api :as api]
            [splendor.view :as view]
            [splendor.model :as model :refer [model]]))

(defn app [model event-chan]
  [:div
   [:div {:on-click #(api/start-game! "00000000-0000-0000-0000-000000000003")} "Start Game"]
   [:div {:on-click #(api/init!)} "Check for games"]
   [:div
    [view/game-view
     (r/cursor model [:active-game])
     (r/cursor model [:current-action])
     event-chan]]])

(defn flash-message [msg]
  (println msg)
  (swap! model assoc-in [:current-action :flash] msg))

(defn transfer-token
  "Transfer amt of color from the bank to the user

  This method allows negative balances for both bank and user"
  [color amt]
  (swap! model update-in [:current-action :tokens color] (fnil + 0) amt)
  (swap! model update-in [:active-game :tokens color] (fnil - 0) amt))

(defn reset-action []
  (let [tokens (get-in @model [:current-action :tokens])]
    (doseq [[color amt] tokens]
      (transfer-token color (- amt)))))

(defn select-token [{:keys [color]}]
  (when (not= :tokens (get-in @model [:current-action :type]))
    (swap! model assoc :current-action {:type :tokens
                                        :tokens {}}))

  (flash-message nil)

  (let [action (:current-action @model)
        n-tokens-in-bank (get-in @model [:active-game :tokens color])
        other-doubles? (not
                        (empty?
                         (filter (fn [[a-color v]]
                                   (and (not= a-color) (= v 2)))
                                 (get-in @model [:active-game :tokens]))))
        colors (set (map first (filter (comp pos? second) (get action :tokens))))
        n-tokens-in-action (reduce + 0 (vals (get action :tokens)))
        n-tokens-in-action-in-color (get-in action [:tokens color] 0)]
    (cond
      ;; Limited to two tokens
      (>= n-tokens-in-action-in-color 2)
      (flash-message "You already have two of those tokens")

      ;; Can't take two tokens if there would be fewer than two left
      (and (= n-tokens-in-action-in-color 1)
           (< n-tokens-in-bank 3))
      (flash-message "Not enough tokens to take two")

      ;; Limit of three total tokens
      (>= n-tokens-in-action 3)
      (flash-message "You can only select three tokens")

      ;; Can't double up if other tokens are in play
      false
      (flash-message "You can't take two of the same color when selecting three tokens")

      (zero? n-tokens-in-bank)
      (flash-message (str "Not enough " (name color) " tokens left in the bank"))

      (= color :gold)
      (flash-message (str "Can't pick up gold tokens"))

      ;; You followed all the rules!
      :else
      (transfer-token color 1))))

(def event-map {:select-token select-token
                :reset-action reset-action})

(defn listen-for-events! [event-map]
  (let [event-chan (chan 3000)]
    (go-loop []
      (when-let [{event-type :type :as event} (<! event-chan)]
        (if-let [event-f (get event-map event-type)]
          (event-f event)
          (throw (str "Invalid event: " event-type)))
        (recur)))
    event-chan))

(defn run []
  (enable-console-print!)
  (let [event-chan (listen-for-events! event-map)]
    (api/init!)
    (r/render [app model/model event-chan]
              (js/document.getElementById "app"))))

(run)
