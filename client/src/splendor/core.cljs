(ns splendor.core
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [cljs.core.async :refer [chan <!]]
            [reagent.core :as r]
            [splendor.api :as api]
            [splendor.view :as view]
            [splendor.model :as model :refer [model]]))

(defn app [model event-chan]
  [:div
   (when (nil? (:gameId (:active-game @model)))
     [:button {:on-click #(api/start-game! "00000000-0000-0000-0000-000000000003")} "Start Game"])
   [:div
    [view/game-view
     (r/cursor model [:error])
     (r/cursor model [:user-state])
     (r/cursor model [:active-game])
     (r/cursor model [:current-action])
     event-chan]]])

(defn flash-message [msg]
  (when msg
    (println msg))
  (swap! model assoc-in [:current-action :flash] msg))

(defn transfer-token
  "Transfer amt of color from the bank to the user

  This method allows negative balances for both bank and user"
  [color amt]
  (swap! model update-in [:current-action :tokens color] (fnil + 0) amt)
  (swap! model update-in [:active-game :tokens color] (fnil - 0) amt))

(defn reset-action [action]
  (let [tokens (get-in @model [:current-action :tokens])]
    (doseq [[color amt] tokens]
      (transfer-token color (- amt)))
    (swap! model assoc :current-action {:type action})))

(defn transfer-gold-if-able []
  (when (pos? (get-in @model [:active-game :tokens :gold]))
    (transfer-token :gold 1)))

(defn select-face-up-card [{:keys [card]}]
  (reset-action :face-up-card)
  (swap! model assoc-in [:current-action :card] card)
  (transfer-gold-if-able))

(defn select-face-down-card [{:keys [tier]}]
  (reset-action :face-down-card)
  (swap! model assoc-in [:current-action :tier] tier)
  (transfer-gold-if-able))

(defn select-token [{:keys [color]}]
  (when (not= :tokens (get-in @model [:current-action :type]))
    (reset-action :tokens))

  (flash-message nil)

  (let [action (:current-action @model)
        n-tokens-in-bank (get-in @model [:active-game :tokens color])
        doubles? (not
                  (empty?
                   (filter (fn [[a-color v]]
                             (= v 2))
                           (get-in @model [:active-game :tokens]))))
        contains-color? (set (map first (filter (comp pos? second) (get action :tokens))))
        n-tokens-in-action (reduce + 0 (vals (get action :tokens)))
        n-tokens-in-action-in-color (get-in action [:tokens color] 0)]
    (cond
      (= n-tokens-in-action 3)
      (flash-message "You can't pick up any more tokens")

      (and (= n-tokens-in-action 2) doubles?)
      (flash-message "You can't pick up two colors with a double selected")

      (and (= n-tokens-in-action 2) (contains-color? color))
      (flash-message "You can't pick up any more of that color")

      (zero? n-tokens-in-bank)
      (flash-message (str "Not enough " (name color) " tokens left in the bank"))

      (= color :gold)
      (flash-message (str "Can't pick up gold tokens"))

      ;; You followed all the rules!
      :else
      (transfer-token color 1))))

(defn execute-action []
  (let [action (get @model :current-action)
        game-id (get-in @model [:active-game :gameId])]
    (when (and game-id action)
      (api/send-action! game-id action))))

(def event-map {:select-token select-token
                :reset-action reset-action
                :select-face-up-card select-face-up-card
                :select-face-down-card select-face-down-card
                :play execute-action})

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
