(ns splendor.view
  (:require [reagent.core :as r]
            [cljs.core.async :refer [put!]]))

(def concatv (comp vec concat))
(defn call-handler [f & args]
  (when f
    (apply f args))
  ;; Make sure not to trigger legacy "prevent default"
  true)

(defn render-cost [{:keys [on-click]} cost]
  (concatv [:div.cost]
           (map (fn [[color qty]]
                  [:div.cost-component {:class (str "cost-" (name color))
                                        :key (name color)
                                        :on-click #(call-handler on-click color)}
                   qty])
                cost)))

(defn render-card [i {:keys [points bonus cost cardType]}]
  [:div.card {:key i
              :class (str "card-bonus-" bonus)}
   [:div.title-bar
    [:div.points points]
    [:div.title "Tier " (last cardType)]]
   (render-cost {} cost)])

(defn render-deck [cards]
  (concatv [:div.cards]
           (map-indexed render-card cards)))

(defn render-noble [i {:keys [points cost]}]
  [:div.card {:key i}
   [:div.title-bar
    [:div.title "Noble"]
    [:div.points points]]
   (render-cost {} cost)])

(defn render-tokens [events tokens]
  [:div.tokens "Tokens"
   (render-cost events tokens)])

(defn token-action [tokens]
  (render-cost {} (remove (comp zero? second) tokens)))

(defn render-action [{:keys [on-reset]} {:keys [title type flash] :as action}]
  [:div.action
   (when flash
     [:div.action-flash flash])
   [:button {:on-click #(call-handler on-reset)}
    "Reset"]
   [:div.action-title
    title]
   (condp = type
     :tokens (token-action (:tokens action))
     [:div "No Action Selected"])])

;;TODO: Players should show face up cards they have

(defn game-view [game-cursor action-cursor event-chan]
  (let [{:keys [tokens decks nobles players]} @game-cursor

        tokens-selected-handler (fn [color]
                                  (put! event-chan {:type :select-token
                                                    :color color}))

        reset-handler (fn [] (put! event-chan {:type :reset-action}))]
    [:div
     [:div.game
      (render-tokens
       {:on-click tokens-selected-handler}
       tokens)
      (concatv [:div.cards
                (map-indexed render-noble nobles)])
      [:div.action-panel
       [:div.decks
        (render-deck (:tier1 decks))
        (render-deck (:tier2 decks))
        (render-deck (:tier3 decks))]
       [:div.actions
        (render-action
         {:on-reset reset-handler}
         @action-cursor)]]]]))
