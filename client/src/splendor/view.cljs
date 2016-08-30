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

(defn render-card [{:keys [on-click]} i {:keys [points bonus cost cardType] :as card}]
  [:div.card {:key i
              :class (str "card-bonus-" bonus)
              :on-click #(call-handler on-click card)}
   [:div.title-bar
    [:div.points points]
    [:div.title "Tier " (last cardType)]]
   (render-cost {} cost)])

(defn render-deck
  [{:keys [on-card-click on-deck-click]} tier cards selected-card?]
  [:div
   (concatv [:div.cards
             [:div.deck.card
              {:on-click #(call-handler on-deck-click tier)}
              "Level " (last (name tier)) " Deck"]] ;; ugly string handling
            (map-indexed (fn [i card]
                           (if (selected-card? card)
                             [:div.empty-card]
                             (render-card {:on-click on-card-click} i card)))
                         cards))])

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

(defn face-up-card-action [{:keys [tokens card]}]
  [:div
   (token-action tokens)
   (render-card {} 0 card)])

(defn face-down-card-action [{:keys [tokens tier]}]
  [:div
   (token-action tokens)
   [:div.card
    "Level " (last (name tier)) " card"]])

(defn render-action
  [{:keys [on-reset on-play]} {:keys [title type flash] :as action}]
  [:div.action
   (when flash
     [:div.action-flash flash])
   [:button {:on-click #(call-handler on-reset)}
    "Reset"]
   [:button {:on-click #(call-handler on-play)}
    "Play"]
   [:div.action-title
    title]
   (condp = type
     :tokens (token-action (:tokens action))
     :face-up-card (face-up-card-action action)
     :face-down-card (face-down-card-action action)
     [:div "No Action Selected"])])

;;TODO: Players should show face up cards they have

(defn game-view [error-cursor user-cursor game-cursor action-cursor event-chan]
  (let [{:keys [tokens decks nobles players currentPlayerId]} @game-cursor

        our-turn? (= currentPlayerId (:id @user-cursor))

        card-click-handler (fn [card]
                             (put! event-chan {:type :select-face-up-card
                                               :card card}))

        deck-click-handler (fn [tier]
                             (put! event-chan {:type :select-face-down-card
                                               :tier tier}))

        selected-card? (set (when (= :face-up-card (:type @action-cursor))
                              [(:card @action-cursor)]))

        tokens-selected-handler (fn [color]
                                  (put! event-chan {:type :select-token
                                                    :color color}))

        play-handler (fn [] (put! event-chan {:type :play}))

        reset-handler (fn [] (put! event-chan {:type :reset-action}))]
    [:div
     (when-let [error @error-cursor]
       [:div.error error])
     [:div.game
      (render-tokens
       {:on-click tokens-selected-handler}
       tokens)
      (concatv [:div.cards
                (map-indexed render-noble nobles)])
      [:div.action-panel
       [:div.decks
        (render-deck
         {:on-card-click card-click-handler
          :on-deck-click deck-click-handler}
         :tier1
         (:tier1 decks)
         selected-card?)
        (render-deck
         {:on-card-click card-click-handler
          :on-deck-click deck-click-handler}
         :tier2
         (:tier2 decks)
         selected-card?)
        (render-deck
         {:on-card-click card-click-handler
          :on-deck-click deck-click-handler}
         :tier3
         (:tier3 decks)
         selected-card?)]
       [:div.actions
        (if our-turn?
          (render-action
           {:on-reset reset-handler
            :on-play play-handler}
           @action-cursor)
          [:div "Waiting for player " currentPlayerId])]]]]))
