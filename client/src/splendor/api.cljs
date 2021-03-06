(ns splendor.api
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [splendor.model :refer [model]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]))

(def base-url "http://localhost:9000")
(def gm-url (str base-url "/gm"))

(defn init! []
  (let [user-id (get-in @model [:user-state :id])
        resp-ch (http/get (str gm-url "/active-game") {:headers {"authorization" user-id}})]
    (go
      (let [{:keys [body status]} (<! resp-ch)
            game (first (:games body))]
        (swap! model assoc :active-game game)))))

(defn start-game! [pending-game-id]
  (let [user-id (get-in @model [:user-state :id])
        resp-ch (http/post (str gm-url "/pending/" pending-game-id "/start") {:headers {"authorization" user-id}})]
    (go
      (let [{:keys [body status]} (<! resp-ch)]
        (swap! model assoc :active-game body)))))

(def tier-to-level {:tier1 1, :tier2 2, :tier3 3})

(defn send-action! [game-id {:keys [type card tokens tier]}]
  (let [action (merge
                {:actionType type}
                (when card
                  {:cardId (:id card)})
                (when (and tokens (nil? card) (nil? tier))
                  {:tokens tokens})
                (when tier
                  {:tier (tier-to-level tier)}))
        user-id (get-in @model [:user-state :id])
        resp-ch (http/put (str gm-url "/game/" game-id "/play")
                          {:json-params action
                           :headers {"authorization" user-id}})]
    (go
      (let [{:keys [body status]} (<! resp-ch)]
        (swap! model assoc :error nil)
        (if (= status 200)
          (swap! model merge {:current-action {}
                              :active-game body})
          (swap! model assoc :error body))))))
