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
        (println (clj->js body))))))
