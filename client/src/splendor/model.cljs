(ns splendor.model
  (:require [reagent.core :as r]))

(def model
  (r/atom
   {:user-state {:id "00000000-0000-0000-0000-000000000001"}
    :current-action {}
    :pending-game []
    :active-game []}))
