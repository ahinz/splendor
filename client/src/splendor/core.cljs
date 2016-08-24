(ns splendor.core
  (:require [reagent.core :as r]))

(enable-console-print!)

(defn app []
  [:div "Hello World"])

(defn run []
  (r/render [app]
            (js/document.getElementById "app")))

(run)
