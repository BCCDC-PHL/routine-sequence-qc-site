(ns routine-sequence-qc.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.set]
            [reagent.dom.client :as rdomc]
            [ag-grid-community :refer [ModuleRegistry AllCommunityModule]]
            [routine-sequence-qc.loaders :as loaders]
            [routine-sequence-qc.components :as components]))

(def app-version "v3.4.1")

(defn app
  "Root app component."
  []
  [:div {:style {:display "grid"
                 :grid-template-columns "1fr"
                 :grid-gap "4px 4px"
                 :height "100%"}}
   [components/header app-version]
   [components/illumina]])

(defonce root
  (rdomc/create-root (.getElementById js/document "app")))

(defn render
  "Render the app into the root app div."
  []
  (rdomc/render root [app]))

(defn ^:dev/after-load re-render
  "Hot-reload hook called by shadow-cljs after code changes.
  Re-renders from root so that new component definitions take effect.
  State is preserved because it lives in `defonce` app state db atom."
  []
  (render))

(defn ^:export init!
  "Exported entry point called by shadow-cljs on page load."
  []
  (.registerModules ModuleRegistry #js [AllCommunityModule])
  (loaders/load-sequencing-runs)
  (render))
