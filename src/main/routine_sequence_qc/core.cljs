(ns routine-sequence-qc.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.string :as str]
            [clojure.set]
            [reagent.core :as r] 
            [reagent.dom.client :as rdomc]
            [reagent.dom.server]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [ag-grid-community :refer [ModuleRegistry AllCommunityModule]]
            [ag-grid-react :as ag-grid]
            [cljs.pprint :refer [pprint]]
            [routine-sequence-qc.state :as state :refer [db]]
            [routine-sequence-qc.loaders :as loaders]
            [routine-sequence-qc.components :as components]
            [routine-sequence-qc.grid :as grid]))


(def app-version "v3.4.1")


(defn run-id->date
  "Extract the date from a run ID as an ISO-8601 string YYYY-MM-DD"
  [run-id]
  (let [date-part (str/split run-id "_" 1)]
    (cond (= 6 (count date-part))
          date-part
          (= 6 (count date-part))
          date-part)
    )
  )

(comment
  (run-id->date "260617_VH00278_407_AAJCFF3M5")
  (run-id->date "20260612_SH01373_0001_ASC2219288")
  )










(defn illumina
  "Component for displaying all illumina sequencing run QC data."
  []
  [:div {:style {:display "grid"
                 :grid-template-columns "3fr 13fr"
                 :grid-template-rows "repeat(2, 1fr)"
                 :gap "4px"
                 :height "800px"}}
   [:div {:style {:display "grid"
                  :grid-column "1"
                  :grid-row "1 / 3"
                  :overflow "auto"
                  :resize "horizontal"}}
    [components/illumina-runs-table]]
   [:div {:style {:display "grid"
                  :grid-column "2"
                  :grid-row "1"
                  :gap "4px"
                  :overflow "auto"
                  :resize "horizontal"}}
    [components/library-sequence-qc-table]]
   [:div {:style {:display "grid"
                  :grid-column "2"
                  :grid-row "2"
                  :overflow "auto"
                  :resize "horizontal"}}
    [components/library-species-abundance-table]]])


(defn app
  "Root app component."
  []
  [:div {:style {:display "grid"
                 :grid-template-columns "1fr"
                 :grid-gap "4px 4px"
                 :height "100%"}}
   [components/header app-version]
   [illumina]])

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