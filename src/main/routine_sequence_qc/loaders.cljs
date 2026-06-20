(ns routine-sequence-qc.loaders
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [routine-sequence-qc.state :refer [db]]))

(defn load-sequencing-runs
  "Pull the sequencing runs from the server and add them to the app db."
  []
  (go
    (let [response (<! (http/get  "data/runs.json"))
          status (:status response)]
      (case status
        200 (let [response-body (:body response)
                  runs response-body]
              (swap! db assoc :runs runs))
        404 (js/console.log "Run data not found.")
        500 (js/console.log "Failed to download run data.")
        :else nil))))


(defn load-library-qc
  "Given a sequencing run ID, pull the library QC data for that run from the server and add it to the app db."
  [run-id]
  (go
    (let [response (<! (http/get (str "data/library-qc/" run-id "_library_qc.json")))]
      (cond (= 200 (:status response))
            (swap! db assoc-in [:library-qc run-id] (:body response))))))

(defn load-species-abundance
  "Given a sequencing run ID, pull the species abundance data for that run from the server and add it to the app db."
  [run-id]
  (go
    (let [response (<! (http/get (str "data/species-abundance/" run-id "_species_abundance.json")))]
      (cond (= 200 (:status response))
            (swap! db assoc-in [:species-abundance run-id] (:body response))))))