(ns routine-sequence-qc.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.set]
            [reagent.core :as r] 
            [reagent.dom :as rdom]
            [reagent.dom.server]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [ag-grid-react :as ag-grid]
            [ag-charts-react :as ag-chart]
            [cljs.pprint :refer [pprint]]))

(defonce db (r/atom {}))

(def app-version "v3.0.0")

(def url-prefix "")


(defn mean [coll]
  "Calculate the artithmetic mean of a collection of numbers"
  (/ (reduce + coll) (count coll)))


(defn round-number
  ""
  [f]
  (/ (.round js/Math (* 100 f)) 100))


(defn in? 
  "true if coll contains elem"
  [coll elem]  
  (some #(= elem %) coll))


(defn load-sequencing-runs []
  ""
  (go (let [response (<! (http/get (str url-prefix "data/runs.json")))]
        (swap! db assoc-in [:runs] (:body response)))))

(defn debug-view []
  (let [current-debug (:debug-view @db)
        toggle-debug #(swap! db assoc :debug-view (not current-debug))]
    [:div
     [:button {:on-click toggle-debug} "Toggle Debug View"]
     [:div.debug {:style {:background-color "#CDCDCD" :display (if (:debug-view @db) "block" "none")}}
      [:pre [:code {:style {:font-family "monospace" }}
             (with-out-str (pprint (select-keys @db [:debug-view :selected-run-id])))]]]]))


(defn header []
  [:header {:style {:display "grid"
                    :grid-template-columns "repeat(2, 1fr)"
                    :align-items "center"}}
   [:div {:style {:display "grid"
                  :grid-template-columns "repeat(2, 1fr)"
                  :align-items "center"}}
    [:h1 {:style {:font-family "Arial" :color "#004a87" :margin "0px"}} "Routine Sequence QC"][:p {:style {:font-family "Arial" :color "grey" :justify-self "start"}} app-version]]
   [:div {:style {:display "grid" :align-self "center" :justify-self "end"}}
    [:img {:src (str url-prefix "/images/bccdc_logo.svg") :height "48px"}]]])


(defn get-selected-rows [e]
  (map #(js->clj (.-data %) :keywordize-keys true)
       (-> e
           .-api
           .getSelectedNodes)))


(defn run-selected [e]
  ""
  (let [previously-selected-run-ids (:selected-run-ids @db)
        currently-selected-run-id (:run_id (first (get-selected-rows e)))]
    (do
      (swap! db assoc-in [:selected-run-id] currently-selected-run-id))))


(defn library-sequence-qc-selected [e]
  ""
  (do
    (swap! db assoc-in [:selected-libraries] (get-selected-rows e))
    ))


(defn library-species-abundance-selected [e]
  ""
  (do
    (swap! db assoc-in [:selected-libraries] (get-selected-rows e))))



(defn runs-table []
  (let [row-data (map #(assoc {} :run_id (:run_id %)
                                 :num_libraries (:num_libraries %))
                      (:runs @db))]
    [:div {:class "ag-theme-balham"
           :style {}}
     [:> ag-grid/AgGridReact
      {:rowData row-data
       :pagination false
       :floatingFilter true
       :rowSelection "single"
       :enableCellTextSelection true
       :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
       :onSelectionChanged run-selected}
      [:> ag-grid/AgGridColumn {:field "run_id" :headerName "Run ID" :minWidth 265 :resizable true :filter "agTextColumnFilter" :sortable true :checkboxSelection true :sort "desc"}]]]))


(defn cell-renderer-hyperlink-button [text params]
  (str "<button><a href=\"" (.-value params) "\" style=\"color: inherit; text-decoration: inherit\" target=\"_blank\">" text "</a></button>"))

(defn cell-renderer-hyperlink-multiqc [params]
  (cell-renderer-hyperlink-button "MultiQC" params))

(defn library-sequence-qc-table []
  (let [join-by-comma #(clojure.string/join ", " %)
        selected-library-qc-summary (get (:library-sequence-qc-summaries @db) (:selected-run-id @db))
        row-data selected-library-qc-summary]
    [:div {:class "ag-theme-balham"
           :style {:height 256}}
     [:> ag-grid/AgGridReact
      {:rowData row-data
       :pagination false
       :rowSelection "multiple"
       :floatingFilter true
       :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
       :onSelectionChanged library-sequence-qc-selected
       }
      [:> ag-grid/AgGridColumn {:field "library_id" :headerName "Library ID" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter" :pinned "left" :checkboxSelection true :headerCheckboxSelectionFilteredOnly true}]
      [:> ag-grid/AgGridColumn {:field "project_id" :headerName "Project ID" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "inferred_species" :headerName "Inferred Species" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "total_bases" :maxWidth 140 :headerName "Total Bases" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]
      [:> ag-grid/AgGridColumn {:field "percent_bases_above_q30" :maxWidth 140 :headerName "Bases Above Q30 (%)" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]
      [:> ag-grid/AgGridColumn {:field "estimated_depth_coverage" :maxWidth 140 :headerName "Est. Depth Coverage" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]
      [:> ag-grid/AgGridColumn {:field "multiqc_link" :headerName "MultiQC" :maxWidth 72 :cellRenderer cell-renderer-hyperlink-multiqc}]
      ]]
    ))


(defn library-species-abundance-table []
  (let [join-by-comma #(clojure.string/join ", " %)
        selected-species-abundance (get (:library-species-abundance @db) (:selected-run-id @db))
        row-data selected-species-abundance]
    [:div {:class "ag-theme-balham"
           :style {:height 256}}
     [:> ag-grid/AgGridReact
      {:rowData row-data
       :pagination false
       :rowSelection "multiple"
       :floatingFilter true
       :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
       :onSelectionChanged library-species-abundance-selected
       }
      [:> ag-grid/AgGridColumn {:field "library_id" :headerName "Library ID" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter" :pinned "left" :checkboxSelection true :headerCheckboxSelectionFilteredOnly true}]
      [:> ag-grid/AgGridColumn {:field "project_id" :headerName "Project ID" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      [:> ag-grid/AgGridColumn {:headerName "Most Abundant Species"}
       [:> ag-grid/AgGridColumn {:field "abundance_1_name" :maxWidth 140 :headerName "Species Name" :sortable true :resizable true :filter "agTextColumnFilter"}]
       [:> ag-grid/AgGridColumn {:field "abundance_1_fraction_total_reads" :maxWidth 120 :headerName "Abundance (%)" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]]
      [:> ag-grid/AgGridColumn {:headerName "2nd Most Abundant Species"}
      [:> ag-grid/AgGridColumn {:field "abundance_2_name" :maxWidth 140 :headerName "Species Name" :sortable true :resizable true :filter "agTextColumnFilter"}]
       [:> ag-grid/AgGridColumn {:field "abundance_2_fraction_total_reads" :maxWidth 120 :headerName "Abundance (%)" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]]
      [:> ag-grid/AgGridColumn {:headerName "3rd Most Abundant Species"}
       [:> ag-grid/AgGridColumn {:field "abundance_3_name" :maxWidth 140 :headerName "Species Name" :sortable true :resizable true :filter "agTextColumnFilter"}]
       [:> ag-grid/AgGridColumn {:field "abundance_3_fraction_total_reads" :maxWidth 120 :headerName "Abundance (%)" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]]
      [:> ag-grid/AgGridColumn {:headerName "4th Most Abundant Species"}
       [:> ag-grid/AgGridColumn {:field "abundance_4_name" :maxWidth 140 :headerName "Species Name" :sortable true :resizable true :filter "agTextColumnFilter"}]
       [:> ag-grid/AgGridColumn {:field "abundance_4_fraction_total_reads" :maxWidth 120 :headerName "Abundance (%)" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]]
      [:> ag-grid/AgGridColumn {:headerName "5th Most Abundant Species"}
       [:> ag-grid/AgGridColumn {:field "abundance_5_name" :maxWidth 140 :headerName "Species Name" :sortable true :resizable true :filter "agTextColumnFilter"}]
       [:> ag-grid/AgGridColumn {:field "abundance_5_fraction_total_reads" :maxWidth 120 :headerName "Abundance (%)" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]]
      ]]
    ))



(defn root []
  [:div {:style {:display "grid"
                 :grid-template-columns "1fr"
                 :grid-gap "4px 4px"}}

   [header]

   [:div {:style {:display "grid"
                  :grid-template-columns "1fr 5fr"
                  :grid-template-rows "repeat(2, 1fr)"
                  :gap "4px"}}
    [:div {:style {:display "grid"
                   :grid-column "1"
                   :grid-row "1 / 3"}}
     [runs-table]]
    [:div {:style {:display "grid"
                   :grid-column "2"
                   :grid-row "1"
                   :gap "4px"}}
     [library-sequence-qc-table]]
    [:div {:style {:display "grid"
                   :grid-column "2"
                   :grid-row "2"}}
     [library-species-abundance-table]]]   
   [debug-view]
   ])

(defn main []
  (load-sequencing-runs)
  (rdom/render [root] (js/document.getElementById "app")))

(set! (.-onload js/window) main)

(comment

  )
