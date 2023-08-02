(ns routine-sequence-qc.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.set]
            [reagent.core :as r] 
            [reagent.dom :as rdom]
            [reagent.dom.server]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [semantic-ui-react :as semantic-ui]
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
  (go
    (let [response (<! (http/get (str url-prefix "data/runs.json")))]
      (swap! db assoc-in [:runs] (:body response)))))


(defn load-library-qc [run-id]
  ""
  (go
    (let [response (<! (http/get (str url-prefix "data/library-qc/" run-id "_library_qc.json")))]
      (if (= 200 (:status response))
        (swap! db assoc-in [:library-qc run-id] (:body response))))))


(defn load-species-abundance [run-id]
  ""
  (go
    (let [response (<! (http/get (str url-prefix "data/species-abundance/" run-id "_species_abundance.json")))]
      (if (= 200 (:status response))
        (swap! db assoc-in [:species-abundance run-id] (:body response))))))


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
                    :align-items "center"
                    :height "48px"}}
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
      (load-library-qc currently-selected-run-id)
      (load-species-abundance currently-selected-run-id)
      (swap! db assoc-in [:selected-run-id] currently-selected-run-id))))


(defn cell-renderer-hyperlink-button [text params]
  (str "<button><a href=\"" (.-value params) "\" style=\"color: inherit; text-decoration: inherit\" target=\"_blank\">" text "</a></button>"))

(defn cell-renderer-hyperlink-multiqc [params]
  (cell-renderer-hyperlink-button "MultiQC" params))

(defn cell-renderer-hyperlink-fastqc-r1 [params]
  (cell-renderer-hyperlink-button "FastQC R1" params))

(defn cell-renderer-hyperlink-fastqc-r2 [params]
  (cell-renderer-hyperlink-button "FastQC R2" params))


(defn illumina-runs-table []
  (let [runs (:runs @db)
        add-multiqc-link #(assoc % :multiqc_link (str url-prefix "data/multiqc/" (:run_id %) "_multiqc.html"))
        row-data (->> runs
                      (map add-multiqc-link))]
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
      [:> ag-grid/AgGridColumn {:field "run_id" :headerName "Run ID" :minWidth 200 :resizable true :filter "agTextColumnFilter" :sortable true :checkboxSelection true :sort "desc"}]
      [:> ag-grid/AgGridColumn {:field "multiqc_link" :headerName "MultiQC" :maxWidth 128 :cellRenderer cell-renderer-hyperlink-multiqc}]]]))



(defn library-sequence-qc-table []
  (let [currently-selected-run-id (:selected-run-id @db)
        selected-run-library-qc (get-in @db [:library-qc currently-selected-run-id])
        add-fastqc-r1-link #(assoc % :fastqc_r1_link (str url-prefix "data/fastqc/" currently-selected-run-id "/" (:library_id %) "_R1_fastqc.html"))
        add-fastqc-r2-link #(assoc % :fastqc_r2_link (str url-prefix "data/fastqc/" currently-selected-run-id "/" (:library_id %) "_R2_fastqc.html"))
        row-data (->> selected-run-library-qc
                      (map (fn [x] (update x :inferred_species_percent #(if % (.toFixed % 2) 0.00))))
                      (map (fn [x] (update x :total_bases #(if % (.toFixed (/ % 1000000) 3)))))
                      (map (fn [x] (update x :inferred_species_estimated_depth #(if % (.toFixed % 2) nil))))
                      (map (fn [x] (update x :percent_bases_above_q30 #(if % (.toFixed % 2)))))
                      (map add-fastqc-r1-link)
                      (map add-fastqc-r2-link))]
    [:div {:class "ag-theme-balham"
           :style {}}
     [:> ag-grid/AgGridReact
      {:rowData row-data
       :pagination false
       :enableCellTextSelection true
       :floatingFilter true
       :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
       :onSelectionChanged #()
       }
      [:> ag-grid/AgGridColumn {:field "library_id" :headerName "Library ID" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter" :pinned "left" :checkboxSelection false :headerCheckboxSelectionFilteredOnly true}]
      [:> ag-grid/AgGridColumn {:field "project_id" :headerName "Project ID" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "inferred_species_name" :headerName "Inferred Species" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      [:> ag-grid/AgGridColumn {:field "inferred_species_percent" :maxWidth 160 :headerName "Species Reads (%)" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]
      [:> ag-grid/AgGridColumn {:field "inferred_species_genome_size_mb" :maxWidth 140 :headerName "Genome Size (Mb)" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]
      [:> ag-grid/AgGridColumn {:field "total_bases" :maxWidth 140 :headerName "Total Bases (Mb)" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]
      [:> ag-grid/AgGridColumn {:field "percent_bases_above_q30" :maxWidth 160 :headerName "Bases Above Q30 (%)" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]
      [:> ag-grid/AgGridColumn {:field "inferred_species_estimated_depth" :maxWidth 172 :headerName "Est. Depth Coverage" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]
      [:> ag-grid/AgGridColumn {:field "fastqc_r1_link" :headerName "FastQC R1" :maxWidth 96 :cellRenderer cell-renderer-hyperlink-fastqc-r1}]
      [:> ag-grid/AgGridColumn {:field "fastqc_r2_link" :headerName "FastQC R2" :maxWidth 96 :cellRenderer cell-renderer-hyperlink-fastqc-r2}]
      ]]
    ))


(defn library-species-abundance-table []
  (let [currently-selected-run-id (:selected-run-id @db)
        selected-run-species-abundance (get-in @db [:species-abundance currently-selected-run-id])
        row-data (->> selected-run-species-abundance
                      (map (fn [x] (update x :abundance_1_fraction_total_reads #(.toFixed (* 100 %) 2))))
                      (map (fn [x] (update x :abundance_2_fraction_total_reads #(.toFixed (* 100 %) 2))))
                      (map (fn [x] (update x :abundance_3_fraction_total_reads #(.toFixed (* 100 %) 2))))
                      (map (fn [x] (update x :abundance_4_fraction_total_reads #(.toFixed (* 100 %) 2))))
                      (map (fn [x] (update x :abundance_5_fraction_total_reads #(.toFixed (* 100 %) 2)))))]
    [:div {:class "ag-theme-balham"
           :style {}}
     [:> ag-grid/AgGridReact
      {:rowData row-data
       :pagination false
       :enableCellTextSelection true
       :floatingFilter true
       :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
       :onSelectionChanged #()
       }
      [:> ag-grid/AgGridColumn {:field "library_id" :headerName "Library ID" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter" :pinned "left" :checkboxSelection false :headerCheckboxSelectionFilteredOnly true}]
      [:> ag-grid/AgGridColumn {:field "project_id" :headerName "Project ID" :maxWidth 200 :sortable true :resizable true :filter "agTextColumnFilter"}]
      [:> ag-grid/AgGridColumn {:headerName "Most Abundant Species"}
       [:> ag-grid/AgGridColumn {:field "abundance_1_name" :maxWidth 140 :headerName "Species Name" :sortable true :resizable true :filter "agTextColumnFilter"}]
       [:> ag-grid/AgGridColumn {:field "abundance_1_fraction_total_reads" :maxWidth 120 :headerName "Abundance" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]]
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



(defn illumina []
  [:div {:style {:display "grid"
                  :grid-template-columns "1fr 4fr"
                  :grid-template-rows "repeat(2, 1fr)"
                  :gap "4px"
                  :height "800px"}}
   [:div {:style {:display "grid"
                  :grid-column "1"
                  :grid-row "1 / 3"}}
    [illumina-runs-table]]
   [:div {:style {:display "grid"
                  :grid-column "2"
                  :grid-row "1"
                  :gap "4px"}}
    [library-sequence-qc-table]]
   [:div {:style {:display "grid"
                  :grid-column "2"
                  :grid-row "2"}}
    [library-species-abundance-table]]])

;; Preparing to have separate tabs for illumina and nanopore but not implemented yet.
(defn tabs []
  (let [panes [{:menuItem "Illumina" :render #(r/reactify-component [:> semantic-ui/Tab.Pane (r/reactify-component illumina)])}
               {:menuItem "Nanopore" :render #(r/reactify-component [:> semantic-ui/Tab.Pane "Tab 2 content."])}]]
    [:> semantic-ui/Tab {:panes panes}]))


(defn root []
  [:div {:style {:display "grid"
                 :grid-template-columns "1fr"
                 :grid-gap "4px 4px"
                 :height "100%"}}
   [header]
   [illumina]
   #_[debug-view]
   ])

(defn main []
  (load-sequencing-runs)
  (rdom/render [root] (js/document.getElementById "app")))

(set! (.-onload js/window) main)

(comment

  )
