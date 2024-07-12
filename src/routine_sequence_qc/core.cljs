(ns routine-sequence-qc.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.string :as str]
            [clojure.set]
            [reagent.core :as r] 
            [reagent.dom :as rdom]
            [reagent.dom.server]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [ag-grid-react :as ag-grid]
            [ag-charts-react :as ag-chart]
            [cljs.pprint :refer [pprint]]))


(defonce db (r/atom {}))


(def app-version "v3.3.0")

(def palette {:green "#6ade8a"
              :yellow "#f5d76e"
              :red "#e6675e"
              :grey "#919191"})

(defn load-sequencing-runs
  "Pull the sequencing runs from the server and add them to the app db."
  []
  (go
    (let [response (<! (http/get  "data/runs.json"))]
      (cond (= 200 (:status response))
        (swap! db assoc-in [:runs] (:body response))))))


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


(defn header
  "Component for displaying the header."
  []
  [:header {:style {:display "grid"
                    :grid-template-columns "repeat(2, 1fr)"
                    :align-items "center"
                    :height "48px"}}
   [:div {:style {:display "grid"
                  :grid-template-columns "repeat(2, 1fr)"
                  :align-items "center"}}
    [:h1 {:style {:font-family "Arial" :color "#004a87" :margin "0px"}} "Routine Sequence QC"][:p {:style {:font-family "Arial" :color "grey" :justify-self "start"}} app-version]]
   [:div {:style {:display "grid" :align-self "center" :justify-self "end"}}
    [:img {:src (str "images/bccdc_logo.svg") :height "48px"}]]])


(defn get-selected-rows
  "Function to get the selected rows from the runs table."
  [e]
  (map #(js->clj (.-data %) :keywordize-keys true)
       (-> e
           .-api
           .getSelectedNodes)))


(defn run-selected
  "Function to run when a row is selected in the runs table."
  [e]
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

(defn cell-renderer-hyperlink-bracken [params]
  (cell-renderer-hyperlink-button "Abundances" params))


(defn illumina-runs-table
  "Component for displaying Illumina sequencing runs."
  []
  (let [runs (:runs @db)
        grid-ref (clj->js {:current nil})
        today-js-date (new js/Date)
        today-y-m-d [(.getFullYear today-js-date) (+ 1 (.getMonth today-js-date)) (.getDate today-js-date)]
        today-iso-str (str/join "-" today-y-m-d)
        _ (js/console.log today-y-m-d)
        _ (js/console.log today-iso-str)
        add-multiqc-link #(assoc % :multiqc_link (str "data/multiqc/" (:run_id %) "_multiqc.html"))
        qc-status-style (fn [params]
                          (let [cell-value (. params -value)]
                            (cond (= "PASS" cell-value) (clj->js {:backgroundColor (:green palette)})
                                  (and (string? cell-value)
                                       (re-find #"PASS" cell-value))
                                       (clj->js {:backgroundColor (:green palette)})
                                  (= "WARN" cell-value) (clj->js {:backgroundColor (:yellow palette)})
                                  (= "FAIL" cell-value) (clj->js {:backgroundColor (:red palette)})
                                  :else (clj->js {:backgroundColor (:grey palette)}))))
        qc-metric-style (fn [metric]
                          (fn [params]
                            (let [row-data (js->clj (. params -data) {:keywordize-keys true})
                                  checked-metrics (get-in row-data [:run_qc_check :checked_metrics]) 
                                  checked-metric (first (filter #(= (:metric %) metric) checked-metrics))
                                  pass-fail (get-in checked-metric [:pass_fail])]
                              (cond (= pass-fail "FAIL")
                                    (clj->js {:backgroundColor (:red palette)})
                                    :else (clj->js {})))))
        add-qc-status (fn [run]
                        (let [qc-status (get-in run [:run_qc_check :overall_qc_pass_fail])]
                          (assoc run :run_qc_check_status qc-status)))
        add-error-rate (fn [run]
                         (let [checked-metrics (get-in run [:run_qc_check :checked_metrics])
                               error-rate-check (filter #(= (:metric %) "ErrorRate") checked-metrics)
                               error-rate (if (empty? error-rate-check)
                                            nil
                                            (:value (first error-rate-check)))]
                           (assoc run :run_error_rate error-rate)))
        add-percent-pf (fn [run]
                         (let [checked-metrics (get-in run [:run_qc_check :checked_metrics])
                               percent-pf-check (filter #(= (:metric %) "PercentPf") checked-metrics)
                               percent-pf (if (empty? percent-pf-check)
                                            nil
                                            (:value (first percent-pf-check)))]
                           (assoc run :run_percent_pf percent-pf)))
        add-percent-q30 (fn [run]
                          (let [checked-metrics (get-in run [:run_qc_check :checked_metrics])
                                percent-q30-check (filter #(= (:metric %) "PercentGtQ30") checked-metrics)
                                percent-q30 (if (empty? percent-q30-check)
                                              nil
                                              (:value (first percent-q30-check)))]
                            (assoc run :run_percent_q30 percent-q30)))
        add-percent-aligned (fn [run]
                              (let [checked-metrics (get-in run [:run_qc_check :checked_metrics])
                                    percent-aligned-check (filter #(= (:metric %) "PercentAligned") checked-metrics)
                                    percent-aligned (if (empty? percent-aligned-check)
                                                      nil
                                                      (:value (first percent-aligned-check)))]
                                (assoc run :run_percent_aligned percent-aligned)))
        add-yield (fn [run]
                    (let [checked-metrics (get-in run [:run_qc_check :checked_metrics])
                          yield-check (filter #(= (:metric %) "YieldTotal") checked-metrics)
                          yield (if (empty? yield-check)
                                  nil
                                  (:value (first yield-check)))]
                      (assoc run :run_yield yield)))
        add-fastq-data (fn [run]
                          (let [checked-metrics (get-in run [:run_qc_check :checked_metrics])
                                fastq-data-check (filter #(= (:metric %) "SumSampleFastqFileSizesMb") checked-metrics)
                                fastq-data (if (empty? fastq-data-check)
                                             nil
                                             (:value (first fastq-data-check)))]
                            (assoc run :run_fastq_data_mb fastq-data)))
        row-data (->> runs
                      (map add-multiqc-link)
                      (map add-qc-status)
                      (map add-error-rate)
                      (map add-percent-pf)
                      (map add-percent-q30)
                      (map add-percent-aligned)
                      (map add-yield)
                      (map add-fastq-data))]
    [:div {:style {:display "grid"
                   :grid-template-columns "1fr"
                   :grid-template-rows "23fr 1fr"}}
     [:div {:class "ag-theme-balham"
            :style {}}
      [:> ag-grid/AgGridReact
       {:ref grid-ref
        :rowData row-data
        :pagination false
        :rowSelection "single"
        :enableCellTextSelection true
        :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
        :onSelectionChanged run-selected}
       [:> ag-grid/AgGridColumn {:field "run_id"
                                 :headerName "Run ID"
                                 :minWidth 200
                                 :resizable true
                                 :filter "agTextColumnFilter"
                                 :sortable true
                                 :checkboxSelection true
                                 :sort "desc"
                                 :floatingFilter true}]
       [:> ag-grid/AgGridColumn {:field "run_qc_check_status"
                                 :headerName "QC"
                                 :minWidth 72
                                 :maxWidth 172
                                 :resizable true
                                 :filter "agTextColumnFilter"
                                 :sortable true
                                 :floatingFilter true
                                 :cellStyle qc-status-style}]
       [:> ag-grid/AgGridColumn {:field "multiqc_link"
                                 :headerName "MultiQC"
                                 :minWidth 96
                                 :maxWidth 128
                                 :resizable true
                                 :cellRenderer cell-renderer-hyperlink-multiqc
                                 :floatingFilter true}]
       [:> ag-grid/AgGridColumn {:field "run_error_rate"
                                 :headerName "Error Rate"
                                 :minWidth 96
                                 :maxWidth 128
                                 :resizable true
                                 :filter "agNumberColumnFilter"
                                 :sortable true
                                 :floatingFilter true
                                 :cellStyle (qc-metric-style "ErrorRate")}]
       [:> ag-grid/AgGridColumn {:field "run_percent_pf"
                                 :headerName "% Pass Filter"
                                 :minWidth 110
                                 :maxWidth 128
                                 :resizable true
                                 :filter "agNumberColumnFilter"
                                 :sortable true
                                 :floatingFilter true
                                 :cellStyle (qc-metric-style "PercentPf")}]
       [:> ag-grid/AgGridColumn {:field "run_percent_q30"
                                 :headerName "% Q30"
                                 :minWidth 96
                                 :maxWidth 128
                                 :resizable true
                                 :filter "agNumberColumnFilter"
                                 :sortable true
                                 :floatingFilter true
                                 :cellStyle (qc-metric-style "PercentGtQ30")}]
       [:> ag-grid/AgGridColumn {:field "run_percent_aligned"
                                :headerName "% Aligned"
                                 :minWidth 96
                                 :maxWidth 128
                                 :resizable true
                                 :filter "agNumberColumnFilter"
                                 :sortable true
                                 :floatingFilter true
                                 :cellStyle (qc-metric-style "PercentAligned")}]
       [:> ag-grid/AgGridColumn {:field "run_yield"
                                 :headerName "Yield (Gigabases)"
                                 :minWidth 96
                                 :maxWidth 152
                                 :resizable true
                                 :filter "agNumberColumnFilter"
                                 :sortable true
                                 :floatingFilter true
                                 :cellStyle (qc-metric-style "YieldTotal")}]
       [:> ag-grid/AgGridColumn {:field "run_fastq_data_mb"
                                 :headerName "Fastq Data (Mb)"
                                 :minWidth 96
                                 :maxWidth 150
                                 :resizable true
                                 :filter "agNumberColumnFilter"
                                 :sortable true
                                 :floatingFilter true
                                 :cellStyle (qc-metric-style "SumSampleFastqFileSizesMb")}]]
      [:div {:style {:grid-row "2"}}
       [:button {:onClick #(.exportDataAsCsv (.-api (.-current grid-ref)) (clj->js {:fileName (str today-iso-str "_illumina_sequencing_runs_routine_qc.csv")}))} "Export CSV"]]]]))


(defn library-sequence-qc-table
  "Component for displaying library sequence QC data."
  []
  (let [grid-ref (clj->js {:current nil})
        currently-selected-run-id (:selected-run-id @db)
        selected-run-library-qc (get-in @db [:library-qc currently-selected-run-id])
        add-fastqc-r1-link #(assoc % :fastqc_r1_link (str "data/fastqc/" currently-selected-run-id "/" (:library_id %) "_R1_fastqc.html"))
        add-fastqc-r2-link #(assoc % :fastqc_r2_link (str "data/fastqc/" currently-selected-run-id "/" (:library_id %) "_R2_fastqc.html"))
        row-data (->> selected-run-library-qc
                      (map (fn [x] (update x :inferred_species_percent #(if % (.toFixed % 2) 0.00))))
                      (map (fn [x] (update x :total_bases #(if % (.toFixed (/ % 1000000) 3)))))
                      (map (fn [x] (update x :inferred_species_estimated_depth #(if % (.toFixed % 2) nil))))
                      (map (fn [x] (update x :percent_bases_above_q30 #(if % (.toFixed % 2)))))
                      (map add-fastqc-r1-link)
                      (map add-fastqc-r2-link))]
    [:div {:style {:display "grid"
                   :grid-template-columns "1fr"
                   :grid-template-rows "11fr 1fr"}}
     [:div {:class "ag-theme-balham"
            :style {}}
      [:> ag-grid/AgGridReact
       {:ref grid-ref
        :rowData row-data
        :pagination false
        :enableCellTextSelection true
        :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
        :onSelectionChanged #()}
       [:> ag-grid/AgGridColumn {:field "library_id"
                                 :headerName "Library ID"
                                 :maxWidth 200
                                 :sortable true
                                 :resizable true
                                 :filter "agTextColumnFilter"
                                 :pinned "left"
                                 :checkboxSelection false
                                 :headerCheckboxSelectionFilteredOnly true
                                 :floatingFilter true}]
       [:> ag-grid/AgGridColumn {:field "project_id"
                                 :headerName "Project ID"
                                 :maxWidth 200
                                 :sortable true
                                 :resizable true
                                 :filter "agTextColumnFilter"
                                 :floatingFilter true}]
       [:> ag-grid/AgGridColumn {:field "inferred_species_name"
                                 :headerName "Inferred Species"
                                 :maxWidth 200
                                 :sortable true
                                 :resizable true
                                 :filter "agTextColumnFilter"
                                 :floatingFilter true}]
       [:> ag-grid/AgGridColumn {:field "inferred_species_percent"
                                 :maxWidth 160
                                 :headerName "Species Reads (%)"
                                 :sortable true
                                 :resizable true
                                 :filter "agNumberColumnFilter"
                                 :type "numericColumn"
                                 :floatingFilter true}]
       [:> ag-grid/AgGridColumn {:field "inferred_species_genome_size_mb"
                                 :maxWidth 140
                                 :headerName "Genome Size (Mb)"
                                 :sortable true
                                 :resizable true
                                 :filter "agNumberColumnFilter"
                                 :type "numericColumn"
                                 :floatingFilter true}]
       [:> ag-grid/AgGridColumn {:field "total_bases"
                                 :maxWidth 140
                                 :headerName "Total Bases (Mb)"
                                 :sortable true
                                 :resizable true
                                 :filter "agNumberColumnFilter"
                                 :type "numericColumn"
                                 :floatingFilter true}]
       [:> ag-grid/AgGridColumn {:field "percent_bases_above_q30"
                                 :maxWidth 160
                                 :headerName "Bases Above Q30 (%)"
                                 :sortable true
                                 :resizable true
                                 :filter "agNumberColumnFilter"
                                 :type "numericColumn"
                                 :floatingFilter true}]
       [:> ag-grid/AgGridColumn {:field "inferred_species_estimated_depth"
                                 :maxWidth 172
                                 :headerName "Est. Depth Coverage"
                                 :sortable true
                                 :resizable true
                                 :filter "agNumberColumnFilter"
                                 :type "numericColumn"
                                 :floatingFilter true}]
       [:> ag-grid/AgGridColumn {:field "fastqc_r1_link"
                                 :headerName "FastQC R1"
                                 :maxWidth 96
                                 :cellRenderer cell-renderer-hyperlink-fastqc-r1}]
       [:> ag-grid/AgGridColumn {:field "fastqc_r2_link"
                                 :headerName "FastQC R2"
                                 :maxWidth 96
                                 :cellRenderer cell-renderer-hyperlink-fastqc-r2}]]]
     [:div {:style {:grid-row "2"}}
      [:button {:onClick #(.exportDataAsCsv (.-api (.-current grid-ref)) (clj->js {:fileName (str currently-selected-run-id "_library_qc.csv")}))} "Export CSV"]]]))


(defn library-species-abundance-table
  "Component for displaying species abundance data."
  []
  (let [grid-ref (clj->js {:current nil})
        add-bracken-link #(assoc % :bracken_link (str "data/bracken-species-abundances/" (:run_id %) "/" (:library_id %) "_bracken_species_abundances.tsv"))
        currently-selected-run-id (:selected-run-id @db)
        selected-run-species-abundance (get-in @db [:species-abundance currently-selected-run-id])
        row-data (->> selected-run-species-abundance
                      (map #(assoc % :run_id currently-selected-run-id))
                      (map add-bracken-link)
                      (map (fn [x] (update x :abundance_1_fraction_total_reads #(.toFixed (* 100 %) 2))))
                      (map (fn [x] (update x :abundance_2_fraction_total_reads #(.toFixed (* 100 %) 2))))
                      (map (fn [x] (update x :abundance_3_fraction_total_reads #(.toFixed (* 100 %) 2))))
                      (map (fn [x] (update x :abundance_4_fraction_total_reads #(.toFixed (* 100 %) 2))))
                      (map (fn [x] (update x :abundance_5_fraction_total_reads #(.toFixed (* 100 %) 2)))))]
    [:div {:style {:display "grid"
                   :grid-template-columns "1fr"
                   :grid-template-rows "11fr 1fr"}}
     [:div {:class "ag-theme-balham"
            :style {}}
      [:> ag-grid/AgGridReact
       {:ref grid-ref
        :rowData row-data
        :pagination false
        :enableCellTextSelection true
        :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
        :onSelectionChanged #()}
       [:> ag-grid/AgGridColumn {:field "library_id"
                                 :headerName "Library ID"
                                 :maxWidth 200
                                 :sortable true
                                 :resizable true
                                 :filter "agTextColumnFilter"
                                 :pinned "left"
                                 :checkboxSelection false
                                 :headerCheckboxSelectionFilteredOnly true
                                 :floatingFilter true}]
       [:> ag-grid/AgGridColumn {:field "bracken_link"
                                 :headerName "Abundances"
                                 :maxWidth 128
                                 :cellRenderer cell-renderer-hyperlink-bracken
                                 :floatingFilter false}]
       [:> ag-grid/AgGridColumn {:field "project_id"
                                 :headerName "Project ID"
                                 :maxWidth 200
                                 :sortable true
                                 :resizable true
                                 :filter "agTextColumnFilter"
                                 :floatingFilter true}]
       [:> ag-grid/AgGridColumn {:headerName "Most Abundant Species"}
        [:> ag-grid/AgGridColumn {:field "abundance_1_name"
                                  :maxWidth 140
                                  :headerName "Species Name"
                                  :sortable true
                                  :resizable true
                                  :filter "agTextColumnFilter"
                                  :floatingFilter true}]
        [:> ag-grid/AgGridColumn {:field "abundance_1_fraction_total_reads"
                                  :maxWidth 120
                                  :headerName "Abundance"
                                  :sortable true
                                  :resizable true
                                  :filter "agNumberColumnFilter"
                                  :type "numericColumn"
                                  :floatingFilter true}]]
       [:> ag-grid/AgGridColumn {:headerName "2nd Most Abundant Species"}
        [:> ag-grid/AgGridColumn {:field "abundance_2_name"
                                  :maxWidth 140
                                  :headerName "Species Name"
                                  :sortable true
                                  :resizable true
                                  :filter "agTextColumnFilter"
                                  :floatingFilter true}]
        [:> ag-grid/AgGridColumn {:field "abundance_2_fraction_total_reads"
                                  :maxWidth 120
                                  :headerName "Abundance (%)"
                                  :sortable true
                                  :resizable true
                                  :filter "agNumberColumnFilter"
                                  :type "numericColumn"
                                  :floatingFilter true}]]
       [:> ag-grid/AgGridColumn {:headerName "3rd Most Abundant Species"}
        [:> ag-grid/AgGridColumn {:field "abundance_3_name"
                                  :maxWidth 140
                                  :headerName "Species Name"
                                  :sortable true
                                  :resizable true
                                  :filter "agTextColumnFilter"
                                  :floatingFilter true}]
        [:> ag-grid/AgGridColumn {:field "abundance_3_fraction_total_reads"
                                  :maxWidth 120
                                  :headerName "Abundance (%)"
                                  :sortable true
                                  :resizable true
                                  :filter "agNumberColumnFilter"
                                  :type "numericColumn"
                                  :floatingFilter true}]]
       [:> ag-grid/AgGridColumn {:headerName "4th Most Abundant Species"}
        [:> ag-grid/AgGridColumn {:field "abundance_4_name"
                                  :maxWidth 140
                                  :headerName "Species Name"
                                  :sortable true
                                  :resizable true
                                  :filter "agTextColumnFilter"
                                  :floatingFilter true}]
        [:> ag-grid/AgGridColumn {:field "abundance_4_fraction_total_reads"
                                  :maxWidth 120
                                  :headerName "Abundance (%)"
                                  :sortable true
                                  :resizable true
                                  :filter "agNumberColumnFilter"
                                  :type "numericColumn"
                                  :floatingFilter true}]]
       [:> ag-grid/AgGridColumn {:headerName "5th Most Abundant Species"}
        [:> ag-grid/AgGridColumn {:field "abundance_5_name"
                                  :maxWidth 140
                                  :headerName "Species Name"
                                  :sortable true
                                  :resizable true
                                  :filter "agTextColumnFilter"
                                  :floatingFilter true}]
        [:> ag-grid/AgGridColumn {:field "abundance_5_fraction_total_reads"
                                  :maxWidth 120
                                  :headerName "Abundance (%)"
                                  :sortable true
                                  :resizable true
                                  :filter "agNumberColumnFilter"
                                  :type "numericColumn"
                                  :floatingFilter true}]]]] 
     [:div {:style {:grid-row "2"}}
      [:button {:onClick #(.exportDataAsCsv (.-api (.-current grid-ref)) (clj->js {:fileName (str currently-selected-run-id "_species_abundance.csv")}))} "Export CSV"]]]))


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
    [illumina-runs-table]]
   [:div {:style {:display "grid"
                  :grid-column "2"
                  :grid-row "1"
                  :gap "4px"
                  :overflow "auto"
                  :resize "horizontal"}}
    [library-sequence-qc-table]]
   [:div {:style {:display "grid"
                  :grid-column "2"
                  :grid-row "2"
                  :overflow "auto"
                  :resize "horizontal"}}
    [library-species-abundance-table]]])


(defn root
  "Root component."
  []
  [:div {:style {:display "grid"
                 :grid-template-columns "1fr"
                 :grid-gap "4px 4px"
                 :height "100%"}}
   [header]
   [illumina]])


(defn render
  "Render the application."
  []
  (rdom/render [root] (js/document.getElementById "app")))


(defn main
  "Main entry-point."
  []
  (load-sequencing-runs)
  (render))


(defn init
  "Initialize the application."
  []
  (set! (.-onload js/window) main))


(comment

  )
