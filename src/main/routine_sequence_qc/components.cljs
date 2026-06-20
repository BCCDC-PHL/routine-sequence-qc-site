(ns routine-sequence-qc.components
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [routine-sequence-qc.state :refer [db]]
            [routine-sequence-qc.loaders :as loaders]
            [routine-sequence-qc.grid :as grid]
            [routine-sequence-qc.color :refer [palette]]))


;; Header Component

(defn header
  "Component for displaying the header."
  [app-version]
  [:header {:style {:display "grid"
                    :grid-template-columns "repeat(2, 1fr)"
                    :align-items "center"
                    :height "48px"}}
   [:div {:style {:display "grid"
                  :grid-template-columns "repeat(2, 1fr)"
                  :align-items "center"}}
    [:h1 {:style {:font-family "Arial" :color (:bccdc-blue palette) :margin "0px"}} "Routine Sequence QC"] [:p {:style {:font-family "Arial" :color "grey" :justify-self "start"}} app-version]]
   [:div {:style {:display "grid" :align-self "center" :justify-self "end"}}
    [:img {:src "images/logo.svg" :height "48px"}]]])

;; Cell Renderers

(defn cell-renderer-hyperlink-button [text params]
  (r/as-element
   [:button
    [:a {:href (str (.-value params))
         :style {:color "inherit"}
         :text-decoration "inherit"
         :target "_blank"} text]]))

(defn cell-renderer-hyperlink-multiqc [params]
  (cell-renderer-hyperlink-button "MultiQC" params))

(defn cell-renderer-hyperlink-fastqc-r1 [params]
  (cell-renderer-hyperlink-button "FastQC R1" params))

(defn cell-renderer-hyperlink-fastqc-r2 [params]
  (cell-renderer-hyperlink-button "FastQC R2" params))

(defn cell-renderer-hyperlink-bracken [params]
  (cell-renderer-hyperlink-button "Abundances" params))


;; Sequencing Runs Table

;; Sequencing Runs Table Helper Fns

(defn run-id->date
  "Extract the date from a run ID as an ISO-8601 string YYYY-MM-DD"
  [run-id]
  (let [date-part (first (str/split run-id "_"))
        normalized-date (cond (= 6 (count date-part))
                              (str "20" (subs date-part 0 6))
                              (= 8 (count date-part))
                              (subs date-part 0 8))
        year (subs normalized-date 0 4)
        month (subs normalized-date 4 6)
        day (subs normalized-date 6 8)]
    (str/join "-" [year month day])))

(defn get-applied-qc-threshold
  "Given a run with structure: {:run_qc_check {:checked_metrics [{:metric \"metric1\"}]}},
  and a name of a metric, return the threshold that was applied for QC of that metric."
  [run metric-name]
  (let [checked-metrics (get-in run [:run_qc_check :checked_metrics])
        metric-of-interest (first (filter #(= (:metric %) metric-name) checked-metrics))]
    (get metric-of-interest :threshold "Unknown")))

(defn run-selected
  "Function to run when a row is selected in the runs table."
  [e]
  (let [previously-selected-run-ids (:selected-run-ids @db)
        currently-selected-run-id (:run_id (first (grid/get-selected-rows e)))]
    (loaders/load-library-qc currently-selected-run-id)
    (loaders/load-species-abundance currently-selected-run-id)
    (swap! db assoc-in [:selected-run-id] currently-selected-run-id)))

(defn qc-status-style
  [params]
  (let [cell-value (. params -value)]
    (cond (= "PASS" cell-value) (clj->js {:backgroundColor (:green palette)})
          (and (string? cell-value)
               (re-find #"PASS" cell-value))
          (clj->js {:backgroundColor (:green palette)})
          (= "WARN" cell-value) (clj->js {:backgroundColor (:yellow palette)})
          (= "FAIL" cell-value) (clj->js {:backgroundColor (:red palette)})
          :else (clj->js {:backgroundColor (:grey palette)}))))

(defn qc-metric-style
  [metric]
  (fn [params]
    (let [row-data (js->clj (. params -data) {:keywordize-keys true})
          checked-metrics (get-in row-data [:run_qc_check :checked_metrics])
          checked-metric (first (filter #(= (:metric %) metric) checked-metrics))
          pass-fail (get-in checked-metric [:pass_fail])]
      (cond (= pass-fail "FAIL")
            (clj->js {:backgroundColor (:red palette)})
            :else (clj->js {})))))

(defn add-run-date
  [{:keys [run_id] :as run}]
  (let [date (run-id->date run_id)]
    (assoc run :run_date date)))

(defn add-qc-status
  [run]
  (let [qc-status (get-in run [:run_qc_check :overall_qc_pass_fail])]
    (assoc run :run_qc_check_status qc-status)))

(defn add-error-rate
  [run]
  (let [checked-metrics (get-in run [:run_qc_check :checked_metrics])
        error-rate-check (filter #(= (:metric %) "ErrorRate") checked-metrics)
        error-rate (if (empty? error-rate-check)
                     nil
                     (:value (first error-rate-check)))]
    (assoc run :run_error_rate error-rate)))

(defn add-percent-pf
  [run]
  (let [checked-metrics (get-in run [:run_qc_check :checked_metrics])
        percent-pf-check (filter #(= (:metric %) "PercentPf") checked-metrics)
        percent-pf (if (empty? percent-pf-check)
                     nil
                     (:value (first percent-pf-check)))]
    (assoc run :run_percent_pf percent-pf)))

(defn add-percent-q30
  [run]
  (let [checked-metrics (get-in run [:run_qc_check :checked_metrics])
        percent-q30-check (filter #(= (:metric %) "PercentGtQ30") checked-metrics)
        percent-q30 (if (empty? percent-q30-check)
                      nil
                      (:value (first percent-q30-check)))]
    (assoc run :run_percent_q30 percent-q30)))

(defn add-percent-aligned
  [run]
  (let [checked-metrics (get-in run [:run_qc_check :checked_metrics])
        percent-aligned-check (filter #(= (:metric %) "PercentAligned") checked-metrics)
        percent-aligned (if (empty? percent-aligned-check)
                          nil
                          (:value (first percent-aligned-check)))]
    (assoc run :run_percent_aligned percent-aligned)))

(defn add-yield
  [run]
  (let [checked-metrics (get-in run [:run_qc_check :checked_metrics])
        yield-check (filter #(= (:metric %) "YieldTotal") checked-metrics)
        yield (if (empty? yield-check)
                nil
                (:value (first yield-check)))]
    (assoc run :run_yield yield)))

(defn add-fastq-data [run]
  (let [checked-metrics (get-in run [:run_qc_check :checked_metrics])
        fastq-data-check (filter #(= (:metric %) "SumSampleFastqFileSizesMb") checked-metrics)
        fastq-data (if (empty? fastq-data-check)
                     nil
                     (:value (first fastq-data-check)))]
    (assoc run :run_fastq_data_mb fastq-data)))

(defn add-multiqc-link
  [run]
  (let [run-id (:run_id run)]
    (assoc run :multiqc_link (str "data/multiqc/" run-id "_multiqc.html"))))

(defn export-illumina-runs-table
  [grid-ref today-iso-str]
  (.exportDataAsCsv (.-api (.-current grid-ref)) (clj->js {:fileName (str today-iso-str "_illumina_sequencing_runs_routine_qc.csv")})))

;; Sequencing Runs Table Column Defs

(defn illumina-runs-table-column-defs
  [latest-run]
  [{:field "run_id"
    :headerName "Run ID"
    :minWidth 200
    :resizable true
    :filter "agTextColumnFilter"
    :sortable true
    :floatingFilter true}
   {:field "run_date"
    :headerName "Run Date"
    :hide true
    :sort "desc"}
   {:field "run_qc_check_status"
    :headerName "QC Status"
    :minWidth 128
    :maxWidth 172
    :resizable true
    :filter "agTextColumnFilter"
    :sortable true
    :floatingFilter true
    :cellStyle qc-status-style
    :headerTooltip "Overall Run QC Status"}
   {:field "multiqc_link"
    :headerName "MultiQC"
    :minWidth 96
    :maxWidth 128
    :resizable true
    :cellRenderer cell-renderer-hyperlink-multiqc
    :floatingFilter true}
   {:field "run_error_rate"
    :headerName "Error Rate"
    :minWidth 96
    :maxWidth 128
    :resizable true
    :filter "agNumberColumnFilter"
    :sortable true
    :floatingFilter true
    :cellStyle (qc-metric-style "ErrorRate")
    :headerTooltip (str "Sequencing Error Rate\n"
                        "Estimated from PhiX alignment.\n"
                        "Current Threshold: " (get-applied-qc-threshold latest-run "ErrorRate") "%")
    :tooltipValueGetter #(str "Applied Threshold: "
                              (get-applied-qc-threshold (grid/get-cell-data %) "ErrorRate") "%")}
   {:field "run_percent_pf"
    :headerName "% Pass Filter"
    :minWidth 110
    :maxWidth 128
    :resizable true
    :filter "agNumberColumnFilter"
    :sortable true
    :floatingFilter true
    :cellStyle (qc-metric-style "PercentPf")
    :headerTooltip (str "Percentage of Clusters Passed Filter\n"
                        "Low-quality clusters are filtered out and do not generate reads\n"
                        "Current Threshold: " (get-applied-qc-threshold latest-run "PercentPf") "%")
    :tooltipValueGetter #(str "Applied Threshold: "
                              (get-applied-qc-threshold (grid/get-cell-data %) "PercentPf") "%")}
   {:field "run_percent_q30"
    :headerName "% Q30"
    :minWidth 96
    :maxWidth 128
    :resizable true
    :filter "agNumberColumnFilter"
    :sortable true
    :floatingFilter true
    :cellStyle (qc-metric-style "PercentGtQ30")
    :headerTooltip (str "Percentage of bases with Quality Score over 30\n"
                        "Q30 is ~1/1000 chance of error.\n"
                        "Current Threshold: " (get-applied-qc-threshold latest-run "PercentGtQ30") "%")
    :tooltipValueGetter #(str "Applied Threshold: "
                              (get-applied-qc-threshold (grid/get-cell-data %) "PercentGtQ30") "%")}
   {:field "run_percent_aligned"
    :headerName "% PhiX Aligned"
    :minWidth 128
    :maxWidth 256
    :resizable true
    :filter "agNumberColumnFilter"
    :sortable true
    :floatingFilter true
    :cellStyle (qc-metric-style "PercentAligned")
    :headerTooltip (str "Percentage of reads aligned to PhiX\n"
                        "PhiX is included as internal control on most runs.\n"
                        "Current Threshold: " (get-applied-qc-threshold latest-run "PercentAligned") "%")
    :tooltipValueGetter #(str "Applied Threshold: "
                              (get-applied-qc-threshold (grid/get-cell-data %) "PercentAligned") "%")}
   {:field "run_yield"
    :headerName "Yield (GBases)"
    :minWidth 128
    :maxWidth 256
    :resizable true
    :filter "agNumberColumnFilter"
    :sortable true
    :floatingFilter true
    :cellStyle (qc-metric-style "YieldTotal")
    :headerTooltip (str "Total Run Yield, in Gigabases\n"
                        "The sum of sequence data across all samples.\n"
                        "Current Threshold: " (get-applied-qc-threshold latest-run "YieldTotal") " Gb")
    :tooltipValueGetter #(str "Applied Threshold: "
                              (get-applied-qc-threshold (grid/get-cell-data %) "YieldTotal") " Gb")}
   {:field "run_fastq_data_mb"
    :headerName "Fastq Data (Mbytes)"
    :minWidth 128
    :maxWidth 150
    :resizable true
    :filter "agNumberColumnFilter"
    :sortable true
    :floatingFilter true
    :cellStyle (qc-metric-style "SumSampleFastqFileSizesMb")
    :headerTooltip (str "Total Sample Fastq data, in Megabytes\n"
                        "The sum of sequence data across all samples.\n"
                        "Current Threshold: " (get-applied-qc-threshold latest-run "SumSampleFastqFileSizesMb") " MB")
    :tooltipValueGetter #(str "Applied Threshold: "
                              (get-applied-qc-threshold (grid/get-cell-data %) "SumSampleFastqFileSizesMb") " MB")}])

;; Sequencing Runs Table Component

(defn illumina-runs-table
  "Component for displaying Illumina sequencing runs."
  []
  (let [runs (:runs @db)
        grid-ref (clj->js {:current nil})
        today-js-date (new js/Date)
        today-y-m-d [(.getFullYear today-js-date) (+ 1 (.getMonth today-js-date)) (.getDate today-js-date)]
        today-iso-str (str/join "-" today-y-m-d)
        row-data (->> runs
                      (map add-run-date)
                      (map add-multiqc-link)
                      (map add-qc-status)
                      (map add-error-rate)
                      (map add-percent-pf)
                      (map add-percent-q30)
                      (map add-percent-aligned)
                      (map add-yield)
                      (map add-fastq-data))
        latest-run (last (sort-by :run_date row-data))]
    [:div {:style {:display "grid"
                   :grid-template-columns "1fr"
                   :grid-template-rows "23fr 1fr"}}
     [:div {:class "ag-theme-balham"
            :style {}}
      [grid/ag-grid-react
       {:ref grid-ref
        :columnDefs (illumina-runs-table-column-defs latest-run)
        :rowData row-data
        :getRowId (fn [params]
                    (let [^js data (.-data params)]
                      (.-run_id data)))
        :theme "legacy"
        :pagination false
        :rowSelection {:mode "singleRow" :checkboxes true}
        :enableCellTextSelection true
        :tooltipShowDelay 10
        :tooltipHideDelay 50
        :enableBrowserTooltips true
        :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
        :onSelectionChanged run-selected}
       ]]
      [:div {:style {:grid-row "2"}}
       [:button {:onClick #(export-illumina-runs-table grid-ref today-iso-str)} "Export CSV"]]]))

;;
;; Library QC Table
;;

;; Library QC Table Helpers

(defn export-sequence-qc-table
  [grid-ref run-id]
  (.exportDataAsCsv (.-api (.-current grid-ref)) (clj->js {:fileName (str run-id "_library_qc.csv")})))

;; Library QC Table Column Definitions

(def library-sequence-qc-column-defs
  [{:field "library_id"
    :headerName "Library ID"
    :maxWidth 200
    :sortable true
    :resizable true
    :filter "agTextColumnFilter"
    :pinned "left"
    :floatingFilter true}
   {:field "project_id"
    :headerName "Project ID"
    :maxWidth 200
    :sortable true
    :resizable true
    :filter "agTextColumnFilter"
    :floatingFilter true}
   {:field "inferred_species_name"
    :headerName "Inferred Species"
    :maxWidth 200
    :sortable true
    :resizable true
    :filter "agTextColumnFilter"
    :floatingFilter true}
   {:field "inferred_species_percent"
    :maxWidth 160
    :headerName "Species Reads (%)"
    :sortable true
    :resizable true
    :filter "agNumberColumnFilter"
    :type "numericColumn"
    :floatingFilter true}
   {:field "inferred_species_genome_size_mb"
    :maxWidth 140
    :headerName "Genome Size (Mb)"
    :sortable true
    :resizable true
    :filter "agNumberColumnFilter"
    :type "numericColumn"
    :floatingFilter true}
   {:field "total_bases"
    :maxWidth 140
    :headerName "Total Bases (Mb)"
    :sortable true
    :resizable true
    :filter "agNumberColumnFilter"
    :type "numericColumn"
    :floatingFilter true}
   {:field "percent_bases_above_q30"
    :maxWidth 160
    :headerName "Bases Above Q30 (%)"
    :sortable true
    :resizable true
    :filter "agNumberColumnFilter"
    :type "numericColumn"
    :floatingFilter true}
   {:field "inferred_species_estimated_depth"
    :maxWidth 172
    :headerName "Est. Depth Coverage"
    :sortable true
    :resizable true
    :filter "agNumberColumnFilter"
    :type "numericColumn"
    :floatingFilter true}
   {:field "fastqc_r1_link"
    :headerName "FastQC R1"
    :maxWidth 96
    :cellRenderer cell-renderer-hyperlink-fastqc-r1}
   {:field "fastqc_r2_link"
    :headerName "FastQC R2"
    :maxWidth 96
    :cellRenderer cell-renderer-hyperlink-fastqc-r2}])

;; Library QC Table Component

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
                      (map (fn [x] (update x :total_bases #(when % (.toFixed (/ % 1000000) 3)))))
                      (map (fn [x] (update x :inferred_species_estimated_depth #(if % (.toFixed % 2) nil))))
                      (map (fn [x] (update x :percent_bases_above_q30 #(when % (.toFixed % 2)))))
                      (map add-fastqc-r1-link)
                      (map add-fastqc-r2-link))]
    [:div {:style {:display "grid"
                   :grid-template-columns "1fr"
                   :grid-template-rows "11fr 1fr"}}
     [:div {:class "ag-theme-balham"
            :style {}}
      [grid/ag-grid-react
       {:ref grid-ref
        :column-defs library-sequence-qc-column-defs
        :rowData row-data
        :getRowId (fn [params]
                    (let [^js data (.-data params)]
                      (.-library_id data)))
        :theme "legacy"
        :pagination false
        :enableCellTextSelection true
        :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
        :onSelectionChanged #()}]]
     [:div {:style {:grid-row "2"}}
      [:button {:onClick #(export-sequence-qc-table grid-ref currently-selected-run-id)} "Export CSV"]]]))

;; Species Abundance Table Helpers

(defn add-bracken-link 
  [{:keys [run_id library_id ] :as row}]
  (assoc row :bracken_link (str "data/bracken-species-abundances/" run_id "/" library_id "_bracken_species_abundances.tsv")))

(defn export-library-species-abundance-table
  [grid-ref run-id]
  (.exportDataAsCsv (.-api (.-current grid-ref)) (clj->js {:fileName (str run-id "_species_abundance.csv")})))

;; Species Abundance Table Column Definitions

(def library-species-abundance-column-defs
  [{:field "library_id"
    :headerName "Library ID"
    :maxWidth 200
    :sortable true
    :resizable true
    :filter "agTextColumnFilter"
    :pinned "left"
    :floatingFilter true}
   {:field "bracken_link"
    :headerName "Abundances"
    :maxWidth 128
    :cellRenderer cell-renderer-hyperlink-bracken
    :floatingFilter false}
   {:field "project_id"
    :headerName "Project ID"
    :maxWidth 200
    :sortable true
    :resizable true
    :filter "agTextColumnFilter"
    :floatingFilter true}
   {:headerName "Most Abundant Species"
    :children [{:field "abundance_1_name"
                :maxWidth 140
                :headerName "Species Name"
                :sortable true
                :resizable true
                :filter "agTextColumnFilter"
                :floatingFilter true}
               {:field "abundance_1_fraction_total_reads"
                :maxWidth 120
                :headerName "Abundance"
                :sortable true
                :resizable true
                :filter "agNumberColumnFilter"
                :type "numericColumn"
                :floatingFilter true}]}
   {:headerName "2nd Most Abundant Species"
    :children [{:field "abundance_2_name"
                :maxWidth 140
                :headerName "Species Name"
                :sortable true
                :resizable true
                :filter "agTextColumnFilter"
                :floatingFilter true}
               {:field "abundance_2_fraction_total_reads"
                :maxWidth 120
                :headerName "Abundance (%)"
                :sortable true
                :resizable true
                :filter "agNumberColumnFilter"
                :type "numericColumn"
                :floatingFilter true}]}
   {:headerName "3rd Most Abundant Species"
    :children [{:field "abundance_3_name"
                :maxWidth 140
                :headerName "Species Name"
                :sortable true
                :resizable true
                :filter "agTextColumnFilter"
                :floatingFilter true}
               {:field "abundance_3_fraction_total_reads"
                :maxWidth 120
                :headerName "Abundance (%)"
                :sortable true
                :resizable true
                :filter "agNumberColumnFilter"
                :type "numericColumn"
                :floatingFilter true}]}
   {:headerName "4th Most Abundant Species"
    :children [{:field "abundance_4_name"
                :maxWidth 140
                :headerName "Species Name"
                :sortable true
                :resizable true
                :filter "agTextColumnFilter"
                :floatingFilter true}
               {:field "abundance_4_fraction_total_reads"
                :maxWidth 120
                :headerName "Abundance (%)"
                :sortable true
                :resizable true
                :filter "agNumberColumnFilter"
                :type "numericColumn"
                :floatingFilter true}]}
   {:headerName "5th Most Abundant Species"
    :children [{:field "abundance_5_name"
                :maxWidth 140
                :headerName "Species Name"
                :sortable true
                :resizable true
                :filter "agTextColumnFilter"
                :floatingFilter true}
               {:field "abundance_5_fraction_total_reads"
                :maxWidth 120
                :headerName "Abundance (%)"
                :sortable true
                :resizable true
                :filter "agNumberColumnFilter"
                :type "numericColumn"
                :floatingFilter true}]}])

;; Species Abundance Table Component

(defn library-species-abundance-table
  "Component for displaying species abundance data."
  []
  (let [grid-ref (clj->js {:current nil})
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
      [grid/ag-grid-react
       {:ref grid-ref
        :columnDefs library-species-abundance-column-defs
        :rowData row-data
        :getRowId (fn [params]
                    (let [^js data (.-data params)]
                      (.-library_id data)))
        :theme "legacy"
        :pagination false
        :enableCellTextSelection true
        :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
        :onSelectionChanged #()}
       ]]
     [:div {:style {:grid-row "2"}}
      [:button {:onClick #(export-library-species-abundance-table grid-ref currently-selected-run-id)} "Export CSV"]]]))