(ns routine-sequence-qc.grid
  (:require [reagent.core :as r]
            [ag-grid-react :refer [AgGridReact]]))

(def ag-grid-react (r/adapt-react-class AgGridReact))

(defn get-selected-rows
  "Function to get the selected rows from the runs table."
  [e]
  (map #(js->clj (.-data %) :keywordize-keys true)
       (-> e
           .-api
           .getSelectedNodes)))

(defn get-cell-data
  "Take a JS cell object and convert to clj data"
  [row]
  (js->clj (.-data row) :keywordize-keys true))

(defn column-defs
  "Build AG-Grid columnDefs from an ordered `headers` vector.
   Optionally accepts `style-fn` and `tooltip-fn` to attach as cellStyle /
   tooltipValueGetter on every column."
  ([headers] (column-defs headers nil nil))
  ([headers style-fn tooltip-fn]
   (when (seq headers)
     (mapv (fn [k]
             (cond-> {:field k :headerName k}
               style-fn   (assoc :cellStyle style-fn)
               tooltip-fn (assoc :tooltipValueGetter tooltip-fn)))
           headers))))


(defn data-grid
  "Render an AG-Grid table from `rows`, a vector of string-key maps.
   Options:
     :headers           — ordered vector of column keys (defaults to (keys (first rows)))
     :height            — CSS height string (default \"50vh\")
     :theme             — AG-Grid theme class name (default \"ag-theme-balham\")
     :validation-errors — result of validate/validate-rows; cells with errors are shaded red"
  [rows & {:keys [column-defs height]
           :or   {height "50vh"
                  theme  "ag-theme-balham"}}]
  (let [indexed-rows  (map-indexed (fn [i row] (assoc row "__row_index" i)) rows)
        col-defs-js   (clj->js column-defs)]
    (if (or (seq rows) (seq column-defs))
      [:div {:class "ag-theme-balham"
             :style {:height height :width "100%"}}
       [ag-grid-react
        {:rowData    indexed-rows
         :enableCellTextSelection true
         :ensureDomOrder true
         :columnDefs col-defs-js
         :theme      "legacy"
         :tooltipShowDelay 0
         :tooltipSwitchShowDelay 200
         :tooltipHideDelay 2000
         :defaultColDef {:sortable  false
                         :filter    true
                         :resizable true
                         :minWidth  80}}]]
      [:div.grid-empty "No data"])))