(ns leiningen.ns-dep-graph
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.tools.namespace.file :as ns-file]
            [clojure.tools.namespace.track :as ns-track]
            [clojure.tools.namespace.find :as ns-find]
            [clojure.tools.namespace.dependency :as ns-dep]
            [rhizome.dot :as dot]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]
            [rhizome.viz :as viz]))


(def cli-options
  [[nil "--remove-tests" "Remove all *test* namespaces"
    :default false]
   ["-f" "--filename FILENAME" "Output filename"
    :default "ns-dep-graph"]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["This is my program. There are many like it, but this one is mine."
        ""
        "Usage: program-name [options] action"
        ""
        "Options:"
        options-summary
        ""
        "Output formats:"
        "  png     Output a png file"
        "  dot     Output a dot file"
        ""
        "Please refer to the manual page for more information."]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))


(defn ns-dep-graph
  "Create a namespace dependency graph and save it as ns-dep-graph.png."
  {:help-arglists '([png dot])
   :subtasks '[#'png #'dot]}
  ([project]
   (ns-dep-graph project :png))
  ( [project subtask & args]
    (let [source-files (apply set/union
                              (map (comp ns-find/find-clojure-sources-in-dir
                                         io/file)
                                   (project :source-paths)))
          tracker (ns-file/add-files {} source-files)
          dep-graph (tracker ::ns-track/deps)
          ns-names (set (map (comp second ns-file/read-file-ns-decl)
                             source-files))

          part-of-project? (partial contains? ns-names)
          nodes (filter part-of-project? (ns-dep/nodes dep-graph))]
      (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
       (condp = (keyword (name subtask))
         :dot
         (with-open [w (io/writer (str (:filename options) ".dot"))]
           (.write w (dot/graph->dot
             nodes
             #(filter part-of-project? (ns-dep/immediate-dependencies dep-graph %))
             :node->descriptor (fn [x] {:label x})
             :directed? true)))
         :png (viz/save-graph
               nodes
               #(filter part-of-project? (ns-dep/immediate-dependencies dep-graph %))
               :node->descriptor (fn [x] {:label x})
               :options {:dpi 72}
               :filename (str (:filename options) ".png")))))))

;; TODO: make output filename configurable.

;; TODO: do not overwrite existing PNG file.

;; TODO: maybe add option to show dependencies on external namespaces as well.
