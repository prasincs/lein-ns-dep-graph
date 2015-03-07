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
   ["-o" "--output FILENAME" "Output filename"
    :default "ns-dep-graph"]
   [nil "--filter-ns REGEX"
    "Regex for filtering on certain namespaces"
    :default nil]
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
      (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)
            filter-namespace-fn (partial re-find (re-pattern (:filter-ns options)))
            filtered-nodes (if-not (nil? (:filter-ns options))
                             (set (filter filter-namespace-fn (map str nodes)))
                             nodes)
            filename (condp = (keyword (name subtask))
                       :dot (str (:output options) ".dot")
                       :png (str (:output options) ".png"))]
        (if (.exists (io/as-file filename ))
          (exit 0  (str filename " already exists !"))
          (condp = (keyword (name subtask))
            :dot
            (with-open [w (io/writer filename)]
              (.write w (dot/graph->dot
                         filtered-nodes
                         #(filter part-of-project? (ns-dep/immediate-dependencies dep-graph %))
                         :node->descriptor (fn [x] {:label x})
                         :directed? true)))
            :png (viz/save-graph
                  filtered-nodes
                  #(filter part-of-project? (ns-dep/immediate-dependencies dep-graph %))
                  :node->descriptor (fn [x] {:label x})
                  :options {:dpi 72}
                  :filename filename)))))))


;; TODO: maybe add option to show dependencies on external namespaces as well.
