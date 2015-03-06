# lein-ns-dep-graph

This is a Leiningen plugin to show the namespace dependencies of Clojure project
sources as a graph. This is a fork of the original `lein-ns-dep-graph` 


## Added features

- generates dot files

## Acknowledgements

The plugin itself is tiny, all the hard work is done by
[clojure.tools.namespace](https://github.com/clojure/tools.namespace) and
[Rhizome](https://github.com/ztellman/rhizome).

## Requirements

You will need to have [Graphviz](http://www.graphviz.org/) installed. Run `dot
-V` at the command line to check.

## Installation and Usage

Put `[lein-ns-dep-graph "0.1.0-SNAPSHOT"]` into the `:plugins` vector of your
`:user` profile, or if you are on Leiningen 1.x do `lein plugin install
lein-ns-dep-graph 0.1.0-SNAPSHOT`. Then run

    lein ns-dep-graph

from a Clojure project directory. This outputs a file `ns-dep-graph.png` showing
the internal namespace dependencies of the project's `.clj` sources.
Dependencies on external namespaces, say `clojure.java.io`, are not shown.

## Examples

Below is the namespace dependency graph obtained for
[Hiccup](https://github.com/weavejester/hiccup).

![Hiccup namespace dependency graph](http://hilverd.github.com/lein-ns-dep-graph/img/hiccup.png)

## License

Copyright © 2013 Hilverd Reker.

Distributed under the Eclipse Public License, the same as Clojure.
