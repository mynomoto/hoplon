(defproject tailrecursion/hoplon "1.1.0-SNAPSHOT"
  :description        "Hoplon web development environment."
  :url                "http://github.com/tailrecursion/hoplon"
  :license            {:name "Eclipse Public License"
                       :url "http://www.eclipse.org/legal/epl-v10.html"}
  :manifest           {"hoplon-provides"            "hoplon"}
  :dependencies       [[tailrecursion/javelin       "2.0.0"]
                       [tailrecursion/boot.task     "0.1.0-SNAPSHOT"]
                       [tailrecursion/castra        "0.1.0-SNAPSHOT"]
                       ;; for fipp, jdk 1.6 compat
                       [org.codehaus.jsr166-mirror/jsr166y "1.7.0"]
                       [fipp                        "0.3.0-SNAPSHOT"]
                       [digest                      "1.3.0"]
                       [criterium                   "0.3.0"]
                       [clj-tagsoup                 "0.3.0"]
                       [org.clojure/core.incubator  "0.1.2"]])
