(ns cnrepl.config
  "Server configuration utilities.
  Some server options can be configured via configuration
  files (local or global).  This namespace provides
  convenient API to work with them.
  The config resolution algorithm is the following:
  The global config file .nrepl/nrepl.edn is merged with
  any local config file (.nrepl.edn) if present.
  The values in the local config file take precedence."
  {:author "Bozhidar Batsov"
   :added  "0.5"}
  (:require
   #?(:clj [clojure.java.io :as io]
      :cljr [clojure.clr.io :as io])
   [clojure.edn :as edn]))

(def ^:private home-dir
  "The user's home directory."
  #?(:clj (System/getProperty "user.home")
     :cljr (Environment/GetEnvironmentVariable "USERPROFILE")))

(def config-dir
  "nREPL's configuration directory.
  By default it's ~/.nrepl, but this can be overridden
  with the NREPL_CONFIG_DIR env variable."
  #?(:clj
     (or (System/getenv "NREPL_CONFIG_DIR")
         (str home-dir java.io.File/separator ".nrepl"))
     :cljr
     (or (Environment/GetEnvironmentVariable "NREPL_CONFIG_DIR")
         (Environment/GetEnvironmentVariable "nrepl.config.dir")
         (str home-dir System.IO.Path/PathSeparator ".nrepl"))))

(def config-file
  "nREPL's config file."
  #?(:clj  (str config-dir java.io.File/separator "nrepl.edn")
     :cljr (str config-dir System.IO.Path/PathSeparator "nrepl.edn")))

(defn- load-edn
  "Load edn from an io/reader source (filename or io/resource)."
  [source]
  #?(:clj
     (with-open [r (io/reader source)]
       (edn/read (java.io.PushbackReader. r)))
     :cljr
     (with-open [r (io/text-reader source)]
       (edn/read (clojure.lang.PushbackTextReader. r)))))

(defn- load-config
  "Load the configuration file identified by `filename`.
  Return its contents as EDN if the file exists,
  or an empty map otherwise."
  [filename]
  (let [file #?(:clj (io/file filename)
                :cljr (System.IO.FileInfo. filename))]
    (if #?(:clj (.exists file) :cljr (.Exists file))
      (load-edn file)
      {})))

(def config
  "Configuration map.
  It's created by merging the global configuration file
  with a local configuration file that would normally
  the placed in the directory in which you're running
  nREPL."
  (merge
   (load-config config-file)
   (load-config ".nrepl.edn")))
