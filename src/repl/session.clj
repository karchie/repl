;;; Copyright (c) 2010 Washington University

;; example :
;; (nrg.session/rename-walk
;;   (java.io.File. "/data/nil-bluearc/marcus/Rob_Anybody")
;;   ["Rob_Anybody" :modality :study-desc :series :instance
;;     :year :month :day :hour :minute :second :msec :uid-frag :suffix]
;;   "."
;;   nrg.session/file-has-dcm-suffix)
;;
;; replaces the first component of DicomServer-formatted names with
;; the given string literal ("Rob_Anybody")

(ns #^{:doc "Rename DICOM files in an XNAT session directory"
       :author "Kevin A. Archie <karchie@wustl.edu>"}
  nrg.session
  (:import (java.io File))
  (:use [clojure.contrib.string :only (join)]))

(defn extract-name-fields
  [filename]
  (zipmap [
					;          :patient-name
	   :dummy
	   :modality :study-desc
	   :series :instance
					;	   :year :month :day
					;	   :hour :minute :second :msec
	   :date 
	   :time
	   :msec
	   :uid-frag
	   :suffix
	   :gz]
	  (re-seq #"[\w\-]+" filename)))
	
(defn new-name
  [filename format-coll separator]
  (let [fields (extract-name-fields filename)]
    (join separator (map #(get fields % %) format-coll))))

(defn rename-walk
  [root format-coll separator should-rename?]
  (doseq [f (file-seq root)]
    (when (should-rename? f)
      (let [name (new-name (.getName f) format-coll separator)
	    file (File. (.getParentFile f) name)]
	(println "Renaming" f "to" file)
	(.renameTo f file)))))

(defn file-has-suffix-fn
  [suffix]
  (let [suffix-re (re-pattern (str "(?i)" suffix))]
    (fn [f]
      (and (.isFile f)
	   (re-find suffix-re (.getName f))))))

(def file-has-ima-suffix (file-has-suffix-fn "\\.ima"))
(def file-has-dcm-suffix (file-has-suffix-fn "\\.dcm"))
(def file-has-dcmgz-suffix (file-has-suffix-fn "dcm.gz"))
