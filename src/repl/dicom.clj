(ns #^{:doc "Manipulate DICOM files"
       :author "Kevin A. Archie <karchie@wustl.edu>"}
  nrg.dicom
  (:import (java.io BufferedInputStream
		    File
		    FileInputStream
		    InputStream
		    IOException)
           (java.util.concurrent Executors)
	   (java.util.zip GZIPInputStream)
	   (org.dcm4che2.data DicomObject
			      Tag
			      UID
                              VR)
	   (org.dcm4che2.io DicomInputStream
			    DicomOutputStream
			    StopTagInputHandler)
           (org.dcm4che2.net Device
                             NetworkApplicationEntity
                             NetworkConnection
                             TransferCapability))
  (:require [clojure.string :as string]
            [clojure.set])
  (:use clojure.test))

(def
  #^{:private true
     :doc "Files with this suffix are assumed to be gzip compressed."}
  gzip-suffix ".gz")

(defn- make-caused-IOException
  "Creates an IOException with the given cause."
  ([cause]
     (doto (IOException.)
       (.initCause cause)))
  ([cause message]
     (doto (IOException. message)
       (.initCause cause))))

(deftest make-caused-IOException-test
  (let [cause (Exception.)]
    (is (nil? (.getCause (IOException.))))
    (is (= cause
	   (.getCause (make-caused-IOException cause))))
    (is (= cause
	   (.getCause (make-caused-IOException cause "oh no!"))))))

(defn read-stream
  "Reads a DICOM object from an InputStream."
  ([in-s max-tag]
     (io!
      (with-open
          [buf-in-s (BufferedInputStream. in-s)
           dicom-in-s (DicomInputStream. buf-in-s)]
        (when max-tag
          (.setHandler dicom-in-s
                       (StopTagInputHandler.
                        (inc (max max-tag Tag/SOPClassUID)))))
        (try
          (let [obj (.readDicomObject dicom-in-s)]
            (when-not (or (.contains obj Tag/FileMetaInformationVersion)
                          (.contains obj Tag/SOPClassUID))
              (throw (IOException. "not a valid DICOM object")))
            obj)
          (catch IOException e (throw e))
          (catch Throwable e
            (throw (make-caused-IOException
                    e "Not a DICOM file, or an error occurred")))))))
  ([in-s] (read-stream in-s nil)))

(defn read-file
  "Reads a DICOM object from a file; f may be a File or String
filename. If the filename ends in .gz, assumes it is gzip compressed
and uncompresses the contents inline."
  ([f max-tag]
     (io!
      (with-open [in-s (FileInputStream. f)]
        (let [name (if (instance? File f) (.getName f) f)]
          (if (.endsWith name gzip-suffix)
            (with-open [in-gzs (GZIPInputStream. in-s)]
              (read-stream in-gzs max-tag))
            (read-stream in-s max-tag))))))
  ([f] (read-file f nil)))

(defn obj-seq
  "Generates a lazy sequence of [File, DICOM object] vectors
from a sequence of files."
  ([fs max-tag]
     (if (seq fs)
       (lazy-seq
	(let [f (first fs)
              file (if (instance? File f) f (File. f))]
	  (try (cons [file (read-file file max-tag)]
		     (obj-seq (rest fs) max-tag))
	       (catch Throwable t
		 (obj-seq (rest fs) max-tag)))))
       '()))
  ([fs] (obj-seq fs nil)))

(def
  #^{:private true
     :doc "Dummy DicomObject for getting tag names"}
  empty-dicom-object (org.dcm4che2.data.BasicDicomObject.))

(defn tag-name [tag]
  "Returns the name of the provided tag."
  (.nameOf empty-dicom-object tag))
   
(defn make-headerless-copy!
  "Make headerless copy of the given DICOM files (paths or Files)
into the provided directory."
  [to-dir & files]
  (io!
   (.mkdir to-dir)
   (doseq [[f o] (obj-seq files)
           :let [name (string/replace (.getName f) #"\.gz$" "")
                 of (File. to-dir name)]]
     (with-open [out-s (DicomOutputStream. of)]
       (.writeDataset out-s (.dataset o)
                      (.getString o Tag/TransferSyntaxUID
                                  UID/ImplicitVRLittleEndian))))))

(def ^{:private true} verification-scu-transfer-capability
  (TransferCapability. UID/VerificationSOPClass
                       (into-array [UID/ImplicitVRLittleEndian])
                       TransferCapability/SCU))

(defn verify-service
  "Use C-ECHO to ping an SCP n times. Returns a sequence of length
n, representing the time in ms of each C-ECHO operation."
  [scp-ae-title & {:keys [count
                          device-name
                          hostname
                          interval
                          port
                          scu-ae-title]
                   :or {count 1
                        device-name "cljecho"
                        hostname "localhost"
                        interval 0
                        port 104
                        scu-ae-title "dcm4clj"}}]
  (let [nc (NetworkConnection.)
        ae (doto (NetworkApplicationEntity.)
             (.setNetworkConnection nc)
             (.setAssociationInitiator true)
             (.setAETitle scu-ae-title)
             (.setTransferCapability
              (into-array [verification-scu-transfer-capability])))
        remote-nc (doto (NetworkConnection.)
                    (.setHostname hostname)
                    (.setPort port))
        remote-ae (doto (NetworkApplicationEntity.)
                    (.setAETitle scp-ae-title)
                    (.setInstalled true)
                    (.setAssociationAcceptor true)
                    (.setNetworkConnection remote-nc))
        device (doto (Device. device-name)
                 (.setNetworkApplicationEntity ae)
                 (.setNetworkConnection nc))
        executor-service (Executors/newCachedThreadPool)
        assoc (.connect ae remote-ae executor-service)]
    (try
      (doall
       (for [i (range count)
             :let [t (System/currentTimeMillis)]]
         (do 
           (Thread/sleep interval)
           (.. assoc (cecho) (next))
           (- (System/currentTimeMillis) t))))
      (finally
       (.release assoc true)))))


(declare to-map)

(defn to-seq [dicom-element specific-charset cache?]
  "Extract the value of the provided DicomElement into a sequence
of suitable JVM representations."
  (let [vr (.vr dicom-element)]
    (seq (condp contains? vr
           #{VR/AE VR/AS VR/CS VR/DS VR/IS VR/LO VR/LT VR/PN VR/SH
             VR/ST VR/UI VR/UT}
           (.getStrings dicom-element specific-charset cache?)
             
           #{VR/AT VR/SL VR/SS VR/UL VR/US}
           (.getInts dicom-element cache?)
               
           #{VR/DA VR/DT VR/TM}
           (.getDates dicom-element cache?)

           #{VR/FL VR/OF}
           (.getFloats dicom-element cache?)

           #{VR/FD}
           (.getDoubles dicom-element cache?)
           
           #{VR/OB VR/OW VR/UN}
           (.getBytes dicom-element)
           
           #{VR/SQ} (map to-map
                      (for [i (range (.countItems dicom-element))]
                        (.getDicomObject dicom-element i)))))))

                                     
(defn to-map [dicom-object & {:keys [cache-elems]
                              :or {cache-elems false}}]
  "Turn the provided DicomObject into a map. Any contained DICOM
sequences will be converted to sequences, possibly containing nested
maps."
  (let [specific-charset (.getSpecificCharacterSet dicom-object)]
    (into {} (map #(vector (.tag %)
                           (to-seq % specific-charset cache-elems))
                  (iterator-seq (.iterator dicom-object))))))
  

(defn push-to-instance [m k from-vals-key]
  "Push the provided attribute down to the instance level and clear
its value from the provided sequence of value maps."
  {:pre [(or (= :study-vals from-vals-key)
             (= :series-vals from-vals-key))]}
  (alter (m :level) assoc k :instance)
  (doseq [from-map (vals @(m from-vals-key))]
    (alter from-map dissoc k)))

(defn set-value-in-series [m series k v]
  "Sets a key-value mapping for a series, creating a value map for that
series or altering it if it already exists."
  (if-let [series-vals (@(m :series-vals) series)]
    (alter series-vals assoc k v)
    (alter (m :series-vals) assoc series (ref {k v}))))

(defn push-from-study [m new-v-series k new-v]
  "The provided attribute k, previously at the study level, has a
different value in the current object than has previously been seen in
the current study, so push it down to series. If a previously inserted
object is in the current series (as determined by checking
series-study), then this series has multiple values for k so k is
actually at the instance level."
  {:pre [(= (@(m :level) k) :study)]}
  (if (@(m :series-study) new-v-series)
    (push-to-instance m k :study-vals)
    (dosync
     (alter (m :level) assoc k :series)
     (doseq [[to-series study] @(m :series-study)
             :let [old-v (@(@(m :study-vals) study) k)]]
       (set-value-in-series m to-series k old-v))
     (set-value-in-series m new-v-series k new-v)
     (doseq [study-vals (vals @(m :study-vals))]
       (alter study-vals dissoc k)))))

(defn push-from-series [m k]
  (push-to-instance m k :series-vals))

(defmacro insert-kv-level [m k v prev-v level level-key
                           & push-down-expr]
  {:pre [(or (= (name level) "study") (= (name level) "series"))] }
  (let [vals-key (keyword (str (name level) "-vals"))]
    `(if (contains? @(~m ~vals-key) ~level-key)
       (if-let [~prev-v (@(@(~m ~vals-key) ~level-key) ~k)]
         (when-not (= ~v ~prev-v)
           ~@push-down-expr)
         (alter (@(~m ~vals-key) ~level-key) assoc ~k ~v))
       (alter (~m ~vals-key) assoc ~level-key (ref {~k ~v})))))

(defn insert-kv [m study series k v]
  (case (@(m :level) k)
    :study (insert-kv-level m k v prev-v :study study
                            (push-from-study m series k v))
    :series (insert-kv-level m k v prev-v :series series
                             (push-from-series m k))
    :instance true
    nil (do
          (alter (m :level) assoc k :study)
          (insert-kv m study series k v))))

(defn into-summary [summary dcmo-map]
  {:pre [(= 1 (count (dcmo-map Tag/StudyInstanceUID)))
         (= 1 (count (dcmo-map Tag/SeriesInstanceUID)))
         (= 1 (count (dcmo-map Tag/SOPInstanceUID)))]}
  (let [study-uid (first (dcmo-map Tag/StudyInstanceUID))
        series-uid (first (dcmo-map Tag/SeriesInstanceUID))]
    (doseq [[k v] dcmo-map]
      (dosync
       (insert-kv summary study-uid series-uid k v)))
    (doseq [k (clojure.set/difference
               (set (keys @(@(summary :study-vals) study-uid)))
               (set (keys dcmo-map)))]
      (push-from-study summary series-uid k @(summary :study-vals)))
    (dosync
     (alter (summary :series-study) assoc series-uid study-uid)))
  summary)

(defn summarize-attribute-levels [& roots]
  (let [summary {:level (ref {})
                 :study-vals (ref {})
                 :series-vals (ref {})
                 :series-study (ref {})}]
    (doseq [root roots
            [f dcmo] (obj-seq (filter #(.isFile %)
                                      (file-seq (File. root)))
                              (dec Tag/PixelData))]
      (into-summary summary (to-map dcmo)))
    summary))
