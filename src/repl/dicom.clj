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
			      UID)
	   (org.dcm4che2.io DicomInputStream
			    DicomOutputStream
			    StopTagInputHandler)
           (org.dcm4che2.net Device
                             NetworkApplicationEntity
                             NetworkConnection
                             TransferCapability))
  (:require [clojure.string :as string])
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
          (doto (.readDicomObject dicom-in-s)
            (when-not (or (.contains Tag/FileMetaInformationVersion)
                          (.contains Tag/SOPClassUID))
              (throw (IOException. "not a valid DICOM object"))))
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
