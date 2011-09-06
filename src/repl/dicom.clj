(ns #^{:doc "Manipulate DICOM files"
       :author "Kevin A. Archie <karchie@wustl.edu>"}
  nrg.dicom
  (:import (java.io
	    BufferedInputStream
	    File
	    FileInputStream
	    InputStream
	    IOException)
	   (java.util.zip GZIPInputStream)
	   (org.dcm4che2.data
	    DicomObject
	    Tag
	    UID)
	   (org.dcm4che2.io
	    DicomInputStream
	    DicomOutputStream
	    StopTagInputHandler))
  (:require [clojure.string :as string]))


(def
 #^{:private true
    :doc "Files with this suffix are assumed to be gzip compressed."}
 gzip-suffix ".gz")

(defn- make-caused-IOException
  "Creates an IOException with the given cause."
  ([cause]
     (let [e (IOException.)]
       (.initCause e cause)
       e))
  ([cause message]
     (let [e (IOException. message)]
       (.initCause e cause)
       e)))

(defn stream->dicom-object
  "Reads a DICOM object from an InputStream."
  ([in-s max-tag]
     (with-open
	 [buf-in-s (BufferedInputStream. in-s)
	  dicom-in-s (DicomInputStream. buf-in-s)]
       (when-not (nil? max-tag)
	 (.setHandler dicom-in-s (StopTagInputHandler.
				  (inc (max max-tag Tag/SOPClassUID)))))
       (try
	(let [dicom-o (.readDicomObject dicom-in-s)]
	  (cond (.contains dicom-o Tag/FileMetaInformationVersion)
		dicom-o			; looks part 10 compliant
		(.contains dicom-o Tag/SOPClassUID)
		dicom-o			; has SOP Class UID
		:else
		(throw (IOException. "not a valid DICOM object"))))
	(catch IOException e (throw e))
	(catch Throwable e
	  (throw (make-caused-IOException
		  e "Not a DICOM file, or an error occurred"))))))
  ([in-s] (stream->dicom-object in-s nil)))

(defn file->dicom-object
  "Reads a DICOM object from a file; f may be a File or String
filename. If the filename ends in .gz, assumes it is gzip compressed
and uncompresses the contents inline."
  ([f max-tag]
     (with-open [in-s (FileInputStream. f)]
       (let [name (if (instance? File f) (.getName f) f)]
	 (if (.endsWith name gzip-suffix)
	   (with-open [in-gzs (GZIPInputStream. in-s)]
	     (stream->dicom-object in-gzs max-tag))
	   (stream->dicom-object in-s max-tag)))))
  ([f] (file->dicom-object f nil)))

(defn obj-seq
  "Generates a lazy sequence of [File, DICOM object] vectors
from a sequence of files."
  ([fs max-tag]
     (if (seq fs)
       (lazy-seq
	(let [f (first fs)]
	  (try (cons [f (file->dicom-object f max-tag)]
		     (obj-seq (rest fs) max-tag))
	       (catch Throwable t
		 (obj-seq (rest fs) max-tag)))))
       '()))
  ([fs] (obj-seq fs nil)))

(defn make-headerless-copy
  "Make headerless copies of all the given DICOM files (or all DICOM
files in the given directory)"
  [files to-path]
  (let [to-dir (File. to-path)]
    (.mkdir to-dir)
    (doseq [[f o] (obj-seq files)
	    :let [name (string/replace (.getName f) #"\.gz$" "")
		  of (File. to-dir name)]]
      (with-open [out-s (DicomOutputStream. of)]
	(.writeDataset out-s (.dataset o)
		       (.getString o Tag/TransferSyntaxUID
				   UID/ImplicitVRLittleEndian))))))
