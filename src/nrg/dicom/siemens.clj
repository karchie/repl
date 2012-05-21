(ns #^{:doc "Siemens vendor-private DICOM fields"
       :author "Kevin A. Archie <karchie@wustl.edu>"}
  nrg.dicom.siemens
  (:import (java.nio ByteBuffer))
  (:use nrg.dicom))

(defprotocol Bytes
  (get-le-int [this offset])
  (get-strz [this offset & {:keys [length]}]))

(extend-type ByteBuffer
  Bytes
  (get-le-int [this offset]
    (Integer/reverseBytes (.getInt this offset)))

  (get-strz [this offset & {:keys [length]}]
    (get-strz (.array this) offset :length length)))

(extend-type (class (make-array Byte/TYPE 0))
  Bytes
  (get-le-int [this offset]
    (get-le-int (ByteBuffer/wrap this) offset))

  (get-strz [this offset & {:keys [length]}]
    (loop [i offset]
      (let [b (aget this i)]
        (if (or (zero? (aget this i))
                (and length (>= (- i offset) length)))
          [(String. this offset (- i offset))
           (cond
            length (+ offset length)
            (even? i) (+ 2 i)
            :else (inc i))]
          (recur (inc i)))))))

(defn- parse-element-items [bytes n offset]
  (loop [i 1
         offset offset
         data []]
    (let [field-width (-> (get-le-int bytes (+ 12 offset))
                          (/ 4)
                          (Math/ceil)
                          (* 4)
                          (int))
          [content next-offset] (get-strz bytes (+ 16 offset)
                                          :length field-width)]
      (cond
       (< i n) (recur (inc i) next-offset (conj data content))
       data [(conj data content) next-offset]
       :else [content next-offset]))))
  
(defn- parse-element
  [bytes offset]
  (let [buf (ByteBuffer/wrap bytes)
        [field-name vm-offset] (get-strz bytes offset :length 64)
        num-items (get-le-int buf (+ 12 vm-offset))
        vm (let [vm (get-le-int buf vm-offset)]
             (if (zero? vm) num-items vm))
        [vr _] (get-strz bytes (+ 4 vm-offset) :length 4)
        items-offset (+ vm-offset 20)
        [content junk-offset]
        (if (zero? num-items)
          [nil items-offset]
          (parse-element-items bytes vm items-offset))
        junk-len (cond
                  (zero? num-items) 0
                  (< num-items vm) 16
                  :else (* 16 (- num-items vm)))
        next-offset (+ junk-offset junk-len)]
    [field-name vr vm content next-offset]))


(defn parse-shadow
  [bytes]
  {:pre [(= "SV10" (String. bytes 0 4))]}
  (loop [num-elem (get-le-int bytes 8)
         offset 16
         m {}]
    (let [[field-name vr vm content offset] (parse-element bytes
                                                           offset)
          m (assoc m field-name [vr vm content])]
      (if (> num-elem 1)
        (recur (dec num-elem) offset m)
        m))))
