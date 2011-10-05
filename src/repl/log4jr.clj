(ns #^{:doc "Test log4jr server"
       :author "Kevin A. Archie <karchie@wustl.edu>"}
  nrg.log4jr
  (:import (org.nrg.log4j PostAppender)
	   (org.apache.log4j.spi LoggingEvent)
	   (org.apache.log4j Logger Level)))

(defn test-server [url & {:keys [message logger throwable]
			  :or {message "Hi there"
			       logger "org.nrg.clj.repl"
			       throwable nil}}]
  "Tests a remote log4jr server by sending it an event."
  (let [logev (LoggingEvent. logger
			     (Logger/getLogger logger)
			     Level/INFO
			     message
			     throwable)]
    (doto (PostAppender.)
      (.setURL url)
      (.append logev))))
