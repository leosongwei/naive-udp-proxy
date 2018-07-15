(ql:quickload 'usocket)
(ql:quickload 'cl-base64)

(defun split-string (string split-char)
  (let* ((result nil)
         (cursor 0))
    (dotimes (i (length string))
      (let ((char (aref string i)))
        (if (equal char split-char)
            (progn (push (subseq string cursor i) result)
                   (setf cursor (1+ i))))))
    (push (subseq string cursor (length string)) result)
    (reverse result)))
;;(split-string "  " #\space)
;;(split-string "127.0.0.1" #\.)

(defun parse-ip-string (ip-string) ;; danger
  (let* ((bytes (mapcar 'parse-integer (split-string ip-string #\.))))
    (make-array 4 :element-type '(unsigned-byte 8)
                :initial-contents `(,(nth 0 bytes) ,(nth 1 bytes)
                                     ,(nth 2 bytes) ,(nth 3 bytes)))))

(defun parse-message (message)
  "parse message line without the newline char"
  (handler-case
      (block :parsing
        (let ((parts (split-string message #\,)))
          (when (not (= 3 (length parts)))
            (return-from :parsing))
          (let* ((ip-string (nth 0 parts))
                 (ip-bytes (split-string ip-string #\.)))
            (when (not (= 4 (length ip-bytes)))
              (return-from :parsing))
            (dolist (byte-string ip-bytes)
              (when (not (and (<= (parse-integer byte-string) 255)
                              (>= (parse-integer byte-string) 0)))
                (return-from :parsing)))
            (let* ((port-string (nth 1 parts))
                   (port-number (parse-integer port-string))
                   (buffer (base64:base64-string-to-usb8-array (nth 2 parts))))
              (when (not (and (<= port-number 65535)
                              (>= port-number 0)))
                (return-from :parsing))
              (values ip-string port-number buffer)))))
    (condition () nil)))
;;(parse-message "127.0.0.1,98847,ZnNkYXdNhZmQK")

;; (defparameter *udp-socket* (usocket:socket-connect nil nil
;;                                                    :protocol :datagram
;;                                                    :local-host "127.0.0.1"
;;                                                    :local-port 6666))
;; (usocket:socket-receive *udp-socket* nil 200)
;; (defparameter *udp-socket* (usocket:socket-connect "127.0.0.1" 12345
;;                                                    :protocol :datagram))
;; (usocket:socket-send *udp-socket*
;;                      (make-array 5 :initial-contents '(49 50 51 10 0)
;;                                  :element-type '(unsigned-byte 8))
;;                      5)
;; (base64:base64-string-to-usb8-array (base64:usb8-array-to-base64-string #(255 255 255)))
;; (base64:usb8-array-to-base64-string (subseq #(255 255 255 0 0) 0 3))

(defparameter *stdout* *standard-output*)

(defparameter *client-udp-ip* "127.0.0.1")
(defparameter *client-udp-port* 7878)

(defparameter *server-tcp-ip* "127.0.0.1")
(defparameter *server-tcp-port* 8787)

(defparameter *server-udp-ip* "127.0.0.1")
(defparameter *server-udp-port* 9999)


(defparameter *client-tcp-socket* nil)
(defparameter *tcp-stream* nil)
(defparameter *client-udp-socket* nil)
(defparameter *client-sending-thread* nil)

(defun udp-2-message ()
  (multiple-value-bind (buffer size user-ip user-port)
      (usocket:socket-receive *client-udp-socket* nil 65535)
    (let* ((user-ip-string (format nil "~A.~A.~A.~A"
                                   (aref user-ip 0) (aref user-ip 1)
                                   (aref user-ip 2) (aref user-ip 3))))
      (format *tcp-stream* "~A,~A,~A~%"
              user-ip-string
              user-port
              (base64:usb8-array-to-base64-string (subseq buffer 0 size)))
      (force-output *tcp-stream*))))



(defun message-2-udp ()
  (let ((line (read-line *tcp-stream*)))
    (format *stdout* "~A~%" line)
    (multiple-value-bind (ip-string port buffer)
        (parse-message line)
      (if ip-string
          (usocket:socket-send *client-udp-socket* buffer (length buffer)
                               :port port
                               :host (parse-ip-string ip-string))))))

(defun clear-client-side ()
  (when (and (typep *client-sending-thread* 'sb-thread:thread)
             (sb-thread:thread-alive-p *client-sending-thread*))
    (sb-thread:terminate-thread *client-sending-thread*))
  (when (typep *client-udp-socket* 'usocket:datagram-usocket)
    (usocket:socket-close *client-udp-socket*)))


(defun client-side (server-ip server-port client-udp-ip client-port)
  (defparameter *client-tcp-socket* (usocket:socket-connect server-ip server-port))
  (defparameter *tcp-stream* (usocket:socket-stream *client-tcp-socket*))
  (defparameter *client-udp-socket* (usocket:socket-connect
                                     nil nil
                                     :protocol :datagram
                                     :local-host client-udp-ip
                                     :local-port client-port))
  (defparameter *client-sending-thread*
    (sb-thread:make-thread (lambda () (loop (message-2-udp)))))
  (loop (udp-2-message)))

(defun use-client-side ()
  (clear-client-side)
  (client-side *server-tcp-ip* *server-tcp-port*
               *client-udp-ip* *client-udp-port*))

;; --------------------------------------------------
;; server side

(defparameter *server-tcp-socket* nil)
(defparameter *server-stream* nil)
(defparameter *server-connection* nil)

(defparameter *sport-user* (make-hash-table))
(defparameter *user-sport* (make-hash-table))
(defparameter *sport-udp* (make-hash-table))
;;(defparameter *server-udp-list* nil)

(defun calculate-user-id (ip-string port)
  (let ((ip-bytes (parse-ip-string ip-string)))
    (+ (ash (aref ip-bytes 0) 0)
       (ash (aref ip-bytes 1) 8)
       (ash (aref ip-bytes 2) 16)
       (ash (aref ip-bytes 3) 24)
       (ash port 32))))

(defun recover-user-id (id)
  (let* ((a (make-array 4 :element-type '(unsigned-byte 8)))
         (port (ash id -32))
         (sum (ash port 32)))
    (setf (aref a 3) (ash (- id sum) -24))
    (incf sum (ash (aref a 3) 24))
    (setf (aref a 2) (ash (- id sum) -16))
    (incf sum (ash (aref a 2) 16))
    (setf (aref a 1) (ash (- id sum) -8))
    (incf sum (ash (aref a 1) 8))
    (setf (aref a 0) (- id sum))
    (values a port)))
;; (recover-user-id (calculate-user-id "127.0.0.1" 12345))

(defun message-2-udp-server ()
  (let ((line (read-line *server-stream*)))
    (multiple-value-bind (ip-string port buffer)
        (parse-message line)
      (let* ((user-id (calculate-user-id ip-string port));;(format nil "~A,~A" ip-string port))
             (sport (gethash user-id *user-sport*)))
        (if (not sport)
            ;; new connection
            (let* ((udp-socket (usocket:socket-connect *server-udp-ip* *server-udp-port*
                                                       :protocol :datagram))
                   (port (usocket:get-local-port udp-socket)))
              (setf (gethash port *sport-udp*) udp-socket)
              (setf (gethash port *sport-user*) user-id)
              (setf (gethash user-id *user-sport*) port)
              (setf sport port))
            ;; old connection
            nil)
        (let* ((udp-socket (gethash sport *sport-udp*)))
          (usocket:socket-send udp-socket buffer (length buffer)))))))

(defun udp-2-message-server (udp-socket)
  (let* ((sport (usocket:get-local-port udp-socket))
         (user-id (gethash sport *sport-user*)))
    (multiple-value-bind (addr user-port) (recover-user-id user-id)
      (multiple-value-bind (buffer size)
          (usocket:socket-receive udp-socket nil 65535)
        (let* ((user-ip-string (format nil "~A.~A.~A.~A"
                                       (aref addr 0) (aref addr 1)
                                       (aref addr 2) (aref addr 3))))
          (format *server-stream* "~A,~A,~A~%"
                  user-ip-string
                  user-port
                  (base64:usb8-array-to-base64-string (subseq buffer 0 size)))
          (force-output *server-stream*))))))

;; (defun udp-2-message-server (udp-socket)
;;   (multiple-value-bind (buffer size user-ip user-port)
;;       (usocket:socket-receive udp-socket nil 65535)
;;     (let* ((user-ip-string (format nil "~A.~A.~A.~A"
;;                                    (aref user-ip 0) (aref user-ip 1)
;;                                    (aref user-ip 2) (aref user-ip 3))))
;;       (format *server-stream* "~A,~A,~A~%"
;;               user-ip-string
;;               user-port
;;               (base64:usb8-array-to-base64-string (subseq buffer 0 size)))
;;       (force-output *server-stream*))))

(defun clear-server-side ()
  (defparameter *sport-user* (make-hash-table))
  (defparameter *user-sport* (make-hash-table))
  (defparameter *sport-udp* (make-hash-table))
  (when (usocket:usocket-p *server-tcp-socket*)
    (usocket:socket-close *server-tcp-socket*)))

(defun get-server-connections ()
  (concatenate 'list
               `(,*server-connection*)
               (let* ((result nil))
                 (maphash (lambda (sport socket)
                            sport
                            (push socket result))
                          *sport-udp*)
                 result)))

(defun server-side ()
  (defparameter *server-tcp-socket*
    (usocket:socket-listen *server-tcp-ip* *server-tcp-port*
                           :reuse-address t))
  (loop
     (block :accepting
       (defparameter *sport-user* (make-hash-table))
       (defparameter *user-sport* (make-hash-table))
       (defparameter *sport-udp* (make-hash-table))

       (format t "Waiting for connection...~%")
       (defparameter *server-connection*
         (usocket:socket-accept *server-tcp-socket*))
       (format t "Connection accepted!~%")
       (defparameter *server-stream* (usocket:socket-stream *server-connection*))

;;       (handler-case
           (loop
              (let ((connections (get-server-connections)))
                (format t "~A~%" connections)
                (let ((ready-sockets (usocket:wait-for-input connections
                                                             :ready-only t)))
                  (dolist (socket ready-sockets)
                    (cond ((usocket:stream-usocket-p socket)
                           ;; incoming message
                           (progn (format t "tcp msg~%")
                                  (message-2-udp-server)))
                          (t (progn
                               (format t "udp msg~%")
                               (udp-2-message-server socket)))))))))))
;;         (condition () (return-from :accepting))))))

;; (progn
;;   (clear-server-side)
;;   (server-side))
