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

(defparameter *debug-p* nil)
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

;; from user application to local UDP2TCP server
(defun udp-2-message ()
  (multiple-value-bind (buffer size user-ip user-port)
      (usocket:socket-receive *client-udp-socket* nil 65535)
    (when *debug-p* (format t "udp msg received, length:~A~%" size))
    (let* ((user-ip-string (format nil "~A.~A.~A.~A"
                                   (aref user-ip 0) (aref user-ip 1)
                                   (aref user-ip 2) (aref user-ip 3))))
      (format *tcp-stream* "~A,~A,~A~%"
              user-ip-string
              user-port
              (base64:usb8-array-to-base64-string (subseq buffer 0 size)))
      (force-output *tcp-stream*))))

;; TCP message from remote, parse and send it to targeted user application
(defun message-2-udp ()
  (let ((line (read-line *tcp-stream*)))
    (when *debug-p* (format *stdout* "~A~%" line))
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
  (format t "server tcp, IP: ~A, port: ~A~%" server-ip server-port)
  (format t "local udp, IP: ~A, port: ~A~%" client-udp-ip client-port)
  (format t "connecting...~%")
  (defparameter *client-tcp-socket* (usocket:socket-connect server-ip server-port))
  (format t "socket established.~%")
  (defparameter *tcp-stream* (usocket:socket-stream *client-tcp-socket*))
  (format t "open local udp socket.~%")
  (defparameter *client-udp-socket* (usocket:socket-connect
                                     nil nil
                                     :protocol :datagram
                                     :local-host client-udp-ip
                                     :local-port client-port))
  (format t "listening to local user application~%")
  (loop (let ((ready-sockets (usocket:wait-for-input (list *client-udp-socket*
                                                           *client-tcp-socket*)
                                                     :ready-only t)))
          (dolist (socket ready-sockets)
            (cond ((usocket:stream-usocket-p socket)
                   ;; incoming message
                   (progn (when *debug-p* (format t "tcp msg~%"))
                          (message-2-udp)))
                  (t (progn (when *debug-p* (format t "udp msg~%"))
                            (udp-2-message))))))))

(defun use-client-side ()
  (block :connection-loop
    (loop
       (handler-case
           (progn
             (clear-client-side)
             (client-side *server-tcp-ip* *server-tcp-port*
                          *client-udp-ip* *client-udp-port*))
         (end-of-file ()
           (format t "EOF error, server may be closed now, reconnect in 5 sec...~%"))
         ((or usocket:connection-reset-error usocket:connection-refused-error) ()
           (format t "Connection failed, reconnect in 5 sec...~%"))
         (condition (e)
           (progn (format t "Abort on Critical Error:~A~%")
                  (return-from :connection-loop))))
       (sleep 5))))

;; --------------------------------------------------
;; server side

(defparameter *server-tcp-socket* nil)
(defparameter *server-stream* nil)
(defparameter *server-connection* nil)

;; sport: service port, opened at the server side, simulating different apps
;; connecting to the server;
;;
;; user-id:
;; bit [0-7, ip0] [8-15, ip1] [16-23, ip2] [24-31, ip3] [32-47, portnumber]
;; todo: endian of portnumber?

;; mapping an sport to an user-id
(defparameter *sport-user* (make-hash-table))
;; mapping an user-id to an sport
(defparameter *user-sport* (make-hash-table))
;; mapping an sport to a udp socket
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

;; read tcp message from client side, parse it and send to server.
;; before send to server, a unique UDP socket must be defined, thus the server application
;; can determine which user application is being serviced:
;; 1. for a new connection(unkown user-id), a new UDP socket should be established.
;;    so do the hash table settings and send.
;; 2. for an old connection(known user-id, found in *user-sport* hash table),
;;    extract the UDP socket and send.
(defun message-2-udp-server ()
  (let ((line (read-line *server-stream*)))
    (when *debug-p* (format t "tcp msg received:~A~%" line))
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

;; receiving UDP package from server, encode it in the "IP,port,base64" format
;; and send it to client side via the TCP connection.
;; the IP and port describing the user application network settings in
;; the client side network, which can be recovered from the "user-id".
;; the user-id is obtained from the *sport-user* hash-table;
(defun udp-2-message-server (udp-socket)
  (let* ((sport (usocket:get-local-port udp-socket))
         (user-id (gethash sport *sport-user*)))
    (multiple-value-bind (addr user-port) (recover-user-id user-id)
      (multiple-value-bind (buffer size)
          (usocket:socket-receive udp-socket nil 65535)
        (let* ((user-ip-string (format nil "~A.~A.~A.~A"
                                       (aref addr 0) (aref addr 1)
                                       (aref addr 2) (aref addr 3)))
               (tcp-msg
                (format nil "~A,~A,~A~%"
                        user-ip-string
                        user-port
                        (base64:usb8-array-to-base64-string (subseq buffer 0 size)))))
          (when *debug-p* (format t "send tcp msg:~A" tcp-msg))
          (write tcp-msg :stream *server-stream*)
          (force-output *server-stream*))))))

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

;; todo: properly handle any conditions.
(defun server-side ()
  (defparameter *server-tcp-socket*
    (usocket:socket-listen *server-tcp-ip* *server-tcp-port*
                           :reuse-address t))
  (format t "server tcp: ~A:~A~%" (usocket:get-local-address *server-tcp-socket*)
          (usocket:get-local-port *server-tcp-socket*))
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
       (format t "Client: ~A:~A~%"
               (usocket:get-peer-address *server-connection*)
               (usocket:get-peer-port *server-connection*))
       (unwind-protect
            (loop
               (handler-case
                   (let ((connections (get-server-connections)))
                     (when *debug-p* (format t "~A~%" connections))
                     (let ((ready-sockets (usocket:wait-for-input connections
                                                                  :ready-only t)))
                       (dolist (socket ready-sockets)
                         (cond ((usocket:stream-usocket-p socket)
                                ;; incoming message
                                (progn (when *debug-p* (format t "tcp msg~%"))
                                       (message-2-udp-server)))
                               (t (progn
                                    (when *debug-p* (format t "udp msg~%"))
                                    (udp-2-message-server socket)))))))
                 (usocket:connection-refused-error (e)
                   (progn (format t "warning:~A~%" e)))
                 (condition (e) (progn (format t "error:~A~%" e)
                                       (return-from :accepting)))))
         ;; clean up
         (let* ((udp-socket-list (let* ((result nil))
                                   (maphash (lambda (sport socket)
                                              sport
                                              (push socket result))
                                            *sport-udp*)
                                   result)))
           (dolist (udp-socket udp-socket-list)
             (usocket:socket-close udp-socket)))))))

;; (progn
;;   (clear-server-side)
;;   (server-side))
