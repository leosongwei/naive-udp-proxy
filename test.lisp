(ql:quickload 'usocket)

(let ((udp-socket (usocket:socket-connect nil nil
                                          :protocol :datagram
                                          :local-host "127.0.0.1"
                                          :local-port 9999)))
  (unwind-protect
       (loop (multiple-value-bind (buffer size user-ip user-port)
                 (usocket:socket-receive udp-socket nil 65535)
               (format t "size:~A~%" size)
               (usocket:socket-send udp-socket buffer size
                                    :port user-port
                                    :host user-ip)))
    (usocket:socket-close udp-socket)))

;; (let ((udp-socket (usocket:socket-connect "127.0.0.1" 4444 :protocol :datagram)))
;;   (defparameter *udp-socket* udp-socket)
;;   (usocket:socket-send udp-socket
;;                        (make-array 5 :initial-contents '(49 50 51 10 0)
;;                                    :element-type '(unsigned-byte 8))
;;                        5)
;;   (usocket:wait-for-input `(,udp-socket))
;;   (format t "ready!~%"))

;; (usocket:socket-receive *udp-socket* nil 10)


;; (concatenate 'list '(1 2 3) '(nil))
