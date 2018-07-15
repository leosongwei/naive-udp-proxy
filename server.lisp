(load "proxy.lisp")

(defparameter *server-tcp-ip* "127.0.0.1")
(defparameter *server-tcp-port* 8787)

(defparameter *server-udp-ip* "127.0.0.1")
(defparameter *server-udp-port* 9999)

;;(defparameter *debug-p* t)

(progn
  (clear-server-side)
  (server-side))
