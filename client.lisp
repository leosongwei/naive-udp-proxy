(load "proxy.lisp")

(defparameter *client-udp-ip* "127.0.0.1")
(defparameter *client-udp-port* 7878)

(defparameter *server-tcp-ip* "127.0.0.1")
(defparameter *server-tcp-port* 8787)

;;(defparameter *debug-p* t)

(use-client-side)
