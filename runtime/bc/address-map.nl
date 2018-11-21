;;; -*- mode: Lisp; coding: utf-8-unix -*-

;;; Common devices

(defvar console-base-addr #xF0001000)
(defvar timer-base-addr #xF0002000)
(defvar timer-interrupt 11)

;;; CLI Devices

(defvar output-dev-base-addr #xF0003000)
(defvar output-dev-interrupt 9)
(defvar input-dev-base-addr #xF0004000)
(defvar intput-dev-interrupt 10)

;;; WWW Devices
(defvar keyboard-base-addr #xF0004000)
(defvar keyboard-interrupt 9)

(defvar gfx-interrupt 10)
(defvar gfx-base-addr #xF0010000)
