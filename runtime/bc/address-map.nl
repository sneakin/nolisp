;;; -*- mode: Lisp; coding: utf-8-unix -*-

;;; Common devices

(defvar console-base-addr #xF0001000)
(defvar timer-base-addr #xF0002000)
(defvar timer-interrupt 11)
(defvar rtc-base-addr #xF0006000)

;;; CLI Devices

(defvar output-dev-base-addr #xF0003000)
(defvar output-dev-interrupt 12)
(defvar input-dev-base-addr #xF0004000)
(defvar input-dev-interrupt 13)

;;; WWW Devices
(defvar keyboard-base-addr #xF0005000)
(defvar keyboard-interrupt 14)

(defvar gfx-interrupt (+ 9 16))
(defvar gfx-base-addr #xF0010000)
