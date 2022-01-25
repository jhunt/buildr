;;;
;;; src/package.lisp
;;; ©2022 James Hunt
;;;
;;; This file defines the BUILR package, its
;;; implicitly-included downstream dependencies,
;;; and its exported functions, variables, etc.
;;;

(in-package #:cl-user)
(defpackage #:buildr
  (:use :cl)
  (:export :run/all))
