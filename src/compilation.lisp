;;; compilation.lisp --- Compiler macros provided by the more-conditions system.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :more-conditions)

(macrolet ((define-checking-compiler-macro (function (class-arg &rest args))
	     `(define-compiler-macro ,function (&whole whole ,class-arg ,@args
						&environment env)
		(declare (ignore ,@(remove '&rest args)))
		(%check-class-argument whole ,class-arg env)
		whole)))

  (define-checking-compiler-macro missing-required-initarg (class initargs))
  (define-checking-compiler-macro incompatible-initargs (class &rest initargs)))


;;; Utility functions
;;

(defun %check-class-argument (whole class env)
  "Try to find CLASS in ENV in order to spot usage errors for
convenience functions like `missing-required-initarg' and
`incompatible-initargs'."
  (when (constantp class env)
    (let ((class/evaluated (eval class)))
      (when (and (symbolp class/evaluated)
		 (not (find-class class/evaluated nil env)))
	(warn 'simple-style-warning
	      :format-control   "~@<Could find class designated by ~S => ~
~S in ~S.~@:>"
	      :format-arguments (list class class/evaluated whole))))))
