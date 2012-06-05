;;; macros.lisp --- Unit tests for the macros provided by the more-conditions system.
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

(cl:in-package :more-conditions.test)

(deftestsuite macros-root (root)
  ()
  (:setup
   ;; Without eval, causes failed aver in SBCL.
   (eval
    `(progn
       (define-condition source-condition (error)
	 ())
       (define-condition target-condition/no-cause (error)
	 ((slot :initarg  :slot
		:reader   target-condition-slot
		:initform :default)))
       (define-condition target-condition/cause (error
						 chainable-condition)
	 ((slot :initarg  :slot
		:reader   target-condition-slot
		:initform :default))))))
  (:documentation
   "Test suite for macros provided by the more-conditions
system."))

(deftestsuite with-condition-translation-root (macros-root)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(addtest (with-condition-translation-root
          :documentation
	  "TODO(jmoringe): document")
  smoke/no-cause

  (let ((source (make-condition 'source-condition)))
   (handler-case
       (with-condition-translation (:to-condition target-condition/no-cause)
	 (error source))
     (target-condition/no-cause (condition)
       (ensure-same (target-condition-slot condition) :default)))))

(addtest (with-condition-translation-root
	  :documentation
	  "TODO(jmoringe): document")
  smoke/cause

  (let ((source (make-condition 'source-condition)))
    (handler-case
	(with-condition-translation (:to-condition target-condition/cause)
	  (error source))
      (target-condition/cause (condition)
	(ensure-same (target-condition-slot condition) :default)
	(ensure-same (cause                 condition) source)
	(ensure-same (root-cause            condition) source)))))

(deftestsuite define-condition-translating-method-root (macros-root)
  ()
  (:setup
   (defmethod foo ((bar t))
     (error bar))
   (define-condition-translating-method foo ((bar t))
     :to-condition target-condition/cause)

   (defmethod foo/initargs ((bar t))
     (error bar))
   (define-condition-translating-method foo/initargs ((bar t))
     :to-condition  target-condition/no-cause
     :initargs      (:slot :supplied)
     :cause-initarg nil))
  (:teardown
   (fmakunbound 'foo)
   (fmakunbound 'foo/initargs))
  (:documentation
   "Test suite for the `define-condition-translating-method' macro."))

(addtest (define-condition-translating-method-root
	  :documentation
	  "TODO(jmoringe): document")
  smoke/cause

  (let ((source (make-condition 'source-condition)))
    (handler-case
	(foo source)
      (target-condition/cause (condition)
	(ensure-same (target-condition-slot condition) :default)
	(ensure-same (cause                 condition) source)
	(ensure-same (root-cause            condition) source)))))

(addtest (define-condition-translating-method-root
	  :documentation
	  "TODO(jmoringe): document")
  smoke/initargs

  (let ((source (make-condition 'source-condition)))
    (handler-case
	(foo/initargs source)
      (target-condition/no-cause (condition)
	(ensure-same (target-condition-slot condition) :supplied)))))
