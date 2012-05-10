;;; macros.lisp --- Macros provided by the cl-more-conditions system.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(defmacro with-condition-translation
    ((&key
      (var            (gensym)                          var-supplied?)
      (from-condition 'error)
      (to-condition   (missing-required-argument :to-condition))
      initargs
      (cause-initarg  :cause)
      (signal-via     'error))
     &body body)
  "TODO(jmoringe): document"
  (check-type var            symbol)
  (check-type from-condition (or symbol cons))
  (check-type to-condition   symbol "the name of a condition class")
  (check-type initargs       list)
  (check-type cause-initarg  symbol)
  (check-type signal-via     symbol)

  `(handler-bind (((and ,from-condition (not ,to-condition))
		   #'(lambda (,var)
		       ,@(unless (or var-supplied? cause-initarg)
			 `((declare (ignore ,var))))
		       (,signal-via ',to-condition
				    ,@initargs
				    ,@(when cause-initarg
				       `(,cause-initarg ,var))))))
     ,@body))

(defmacro define-condition-translating-method
    (name (&rest args)
     &key
     (var            nil                                 var-supplied?)
     (from-condition 'error)
     (to-condition   (required-argument :condition-class))
     initargs
     (cause-initarg  :cause)
     (signal-via     'error))
  "TODO(jmoringe): document"
  (when var-supplied?
    (check-type var symbol))
  (check-type to-condition  symbol "the name of a condition class")
  (check-type initargs      list)
  (check-type cause-initarg symbol)
  (check-type signal-via    symbol)

  `(defmethod ,name :around (,@args)
     ,(format nil "Translate conditions satisfying ~(~S~) to conditions of class `~(~A~)'."
	      from-condition to-condition)
     (with-condition-translation (,@(when var-supplied?
				      `((,:var ,var)))
				  :to-condition  ,to-condition
				  :initargs      ,initargs
				  :cause-initarg ,cause-initarg
				  :signal-via    ,signal-via)
       (call-next-method))))
