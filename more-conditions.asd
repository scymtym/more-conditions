;;; more-conditions.asd --- System definition for more-conditions.
;;
;; Copyright (C) 2010, 2011, 2012 Jan Moringen
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

(cl:defpackage :more-conditions-system
  (:use
   :cl
   :asdf)

  (:export
   :version/list
   :version/string))

(cl:in-package :more-conditions-system)


;;; Version stuff
;;

(defconstant +version-major+ 0
  "Major component of version number.")

(defconstant +version-minor+ 1
  "Minor component of version number.")

(defconstant +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))


;;; System definition
;;

(defsystem :more-conditions
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPL3; see COPYING file for details."
  :description "This system provides some generic condition classes in
conjunction with support functions and macros."
  :depends-on  (:alexandria)
  :components  ((:module     "src"
		 :serial     t
		 :components ((:file       "package")
			      (:file       "conditions")
			      (:file       "macros"))))

  :in-order-to ((test-op (test-op :more-conditions-test))))

(defsystem :more-conditions-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPL3; see COPYING file for details."
  :description "Unit tests for the more-conditions system."
  :depends-on  (:cl-more-conditions
		:lift)
  :components  ((:module     "test"
		 :serial     t
		 :components ((:file       "package")
			      (:file       "conditions")
			      (:file       "macros")))))

(defmethod perform ((op        test-op)
		    (component (eql (find-system :more-conditions-test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))
