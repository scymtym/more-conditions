;;;; conditions.lisp --- Unit tests for conditions provided by the more-conditions system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:more-conditions.test)

(in-suite :more-conditions)

(define-condition foo-error (error
                             chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Foo-error ~
                     occurred.~/more-conditions:maybe-print-cause/"
             condition))))

(test maybe-print-cause.print
  "Test printing condition instances using the `maybe-print-cause'
   helper function."

  (mapc (lambda+ ((initargs expected))
          (is (string= expected
                       (princ-to-string
                        (apply #'make-condition 'foo-error initargs)))))

        `((()
           "Foo-error occurred.")
          ((:cause ,(make-condition 'simple-error
                                    :format-control "The number was ~S."
                                    :format-arguments '(1)))
           "Foo-error occurred. Caused by:
> The number was 1."))))

(define-condition simple-foo-error (simple-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Foo-error ~
                     occurred~/more-conditions:maybe-print-explanation/"
             condition))))

(test maybe-print-explanation.print
  "Test printing condition instances using the
   `maybe-print-explanation' helper function."

  (mapc
   (lambda+ ((initargs expected))
     (is (string= expected
                  (princ-to-string
                   (apply #'make-condition 'simple-foo-error initargs)))))

   '(;; No format-control.
     (()
      "Foo-error occurred.")
     ;; With format-control.
     ((:format-control   "the number was ~S."
       :format-arguments (1))
      "Foo-error occurred: the number was 1."))))

;;; Program error conditions

(defmacro define-condition-suite ((name &key (constructor name))
                                  &body cases)
  (let ((suite-name (make-keyword name)))
    `(progn
       (def-suite ,suite-name
         :in :more-conditions
         :description
         ,(format nil "Unit tests for the `~(~A~)' condition class."
                  name))
       (in-suite ,suite-name)

       (test construct-and-print/make-condition
         ,(format nil "Test printing instances of the `~(~A~)' condition class"
                  name)

         (mapc (lambda+ ((initargs constructor-args expected))
                 (declare (ignore constructor-args))
                 (is (string= expected
                              (princ-to-string
                               (apply #'make-condition ',name initargs)))))
               (list ,@cases)))

       ,@(when (fboundp constructor)
           `((test construct-and-print/constructor
               ,(format nil "Test printing instances of the `~(~A~)' condition class"
                        name)

               (mapc (lambda+ ((initargs constructor-args expected))
                       (declare (ignore initargs))
                       (unless (eq constructor-args :skip)
                         (is (string= expected
                                      (handler-case
                                          (apply #',constructor constructor-args)
                                        (,name (condition)
                                          (princ-to-string condition)))))))
                     (list ,@cases))))))))

(define-condition-suite (missing-required-argument)
  '((:parameter :foo)
    (:foo)
    "No value has been supplied for the required parameter :FOO."))

(define-condition-suite (incompatible-arguments)
  '((:parameters () :values ())
    ()
    "No arguments are invalid.")
  '((:parameters (:foo) :values (1))
    (:foo 1)
    "The combination of arguments

  :FOO 1

is invalid.")
  '((:parameters (:foo :barbar) :values (1 2))
    (:foo 1 :barbar 2)
    "The combination of arguments

  :FOO    1
  :BARBAR 2

is invalid.")
  `((:parameters (:foo)
     :values     (1)
     :cause      ,(make-instance 'simple-error
                                 :format-control "foo"))
    :skip
    "The combination of arguments

  :FOO 1

is invalid.
Caused by:
> foo"))

(define-condition-suite (initarg-error)
  '((:class :foo)
    nil
    "Invalid initargs have been supplied for class :FOO."))

(define-condition-suite (missing-required-initarg)
  '((:class :foo :parameter :bar)
    (:foo :bar)
    "The initarg :BAR is required by class :FOO, but has not been supplied."))

(define-condition-suite (incompatible-initargs)
  '((:class :foo :parameters () :values ())
    (:foo)
    "No initargs are invalid for class :FOO.")
  '((:class :foo :parameters (:bar) :values (1))
    (:foo :bar 1)
    "The combination of initargs

  :BAR 1

is invalid for class :FOO.")
  '((:class :foo :parameters (:bar :bazbaz) :values (1 2))
    (:foo :bar 1 :bazbaz 2)
    "The combination of initargs

  :BAR    1
  :BAZBAZ 2

is invalid for class :FOO.")
  `((:class      :foo
     :parameters (:bar)
     :values     (1)
     :cause      ,(make-instance 'simple-error
                                 :format-control "foo"))
    :skip
    "The combination of initargs

  :BAR 1

is invalid for class :FOO.
Caused by:
> foo"))

;;; `reference-condition'

(define-condition reference-error (error reference-condition)
  ()
  (:report "Reference Error."))

(define-condition-suite (reference-error)
  '((:references ())
    ()
    "Reference Error.")

  '((:references ((:foo "bar")))
    ()
    "Reference Error.
See also:
  FOO, bar")

  '((:references ((:foo "bar")
                  (:foo ("bar" "baz"))
                  (:foo ("bar" "baz") "http://fez.org")))
    ()
    "Reference Error.
See also:
  FOO, bar
  FOO, bar » baz
  FOO, bar » baz <http://fez.org>"))

(define-condition mock-error/reference-condition (error
                                                  reference-condition
                                                  chainable-condition)
  ()
  (:report (lambda (condition stream)
             (let ((*print-references* nil))
               (format stream "Mock Error.~
                               ~/more-conditions:maybe-print-cause/"
                       condition)))))

(define-condition-suite (mock-error/reference-condition)
  `((:references ((:foo "bar")
                  (:fez "whiz"))
     :cause      ,(make-condition 'reference-error
                                  :references '((:foo "bar")
                                                (:foo "baz")
                                                (:bar "fez" "http://whoop.org"))))
    ()
    "Mock Error. Caused by:
> Reference Error.
See also:
  FOO, bar
  FOO, baz
  BAR, fez <http://whoop.org>
  FEZ, whiz"))

;;; Progress conditions

(define-condition-suite (progress-condition :constructor progress)
  `(()
    ()
    "???.?? %")
  `((:operation :foo)
    (:foo)
    "FOO: ???.?? %")
  `((:operation :foo :progress nil)
    (:foo nil)
    "FOO: ???.?? %")
  `((:operation :foo :progress 0)
    (:foo 0)
    "FOO:   0.00 %")
  `((:operation :foo :progress 1)
    (:foo 1)
    "FOO: 100.00 %")
  `((:operation :foo :progress t)
    (:foo t)
    "FOO: 100.00 %")
  `((:operation :foo :progress .51234)
    (:foo .51234 progress-condition)
    "FOO:  51.23 %"))

(define-condition-suite (simple-progress-condition :constructor progress)
  `((:operation :foo :progress .51234)
    (:foo .51234 simple-progress-condition)
    "FOO:  51.23 %")
  `((:operation :foo :progress .51234 :format-control "bar")
    (:foo .51234 "bar")
    "FOO:  51.23 %: bar")
  `((:operation        :foo
     :progress         .51234
     :format-control   "bar: ~A"
     :format-arguments (:baz))
    (:foo .51234 "bar: ~A" :baz)
    "FOO:  51.23 %: bar: BAZ"))
