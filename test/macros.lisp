;;;; macros.lisp --- Unit tests for the macros provided by the more-conditions system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:more-conditions.test)

(in-suite :more-conditions)

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
         :initform :default)))

(test with-condition-translation.smoke/no-cause
  "Smoke test for translating `error' to a condition class without
   cause storage via `with-condition-translation'."

  (let ((source (make-condition 'source-condition)))
    (handler-case
        (with-condition-translation (((error target-condition/no-cause
                                       :cause-initarg nil)))
          (error source))
      (target-condition/no-cause (condition)
        (is (eq :default (target-condition-slot condition)))))))

(test with-condition-translation.smoke/cause
  "Smoke test for translating `error' to a condition class with cause
   storage via `with-condition-translation'."

  (let ((source (make-condition 'source-condition)))
    (handler-case
        (with-condition-translation (((error target-condition/cause)))
          (error source))
      (target-condition/cause (condition)
        (is (eq :default (target-condition-slot condition)))
        (is (eq source   (cause                 condition)))
        (is (eq source   (root-cause            condition)))))))

(test with-condition-translation.smoke/chain-same-class
  "Smoke test for chaining causing conditions of the same class in
   nested translations."

  (let ((source (make-condition 'source-condition)))
    ;; Without chaining causing conditions of the same class (the
    ;; default), SOURCE should be wrapped in one
    ;; `target-condition/cause' condition.
    (handler-case
        (with-condition-translation (((error target-condition/cause
                                             :chain-same-class? nil)))
          (with-condition-translation (((error target-condition/cause)))
            (error source)))
      (target-condition/cause (condition)
        (is (eq :default (target-condition-slot condition)))
        (is (eq source   (cause                 condition)))
        (is (eq source   (root-cause            condition)))))

    ;; With chaining causing conditions of the same class, SOURCE
    ;; should be wrapped in two `target-condition/cause' conditions.
    (handler-case
        (with-condition-translation (((error target-condition/cause
                                       :chain-same-class? t)))
          (with-condition-translation (((error target-condition/cause)))
            (error source)))
      (target-condition/cause (condition)
        (is (eq :default (target-condition-slot condition)))
        (is (eq :default (target-condition-slot (cause condition))))

        (is (typep (cause condition) 'target-condition/cause))
        (is (eq source   (cause                 (cause condition))))

        (is (eq source   (root-cause            condition)))
        (is (eq source   (root-cause            (cause condition))))))))

(def-fixture with-mock-generic-function/foo ()
  (unwind-protect
       (progn
         (defgeneric foo (bar)
           (:method ((bar t))
             (error bar)))

         (define-condition-translating-method foo ((bar t))
           ((error target-condition/cause)))

         (locally
             (declare #+sbcl (sb-ext:muffle-conditions style-warning))
           (&body)))

    (fmakunbound 'foo)))

(test (define-condition-translating-method.smoke/cause
       :fixture with-mock-generic-function/foo)
  "Smoke test for defining a condition translating method with
   capturing of the causing condition via
   `define-condition-translating-method.'"

  (let ((source (make-condition 'source-condition)))
    (handler-case
        (foo source)
      (target-condition/cause (condition)
        (is (eq :default (target-condition-slot condition)))
        (is (eq source   (cause                 condition)))
        (is (eq source   (root-cause            condition)))))))

(def-fixture with-mock-generic-function/foo/initargs ()
  (unwind-protect
       (progn
         (defgeneric foo/initargs (bar)
           (:method ((bar t))
             (error bar)))

         (define-condition-translating-method foo/initargs ((bar t))
           ((error target-condition/no-cause
                   :cause-initarg nil)
            :slot :supplied))

         (locally
             (declare #+sbcl (sb-ext:muffle-conditions style-warning))
           (&body)))

     (fmakunbound 'foo/initargs)))

(test (define-condition-translating-method.smoke/initargs
       :fixture with-mock-generic-function/foo/initargs)
  "Smoke test for defining a condition translating method which adds
   additional initargs via `define-condition-translating-method.'"

  (let ((source (make-condition 'source-condition)))
    (handler-case
        (foo/initargs source)
      (target-condition/no-cause (condition)
        (is (eq :supplied (target-condition-slot condition)))))))

;;; `error-behavior-restart-case'

(test error-behavior-restart-case-root.smoke
  "Smoke test for the `error-behavior-restart-case' macro."

  (mapc
   (lambda+ ((policy expected))
     (flet ((do-it (warning?)
              (macrolet
                  ((body (warning?)
                     `(error-behavior-restart-case
                       (policy
                        (simple-error
                         :format-control   "Example error: ~A"
                         :format-arguments (list :foo))
                        ,@(when warning?
                            '(:warning-condition simple-warning))
                        :allow-other-values? t)
                       (continue (&optional condition)
                         (declare (ignore condition))
                         :continue))))
                (if warning? (body t) (body nil)))))

       (cond
         ((eq expected 'error)
          (signals error (do-it nil))
          (signals error (do-it t)))
         ((member policy `(warn ,#'warn))
          (signals program-error (do-it nil))
          (signals warning (do-it t)))
         (t
          (is (eq expected (do-it nil)))
          (is (eq expected (do-it t)))))))

   `((,#'error    error)
     (error       error)
     (,#'warn     nil)
     (warn        nil)
     (,#'continue :continue)
     (continue    :continue)
     (nil         nil)
     (:foo        :foo)
     (1           1))))

;;; Progress macros

(test with-trivial-progress-root.smoke
  "Smoke test for the `with-trivial-progress' macro."

  (macrolet
      ((test-case (&rest args)
         `(let ((conditions '()))
            (handler-bind ((progress-condition
                             (lambda (condition)
                               (push condition conditions))))
              (with-trivial-progress ,args))
            (mapc #'princ-to-string conditions)
            (is (= 2 (length conditions))))))

    (test-case :foo)
    (test-case :foo "bar")
    (test-case :foo "bar: ~A" :baz)
    (test-case :foo 'simple-progress-condition)
    (test-case :foo 'simple-progress-condition
                    :format-control "bar")
    (test-case :foo 'simple-progress-condition
                    :format-control   "bar: ~A"
                    :format-arguments '(:baz))))

(test with-sequence-progress.smoke
  "Smoke test for the `with-sequence-progress' macro."

  (macrolet
      ((test-case ((expected-conditions operation sequence &rest args) &body body)
         `(let ((conditions '()))
            (handler-bind ((progress-condition
                             (lambda (condition)
                               (push condition conditions))))
              (let ((sequence ,sequence))
                (with-sequence-progress (,operation sequence ,@(rest args))
                  ,@body)))
            (mapc #'princ-to-string conditions)
            (is (= ,expected-conditions (length conditions))))))

    ;; `progress'
    (test-case (3 :foo '(1 2)) (progress))
    (test-case (3 :foo '(1 2)) (progress "bar"))
    (test-case (3 :foo '(1 2)) (progress (formatter "bar")))
    (test-case (3 :foo '(1 2)) (progress "bar: ~A" :baz))
    (test-case (3 :foo '(1 2)) (progress (formatter "bar: ~A") :baz))
    (test-case (3 :foo '(1 2)) (progress 'simple-progress-condition))
    (test-case (3 :foo '(1 2))
      (progress 'simple-progress-condition :format-control "bar"))
    (test-case (3 :foo '(1 2))
      (progress 'simple-progress-condition
                :format-control (formatter "bar")))
    (test-case (3 :foo '(1 2))
      (progress 'simple-progress-condition
                :format-control   "bar: ~A"
                :format-arguments '(:baz)))
    (test-case (3 :foo '(1 2))
      (progress 'simple-progress-condition
                :format-control   (formatter "bar: ~A")
                :format-arguments '(:baz)))

    ;; `progressing'
    (test-case (4 :foo '(1 2))
      (mapc (progressing #'1+ :foo) sequence))
    (test-case (4 :foo '(1 2))
      (mapc (progressing #'1+ :foo "bar") sequence))
    (test-case (4 :foo '(1 2))
      (mapc (progressing #'1+ :foo (formatter "bar")) sequence))
    (test-case (4 :foo '(1 2))
      (mapc (progressing #'1+ :foo "bar: ~A" :baz) sequence))
    (test-case (4 :foo '(1 2))
      (mapc (progressing #'1+ :foo (formatter "bar: ~A") :baz) sequence))
    (test-case (4 :foo '(1 2))
      (mapc (progressing #'1+ :foo "bar: ~D") sequence))
    (test-case (4 :foo '(1 2))
      (mapc (progressing #'1+ :foo 'simple-progress-condition)
            sequence))
    (test-case (4 :foo '(1 2))
      (mapc (progressing #'1+ :foo 'simple-progress-condition
                              :format-control "bar")
            sequence))
    (test-case (4 :foo '(1 2))
      (mapc (progressing #'1+ :foo 'simple-progress-condition
                              :format-control (formatter "bar"))
            sequence))
    (test-case (4 :foo '(1 2))
      (mapc (progressing #'1+ :foo 'simple-progress-condition
                              :format-control   "bar: ~A"
                              :format-arguments '(:baz))
            sequence))
    (test-case (4 :foo '(1 2))
      (mapc (progressing #'1+ :foo 'simple-progress-condition
                              :format-control   (formatter "bar: ~A")
                              :format-arguments '(:baz))
            sequence))))
