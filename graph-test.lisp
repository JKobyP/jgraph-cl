;;;; Unit testing harness for the graph-library
;;; To operate, execute (in-package :lisp-unit)
;;; to set the repl's *package* variable, compile
;;; the graph package (with C-c-k) and then compile
;;; the testing harness. Run all tests with (run-tests).

;; Loading librarties and setting up environment
;(ql:quickload :lisp-unit)
;(defparameter old-package *package*)
;(defun leave ()
;  (setf *package* old-package))
;(load "graph.lisp")
(in-package :edu.case.acm-people.jkp46.graph)
;(in-package :lisp-unit)

;; ========== Testing utilities ===========
(defparameter *gtype* 'graph)
(defparameter *g* (make-instance *gtype*))
(defun clean ()
  (setf *g* (make-instance *gtype*))) 

;; ========== Test definitions ==========
(define-test test-create-graph
  (assert-true (make-instance 'graph)))

(define-test test-add-vertex
  (clean)
  (assert-equal
   "hi" (progn
	  (add-vertex *g* "hi" nil)
	  (vname (first (graph-al *g*))))))

(define-test test-add-vertex-returns-type-graph
  (clean)
  (assert-equal
   'graph (type-of (add-vertex *g* "hi" nil))))

(define-test test-get-neighbors
  (clean)
  (add-vertex *g* "a" '("b" "c"))
  (assert-equal '("b" "c") (get-neighbors *g* "a")))

(define-test test-add-vertex-with-invalid-neighbors
  (clean)
  (assert-error 'error (add-vertex *g* "a" "b")))
