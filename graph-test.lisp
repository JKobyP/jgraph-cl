;;;; Unit testing harness for the graph-library
;; To operate, first load compile.lisp, then load
;; test.lisp

(in-package :edu.case.acm-people.jkp46.graph)

;; ========== Testing utilities ===========
(defparameter *gtype* 'graph)
(defparameter *g* (make-instance *gtype*))
(defun clean ()
  (setf *g* (make-instance *gtype*))) 

;; ========== Test definitions ==========
(define-test test-create-graph
  (assert-true (make-instance *gtype*)))

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
