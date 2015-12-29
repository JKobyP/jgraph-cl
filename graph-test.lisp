;;;; Unit testing harness for the graph-library
;; To operate, first load compile.lisp, then load
;; test.lisp

(in-package :edu.case.acm-people.jkp46.graph)
;; ========== Testing utilities ===========
(defparameter *gtype* 'vgraph)
;(defparameter *gtype* 'list-graph)
(defparameter *g* (make-instance *gtype*))
(defun clean ()
  (setf *g* (make-instance *gtype*))) 

;; ========== Test definitions ==========
(define-test test-create-graph
  (assert-true (make-instance *gtype*)))

;; We try to be as abstract as possible, but for a test
;; as low level as this, we must introspect to determine
;; whether a vertex is added as expected. As such we have
;; two separate cases for the different graph types.
(define-test test-add-vertex
  (clean)
  (add-vertex *g* 'hi nil)
  (assert-false (emptyp *g*))
  (assert-equal
   'hi (funcall (if (equal *gtype* 'vgraph)
                 #'vname 
                 #'car) 
        (first (graph-al *g*)))))

;(define-test test-add-vertex-returns-type-graph
;  (clean)
;  (assert-equal
;   *gtype*
;   (type-of (add-vertex *g* 'hi nil))))

(define-test test-get-neighbors
  (clean)
  (add-vertex *g* 'a '('b 'c))
  (assert-equal 
    '('b 'c) 
    (get-neighbors *g* 'a)))

(define-test test-add-vertex-with-invalid-neighbors
  (clean)
  (assert-error 'type-error (add-vertex *g* 'a 'b)))

(define-test test-remove-vertex
  (clean)
  (add-vertex *g* 'a '('b 'c))
  (remove-vertex *g* 'a)
  (assert-false
    (graph-al *g*)))
