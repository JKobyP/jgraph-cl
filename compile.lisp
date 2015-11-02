(ql:quickload :lisp-unit)
(compile-file "graph-package.lisp")
(load "graph-package.lisp")

(compile-file "graph.lisp")
(load "graph.lisp")

(compile-file "graph-test.lisp")
(load "graph-test.lisp")

;(use-package :edu.case.acm-people.jkp46.graph)
