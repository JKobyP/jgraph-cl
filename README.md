# jgraph-cl
It's just a graph implementation in Common Lisp. It may eventually become more than that. 
The package is nicknamed `jgraph`.

### to build
Get behind your repl and execute this gem - 

    (load "compile.lisp")
Having quicklisp installed will help make sure this doesn't fail.

### to test
    (load "test.lisp")
We use lisp-unit for our test suite. Coverage is currently dismal. But then again, so are the features.

## details
There are currently two implementations of the graph - one using an **object oriented** approach,
and one using Lisp's builtin **association-list** type. **The association-list implementation is deprecated** -
CLOS contributes method overloading and information-hiding structures that are nice enough it
wouldn't make sense to go without.
