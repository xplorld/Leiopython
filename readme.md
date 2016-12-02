# Leiopython

Yet another Scheme Interpreter in Python!

## Etymology:

Leiopython is a kind of white-lipped python in New Guinea.

## Known Problems

### deep recursion failure - nested Enviroment

Now recursion is called by python calls. see `bulitin_lambda`. Enviroment is stored in `Env` object, which is (sort of) a reversed single-link list. When we need to resolve a `NAME`, `Env` is searched through its father `Env`s by `literal` strings. Every call to a lambda introduce a deeper level of `Env`. Consider code below:
		
		(define (zero? x)
		  (= x 0))
		(define (fac i)
		  (if (zero? i) 1 (* (fac (- i 1)) i)))
		(fac 100)

`fac n` computes `n!`. When we call, say `(fac 100)`, the Env looks like

	{"fac":<lambda object>} -> {"i":100} -> {"i":99} -> {"i":98} ...

and when we finally go down into `(fac 2)`, the NAME `fac` mush searched through chained `Env.get` method by 98 times! In another word, `n` nested calls in scheme would make `O(n^2)` nested calls in python.

#### Solution

1. flattened `Env`

Instead of `closure = Env(parent_env).set("i", 99)`, we have `closure = parent_env.copy().set("i",99)`

**What about `set!`?**

2. a global `Env` whose `get` is implemented manually instead of recursive call

Instead of `closure = Env(parent_env).set("i", 99)`, we have `closure_p = env.new_level().set("i",99)`

### deep recursion failure - tail recursion optimization

Even the env problem is solved, that a `n` nested scheme call reflects in `O(n)` python call, it still may yield a stack overflow. 

Scheme is guranteed to have tail recursion optimization. We should implement it.

### Hygienic Macros

We should implement it. 'cause it's cool.

### Quasiquote and Unquote

### Variable Arguments

### Moriturus te saluto.
