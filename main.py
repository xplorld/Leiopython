#python 3
import re
import operator
#python docs say 99 percent of the time an explicit for loop is more readable than `reduce`.
#hmmmm. no.
from functools import reduce 

LEXER_DEBUG = False
RUNTIME_DEBUG = False

class SchemeLexError(Exception):
	pass

class SchemeParseError(Exception):
	pass

class SchemeRumtimeError(Exception):
	def __init__(self, message = "unknown runtime error"):
		self.message = message
	def __str__(self):
		return self.message

def lex(source):
	#list of regexes, in order
	token_res = [
		(re.compile(r'(\s+)'), ' '), #space
		(re.compile(r"(')"), "'"),
		(re.compile(r'(\()'), '('), 
		(re.compile(r'(\))'), ')'), 
		(re.compile(r'(\[)'), '['), 
		(re.compile(r'(\])'), ']'), 
		(re.compile(r'(nil)'), 'NIL'), 
		(re.compile(r'(#[tf])'), 'BOOL'),
		(re.compile(r'"([^"]*?)"' ), "STRING"), #string require " as quotation; don't allow " in string.
		(re.compile(r'(-?\d+)'), "NUM"),
		(re.compile(r'([^"\'\(\)\s\d\[\]][^"\'\(\)\s\[\]]*)'), 'NAME'),  #NAME token don't allow  spaces " ' ( ) [ ], don't allow numbers at head
	]

	length = len(source)
	pos = 0
	while pos < length:
		token_pair = None
		for regex, typ in token_res:
			m = regex.match(source, pos)
			if m:
				token = m.group(1)
				pos += len(m.group(0)) #may discard some chars
				token_pair = (token, typ) #string, type
				break
		if token_pair:
			if token_pair[1] != ' ':
				if LEXER_DEBUG:
					print(token_pair)
				yield token_pair
		else: #token_pair == None
			raise SchemeLexError

'''
ASTCall = ( SchemeValue* )
SchemeValue = ASTCall | NUM | token
'''

#a scope of values.
class ASTCall(object): 
	def __init__(self, typ):
		self.list = []
		self.typ = typ #( or [ or '
	@staticmethod
	def parentheses():
		return {'(':')','[':']', "'":""}
	def matches(self, closing):
		return closing == ASTCall.parentheses()[self.typ]
	def __str__(self):
		return self.typ + " ".join(map(str, self.list)) + ASTCall.parentheses()[self.typ]
	

#a name or a number or a string
'''
NUM, BOOL, STRING, NAME, LAMBDA, LIST, QUOTE, CRASH
CRASH is something not supposed to be evaled. e.g. return of (define )
'''
class SchemeValue(object):
	def __init__(self, literal, typ):
		self.typ = typ
		if typ == 'NUM':
			self.literal = int(literal)
		else:
			self.literal = literal
	def __str__(self):
		if self.typ == 'QUOTE':
			return "'" + str(self.literal)
		elif self.typ == 'LIST':
			return "(" + " ".join(map(str, self.literal)) + ")"
		elif self.typ == 'LAMBDA':
			return '<lambda>'
		elif self.typ == 'BOOL':
			return '#t' if self.literal else '#f'
		elif self.typ == 'STRING':
			return '"' + self.literal + '"'
		elif self.typ == 'CRASH':
			return ''
		return str(self.literal)
	def __eq__(self, other):
		if self.typ != other.typ:
			return False
		if self.typ in ['QUOTE', 'BOOL', 'NUM', 'STRING', 'LIST']:
			return self.literal == other.literal
		elif self.typ == 'LAMBDA':
			return self is other #labmdas are not comparable unless is the same object
		else: #NAME, CRASH
			raise SchemeRuntimeError
	def eval(self, env):
		if RUNTIME_DEBUG:
			print("evaling " + str(self) + ", type is " + self.typ)
		if self.typ == 'QUOTE':
			return self.literal
		elif self.typ == 'LIST':
			if not self.literal:
				return NIL_VALUE #there is only one '() object
			head = self.literal[0].eval(env) #now head should be a LAMBDA
			params = self.literal[1:] # not evaled
			return head.literal(params, env)
		elif self.typ == 'NAME':
			# print "got " + str(env[self.literal]) + " from " + self.literal
			return env.get(self.literal)
		elif self.typ == 'CRASH':
			raise SchemeRumtimeError
		else: # plain old values
			return self
	def bool(self):
		isFalse = self.typ == 'BOOL' and self.literal == False
		return SchemeBoolValue(not isFalse)

NIL_VALUE = SchemeValue(SchemeValue([], 'LIST'), 'QUOTE')
CRASH_VALUE = SchemeValue(None, 'CRASH')
TRUE_VALUE = SchemeValue(True, 'BOOL')
FALSE_VALUE = SchemeValue(False, 'BOOL')

def SchemeBoolValue(boo):
	return TRUE_VALUE if boo else FALSE_VALUE

def parse(tokens):
	stack = []
	for token, typ in tokens:
		if typ in ASTCall.parentheses():
			stack.append(ASTCall(typ))
		else: 
			value = None
			if typ in ASTCall.parentheses().values(): #) and ]
				top = stack.pop()
				assert top.matches(typ)
				value = SchemeValue(top.list, 'LIST')
			elif typ == 'NIL':
				value = NIL_VALUE
			elif typ == 'BOOL':
				value = SchemeBoolValue(token == '#t')
			elif typ == 'NUM':
				value = SchemeValue(int(token), typ)
			else: #NAME, STRING
				value = SchemeValue(token, typ)

			if stack and stack[-1].typ == "'":
				stack.pop()
				value = SchemeValue(value, 'QUOTE')

			if stack:
				stack[-1].list.append(value)
			else:
				yield value
	
'''a tree scoped dict'''
class Env(object):
	def __init__(self, parent):
		super(Env, self).__init__()
		self.parent = parent
		self.dict = {}
	def set(self, key, value):
		self.dict[key] = value
		return self
	def force_set(self, key, value):
		closure = self.find_closure(key)
		closure.set(key, value)
	def get(self, key):
		return self.find_closure(key).get(key)
	def find_closure(self, key):
		env = self
		while env:
			if key in env.dict:
				return env.dict
			else:
				env = env.parent
		raise SchemeRumtimeError("unbounded value "+ str(key))

def builtin_write_line(params, env):
	for p in params:
		print(str(p.eval(env)))
	return NIL_VALUE

#op: a -> a -> a
#builtin_arithmetic(op): [param a] -> param a
def builtin_arithmetic(op):
	def f(params, env):
		value = reduce(op,[o.eval(env).literal for o in params])
		return SchemeValue(value, 'NUM')	
	return SchemeValue(f, 'LAMBDA')

def builtin_predicate(op, binary = False):
	def f(params, env):
		if binary:
			assert len(params) == 2
		value = reduce(op,[o.eval(env).literal for o in params])
		return SchemeBoolValue(value)
	return SchemeValue(f, 'LAMBDA')


# (lambda (arg1 arg2) (display arg1) (+ arg1 arg2 1))
#return a LAMBDA value
def builtin_lambda(params, env):
	assert len(params) > 1
	#params[0] is LIST of NAMEs
	names = [o.literal for o in params[0].literal]
	body = params[1:]
	lexical_env = env
	#when the lambda is called...
	# ((some_lambda) 2 3)
	#params == (2 3)
	def f(params, env):
		assert len(names) == len(params)
		closure = Env(lexical_env)
		for i in range(len(names)):
			closure.set(names[i], params[i].eval(env))
		return [expr.eval(closure) for expr in body][-1]
	return SchemeValue(f, 'LAMBDA')

def builtin_cons(params, env):
	assert len(params) == 2
	params = [o.eval(env) for o in params]
	ls = SchemeValue([], 'LIST')
	if params[1].typ == 'LIST':
		ls.literal = [params[0]] + params[1].literal
	else:
		ls.literal = params
	return ls # a SchemeValue


def builtin_car(params, env):
	assert len(params) == 1
	lst = params[0].eval(env)
	head = lst.literal[0]
	return head

def builtin_cdr(params, env):
	assert len(params) == 1
	lst = params[0].eval(env)
	assert len(lst.literal) > 0
	tail = lst.literal[1:]
	return SchemeValue(tail, 'LIST')

#(define somename value)
#(define (fname args...) (some closures...))
def builtin_define(params, env):
	assert len(params) > 1
	if params[0].typ == 'NAME':
		name = params[0].literal
		value = params[1].eval(env)
	elif params[0].typ == 'LIST':
		name = params[0].literal[0].literal
		value = SchemeValue([
			SchemeValue('lambda', 'NAME'), 
			SchemeValue(params[0].literal[1:], 'LIST'), 
			*params[1:]
			], 'LIST').eval(env)
	else:
		raise SchemeRumtimeError
	env.set(name, value)
	return CRASH_VALUE

# (define x 0)
# (set! x 1) ;return 0
def builtin_set_mutating(params, env):
	assert len(params) == 2
	name = params[0]
	oldVal = name.eval(env)
	newVal = params[1].eval(env)
	env.set(name.literal, newVal)
	return oldVal

#values in bindings are evaled before application
def builtin_let(params, env):
	assert len(params) > 1
	if params[0].typ == 'NAME':
		# (let <name> (bindings) <body>)
		# -> (letrec (<name> (lambda bindings-name <body>)) bindings-val)	
		name, bindings, *body = params
	else:
		# (let ((i 1) (j 2)) (+ i j))
		# -> ((lambda (i j) (+ i j)) 1 2)
		bindings, *body = params

	names = [bind.literal[0] for bind in bindings.literal]
	vals = [bind.literal[1] for bind in bindings.literal]
	# for bind in bindings.literal:
	# 	assert len(bind.literal) == 2
	# 	names.append(bind.literal[0])
	# 	vals.append(bind.literal[1]) #not evaled here: evaled by lambda call
	rewritten_lambda = SchemeValue([
			SchemeValue('lambda', 'NAME'), 
			SchemeValue(names, 'LIST'), 
			*body
			], 'LIST').eval(env)
	if params[0].typ == 'NAME':
		closure = Env(env).set(name.literal, rewritten_lambda)
	else:
		closure = env
	caller = SchemeValue([rewritten_lambda] + vals, 'LIST')
	return caller.eval(closure)	

	# assert len(params) == 2
	# closure = Env(env)
	# for bind in params[0].literal:
	# 	assert len(bind.literal) == 2
	# 	val = bind.literal[1].eval(env)
	# 	closure.set(bind.literal[0].literal, val)
	# return params[1].eval(closure)
'''
(let ((x 2))
 (let ((x 3) (y x))
  y) => 2

(let ((x 2))
 (let* ((x 3) (y x))
  y) => 3
'''
def builtin_let_star(params, env):
	assert len(params) == 2
	closure = Env(env)
	for bind in params[0].literal:
		assert len(bind.literal) == 2
		val = bind.literal[1].eval(closure)
		closure.set(bind.literal[0].literal, val)
	return params[1].eval(closure)


def builtin_letrec(params, env):
	return builtin_let(params, env)

def builtin_if(params, env):
	assert len(params) == 3
	predicate = params[0].eval(env).bool()
	if predicate.literal:
		return params[1].eval(env)
	else:
		return params[2].eval(env)

# (cond
#   (predicate_1 clauses_1)
#   (predicate_2 clauses_2)
#     ......
#   (predicate_n clauses_n)
#   (else        clauses_else))

#else is just #t here.
#if no good branch to follow, return '()
def builtin_cond(params, env):
	for branch in params:
		pred = branch.literal[0]
		isElse = pred.typ == 'NAME' and pred.literal == 'else'
		if isElse or branch.literal[0].eval(env).bool() == TRUE_VALUE:
			return map(lambda o:o.eval(env), branch.literal[1:])[-1]
	return NIL_VALUE

# (map procedure list1 list2 ...)
def builtin_map(params, env):
	func = params[0].eval(env).literal
	lists = map(lambda o:o.eval(env).literal, params[1:])
	map_params = zip(*lists) #transpose, overflowed params ignored, as MIT scheme
	value = map(lambda o:func(o, env), map_params)
	return SchemeValue(value, 'LIST')

def builtin_not(params, env):
	assert len(params) == 1
	value = params[0].eval(env).bool()
	return SchemeBoolValue(not value.literal)

# (keep-matching-items '(1 2 -3 -4 5) positive?)
def builtin_keep_matching_items(params, env):
	assert len(params) == 2
	lst = params[0].eval(env)
	func = params[1].eval(env)
	ret = filter(lambda o:func.literal([o], env) == TRUE_VALUE, lst.literal)
	return SchemeValue(ret, 'LIST')

# (delete_matching_items '(1 2 -3 -4 5) positive?)
def builtin_delete_matching_items(params, env):
	assert len(params) == 2
	lst = params[0].eval(env)
	func = params[1].eval(env)
	ret = filter(lambda o:func.literal([o], env) == FALSE_VALUE, lst.literal)
	return SchemeValue(ret, 'LIST')

# (reduce + 0 '(1 2 3 4))
def builtin_reduce(params, env):
	assert len(params) == 3
	func = params[0].eval(env)
	base = params[1].eval(env)
	lst = params[2].eval(env)
	def reducing_call(base, next):
		call = SchemeValue([func, base, next], 'LIST')
		return call.eval(env)
	return reduce(reducing_call, lst.literal, base)

builtin_env = {
	'lambda': SchemeValue(builtin_lambda, 'LAMBDA'),
	'write-line': SchemeValue(builtin_write_line, 'LAMBDA'), #print everything
	'display': SchemeValue(builtin_write_line, 'LAMBDA'), #print everything
	'cons':SchemeValue(builtin_cons, 'LAMBDA'),
	'car':SchemeValue(builtin_car, 'LAMBDA'),
	'cdr':SchemeValue(builtin_cdr, 'LAMBDA'),
	'define':SchemeValue(builtin_define, 'LAMBDA'),
	'set!':SchemeValue(builtin_set_mutating, 'LAMBDA'),
	'let':SchemeValue(builtin_let, 'LAMBDA'),
	'let*':SchemeValue(builtin_let_star, 'LAMBDA'),
	'letrec':SchemeValue(builtin_letrec, 'LAMBDA'),
	'if':SchemeValue(builtin_if, 'LAMBDA'),	
	'cond':SchemeValue(builtin_cond, 'LAMBDA'),
	'not':SchemeValue(builtin_not, 'LAMBDA'),
	'eq?':builtin_predicate(operator.is_, binary = True), #eqv? is strange. Do not use it.
	'equal?':builtin_predicate(operator.eq, binary=True),
	'map':SchemeValue(builtin_map, 'LAMBDA'),
	'keep-matching-items':SchemeValue(builtin_keep_matching_items, 'LAMBDA'),
	'delete-matching-items':SchemeValue(builtin_delete_matching_items, 'LAMBDA'),
	'reduce':SchemeValue(builtin_reduce, 'LAMBDA'),
	'+': builtin_arithmetic(operator.add),
	'-': builtin_arithmetic(operator.sub),
	'*': builtin_arithmetic(operator.mul),
	'/': builtin_arithmetic(operator.truediv),
	'=': builtin_predicate(operator.eq),
	'<': builtin_predicate(operator.lt),
	'>': builtin_predicate(operator.gt),
	'<=': builtin_predicate(operator.le),
	'>=': builtin_predicate(operator.ge),
	}
global_env = Env(None)
global_env.dict = builtin_env
def eval(root):
	return root.eval(global_env)


'''
todo:
eval: more built-in functions, 7 axioms
better error messages
there are no (1 .2) like lists. all lists are nil terminated
'''


def main():
	# code = '''(write-line "1+2=" (+ 1 2) '(1 3))'''
	# code = '(+ 1 (* 2 3) (- 8 7))'
	# code = '((lambda (a b c d) (+ a b c d)) 2 3 4 5)'
	# code = '(cons 1 (cons 2 nil))'
	# code = '(define addOne (lambda (x) (+ x 1))) (addOne 2)'
	# code = '(define (addOne x) (+ x 1)) (addOne 2)'
	# code = '(define fac (lambda (a) (if (> a 0) (* a (fac (- a 1))) 1))) (fac 10)'
	# code = '(not #t)'
	# code = ' (let ((i 1) (j 2)) (+ i j))'
	# code = '''(map + '(1 2 3) '(4 5))'''
	# code = "(define (positive? x) [> x 0]) (delete-matching-items '[1 2 -3 -4 5] positive?)"
	# code = "'a"
	# code = "(reduce + 0 '(1 2 3 4))  "
	# code = '''
	# (define (zero? x) (= x 0))
	# (let my-chosen-name ((n 10) (acc '()))
 	#  (if (zero? n)
 	#      acc
 	#      (my-chosen-name (- n 1) (cons n acc)))) 
	# '''
	# code = '(= 1 2)'
	# code = '()'
	# code = "(cdr '('(* 2 3) (- 8 7)))"
	# code = '''
 	#  	(define (fact-tail x accum)
	#     (if (= x 0) accum
	#         (fact-tail (- x 1) (* x accum))))
	# (define (fact x)
	#  	(fact-tail x 1))
	# (fact 100)
	# '''
	# code = '''
	# (let ((x 1))
	# 	(let ((x 2))
	# 		(display x)
	# 		(set! x 10)
	# 		(display x))
	# 	(display x)
	# 	x)
	# '''
	code = '''
	(define x 10)
	(define (addX y) (+ x y))
	(let ((x 1)) (addX 2))
	'''#12 if lexical scoping, 3 if dynamic scoping
	# with open("incx.scm") as f:
	# 	code = f.read()
	global LEXER_DEBUG
	global RUNTIME_DEBUG
	# LEXER_DEBUG = True
	RUNTIME_DEBUG = True
	roots = parse(lex(code))
	for root in roots:
		print("value: " + str(eval(root)))

if __name__ == '__main__':
	main()