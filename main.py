import re
import operator

class LispLexError(Exception):
	pass

class LispParseError(Exception):
	pass

class LispRumtimeError(Exception):
	def __init__(self, message = "unknown runtime error"):
		self.message = message
	def __str__(self):
		return self.message

def lex(source):
	#list of regexes, in order
	token_res = [
		(re.compile(r'\s+'), ' '), #space
		(re.compile(r"'"), "'"),
		(re.compile(r'\('), '('), 
		(re.compile(r'\)'), ')'), 
		(re.compile(r'\['), '['), 
		(re.compile(r'\]'), ']'), 
		(re.compile(r'nil'), 'NIL'), 
		(re.compile(r'#[tf]'), 'BOOL'),
		(re.compile(r'"([^"])*?"' ), "STRING"), #string require " as quotation; don't allow " in string.
		(re.compile(r'-?\d+'), "NUM"),
		(re.compile(r'[^"\'\(\)\s\d\[\]][^"\'\(\)\s\[\]]*'), 'NAME'),  #NAME token don't allow  spaces " ' ( ) [ ], don't allow numbers at head
	]

	length = len(source)
	pos = 0
	while pos < length:
		token_pair = None
		for regex, typ in token_res:
			m = regex.match(source, pos)
			if m:
				token = m.group(0)
				pos += len(token)
				token_pair = (token, typ) #string, type
				break
		if token_pair:
			if token_pair[1] != ' ':
				yield token_pair
		else: #token_pair == None
			raise LispLexError

'''
ASTCall = ( LispValue* )
LispValue = ASTCall | NUM | token
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
class LispValue(object):
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
			raise LispRuntimeError
	def eval(self, env):
		# print "evaling " + str(self) + ", type is " + self.typ
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
			return env[self.literal]
		elif self.typ == 'CRASH':
			raise LispRumtimeError
		else: # plain old values
			return self
	def bool(self):
		isFalse = self.typ == 'BOOL' and self.literal == False
		return LispBoolValue(not isFalse)

NIL_VALUE = LispValue(LispValue([], 'LIST'), 'QUOTE')
CRASH_VALUE = LispValue(None, 'CRASH')
TRUE_VALUE = LispValue(True, 'BOOL')
FALSE_VALUE = LispValue(False, 'BOOL')

def LispBoolValue(boo):
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
				value = LispValue(top.list, 'LIST')
			elif typ == 'NIL':
				value = NIL_VALUE
			elif typ == 'BOOL':
				value = LispBoolValue(token == '#t')
			elif typ == 'NUM':
				value = LispValue(int(token), typ)
			else: #NAME, STRING
				value = LispValue(token, typ)

			if stack and stack[-1].typ == "'":
				stack.pop()
				value = LispValue(value, 'QUOTE')

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
	def get(self, key):
		if key in self.dict:
			return self.dict[key]
		if not self.parent:
			raise LispRumtimeError("unbounded value "+ str(key))
		return self.parent[key]
	def __getitem__(self, key):
		return self.get(key)
	def __contains__(self, key):
		return key in self.dict or key in self.parent

def builtin_write_line(params, env):
	for p in params:
		print(str(p.eval(env)))
	return NIL_VALUE

#op: a -> a -> a
#builtin_arithmetic(op): [param a] -> param a
def builtin_arithmetic(op):
	def f(params, env):
		value = reduce(op,[o.eval(env).literal for o in params])
		return LispValue(value, 'NUM')	
	return LispValue(f, 'LAMBDA')

def builtin_predicate(op):
	def f(params, env):
		value = reduce(op,[o.eval(env).literal for o in params])
		return LispBoolValue(value)
	return LispValue(f, 'LAMBDA')


# (lambda (arg1 arg2) (+ arg1 arg2 1))
#return a value such that (v 1) is evaled to 2
def builtin_lambda(params, env):
	assert len(params) == 2
	#params[0] is LIST of NAMEs
	names = [o.literal for o in params[0].literal]
	body = params[1]
	#when the lambda is called...
	# ((some_lambda) 2 3)
	#params[1:] == (2 3)
	def f(params, env):
		assert len(names) == len(params)
		closure = Env(env)
		for i in range(len(names)):
			closure.set(names[i], params[i].eval(env))
		return body.eval(closure)
	return LispValue(f, 'LAMBDA')

def builtin_cons(params, env):
	assert len(params) == 2
	params = [o.eval(env) for o in params]
	ls = LispValue([], 'LIST')
	if params[1].typ == 'LIST':
		ls.literal = [params[0]] + params[1].literal
	else:
		ls.literal = params
	return ls # a LispValue


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
	return LispValue(tail, 'LIST')

#(define somename value)
#(define (fname args...) (some closure))
def builtin_define(params, env):
	assert len(params) == 2
	if params[0].typ == 'NAME':
		name = params[0].literal
		value = params[1].eval(env)
	elif params[0].typ == 'LIST':
		name = params[0].literal[0].literal
		value = LispValue([
			LispValue('lambda', 'NAME'), 
			LispValue(params[0].literal[1:], 'LIST'), 
			params[1]
			], 'LIST').eval(env)
	else:
		raise LispRumtimeError
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
	if len(params) == 2:
		# (let ((i 1) (j 2)) (+ i j))
		# -> ((lambda (i j) (+ i j)) 1 2)
		bindings, body = params
	elif len(params) == 3:		
		# (let <name> (bindings) <body>)
		# -> (letrec (<name> (lambda bindings-name <body>)) bindings-val)
		name, bindings, body = params
	else:
		raise LispRumtimeError
	names = [bind.literal[0] for bind in bindings.literal]
	vals = [bind.literal[1] for bind in bindings.literal]
	# for bind in bindings.literal:
	# 	assert len(bind.literal) == 2
	# 	names.append(bind.literal[0])
	# 	vals.append(bind.literal[1]) #not evaled here: evaled by lambda call
	rewritten_lambda = LispValue([
			LispValue('lambda', 'NAME'), 
			LispValue(names, 'LIST'), 
			body
			], 'LIST').eval(env)
	if len(params) == 2:
		closure = env
	elif len(params) == 3:		
		closure = Env(env).set(name.literal, rewritten_lambda)
	caller = LispValue([rewritten_lambda] + vals, 'LIST')
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
	evaled_params = map(lambda o:o.eval(env).literal, params)
	func = evaled_params[0]
	lists = evaled_params[1:]
	map_params = zip(*lists) #transpose, overflowed params ignored, as MIT scheme
	print map_params
	value = map(lambda o:func(o, env), map_params)
	return LispValue(value, 'LIST')

def builtin_not(params, env):
	assert len(params) == 1
	value = params[0].eval(env).bool()
	return LispBoolValue(not value.literal)

def builtin_eq_q(params, env):
	assert len(params) == 2
	return LispBoolValue(params[0].eval(env) is params[1].eval(env))

def builtin_equal_q(params, env):
	assert len(params) == 2
	return LispBoolValue(params[0].eval(env) == params[1].eval(env))

# (keep-matching-items '(1 2 -3 -4 5) positive?)
def builtin_keep_matching_items(params, env):
	assert len(params) == 2
	lst = params[0].eval(env)
	func = params[1].eval(env)
	ret = filter(lambda o:func.literal([o], env) == TRUE_VALUE, lst.literal)
	return LispValue(ret, 'LIST')

# (delete_matching_items '(1 2 -3 -4 5) positive?)
def builtin_delete_matching_items(params, env):
	assert len(params) == 2
	lst = params[0].eval(env)
	func = params[1].eval(env)
	ret = filter(lambda o:func.literal([o], env) == FALSE_VALUE, lst.literal)
	return LispValue(ret, 'LIST')

# (reduce + 0 '(1 2 3 4))
def builtin_reduce(params, env):
	assert len(params) == 3
	func = params[0].eval(env)
	base = params[1].eval(env)
	lst = params[2].eval(env)
	def reducing_call(base, next):
		call = LispValue([func, base, next], 'LIST')
		return call.eval(env)
	return reduce(reducing_call, lst.literal, base)

builtin_env = {
	'lambda': LispValue(builtin_lambda, 'LAMBDA'),
	'write-line': LispValue(builtin_write_line, 'LAMBDA'), #print everything
	'cons':LispValue(builtin_cons, 'LAMBDA'),
	'car':LispValue(builtin_car, 'LAMBDA'),
	'cdr':LispValue(builtin_cdr, 'LAMBDA'),
	'define':LispValue(builtin_define, 'LAMBDA'),
	'set!':LispValue(builtin_set_mutating, 'LAMBDA'),
	'let':LispValue(builtin_let, 'LAMBDA'),
	'let*':LispValue(builtin_let_star, 'LAMBDA'),
	'letrec':LispValue(builtin_letrec, 'LAMBDA'),
	'if':LispValue(builtin_if, 'LAMBDA'),	
	'cond':LispValue(builtin_cond, 'LAMBDA'),
	'not':LispValue(builtin_not, 'LAMBDA'),
	'eq?':LispValue(builtin_eq_q, 'LAMBDA'), #eqv? is strange. Do not use it.
	'equal?':LispValue(builtin_equal_q, 'LAMBDA'),
	'map':LispValue(builtin_map, 'LAMBDA'),
	'keep-matching-items':LispValue(builtin_keep_matching_items, 'LAMBDA'),
	'delete-matching-items':LispValue(builtin_delete_matching_items, 'LAMBDA'),
	'reduce':LispValue(builtin_reduce, 'LAMBDA'),
	'+': builtin_arithmetic(operator.add),
	'-': builtin_arithmetic(operator.sub),
	'*': builtin_arithmetic(operator.mul),
	'/': builtin_arithmetic(operator.div),
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
lexer: use groups to handle strings, get rid of doublequote in literal
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
	# (define (zero? x)
	#   (= x 0))
	# (define (incx i)
	#   (let inc-int ((j i) (s 0))
	#   	(if (zero? j)
	#   	    s
	#   	    (inc-int (- j 1) (+ s j)))))
	# (incx 1000)
	# '''
	# print code
	# for token, typ in lex(code):
	# 	print(token, typ)
	with open("incx.scm") as f:
		code = f.read()
	roots = parse(lex(code))
	for root in roots:
		print "value: " + str(eval(root))

if __name__ == '__main__':
	main()