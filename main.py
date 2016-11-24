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
		(re.compile('\s+'), ' '), #space
		(re.compile('\('), '('), 
		(re.compile("'\("), "'("),
		(re.compile('\)'), ')'), 
		(re.compile('nil'), 'NIL'), 
		(re.compile('#[tf]'), 'BOOL'),
		(re.compile(r'"([^"])*?"' ), "STRING"), #string require " as quotation; don't allow " in string.
		(re.compile('\d+'), "NUM"),
		(re.compile(r'[^"\'\(\)\s\d][^"\'\(\)\s]*'), 'NAME'), 
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

#just a list of values.
class ASTCall(object): 
	def __init__(self, is_quote = False):
		self.list = []
		self.is_quote = is_quote
	def __str__(self):
		head = "("
		if self.is_quote:
			head = "'("
		return head + " ".join(map(str, self.list)) + ")"
	

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
	def eval(self, env):
		print "evaling " + str(self) + ", type is " + self.typ
		if self.typ == 'QUOTE':
			return self.literal
		elif self.typ == 'LIST':
			if not self.literal:
				return self
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
		if self.typ == 'BOOL' and self.literal == False:
			return FALSE_VALUE
		return TRUE_VALUE

NIL_VALUE = LispValue(LispValue([], 'LIST'), 'QUOTE')
CRASH_VALUE = LispValue(None, 'CRASH')
TRUE_VALUE = LispValue(True, 'BOOL')
FALSE_VALUE = LispValue(False, 'BOOL')

def parse(tokens):
	stack = []
	for token, typ in tokens:
		if typ == '(':
			stack.append(ASTCall())
		elif typ == "'(":
			stack.append(ASTCall(True))
		else: 
			value = None
			if typ == ')':
				top = stack.pop()
				value = LispValue(top.list, 'LIST')
				if top.is_quote:
					value = LispValue(value, 'QUOTE')
			elif typ == 'NIL':
				value = NIL_VALUE
			elif typ == 'BOOL':
				value = TRUE_VALUE if token == '#t' else FALSE_VALUE
			elif typ == 'NUM':
				value = LispValue(int(token), typ)
			else: #NAME, STRING
				value = LispValue(token, typ)

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
		value = reduce(op,map(lambda o:o.eval(env).literal, params))
		return LispValue(value, 'NUM')	
	return LispValue(f, 'LAMBDA')

def builtin_predicate(op):
	def f(params, env):
		value = reduce(op,map(lambda o:o.eval(env).literal, params))
		return TRUE_VALUE if value else FALSE_VALUE
	return LispValue(f, 'LAMBDA')


# (lambda (arg1 arg2) (+ arg1 arg2 1))
#return a value such that (v 1) is evaled to 2
def builtin_lambda(params, env):
	assert len(params) == 2
	#params[0] is LIST of NAMEs
	names = map(lambda o:o.literal, params[0].literal)
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
	params = map(lambda o:o.eval(env), params)
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

# (let ((i 1) (j 2)) (+ i j))
# 3
#values in bindings are evaled before application
def builtin_let(params, env):
	assert len(params) == 2
	closure = Env(env)
	for bind in params[0].literal:
		assert len(bind.literal) == 2
		val = bind.literal[1].eval(env)
		closure.set(bind.literal[0].literal, val)
	return params[1].eval(closure)

def builtin_if(params, env):
	assert len(params) == 3
	predicate = params[0].eval(env).bool()
	if predicate.literal:
		return params[1].eval(env)
	else:
		return params[2].eval(env)
def builtin_not(params, env):
	assert len(params) == 1
	value = params[0].eval(env).bool()
	return FALSE_VALUE if value.literal else TRUE_VALUE

builtin_env = {
	'lambda': LispValue(builtin_lambda, 'LAMBDA'),
	'write-line': LispValue(builtin_write_line, 'LAMBDA'), #print everything
	'cons':LispValue(builtin_cons, 'LAMBDA'),
	'car':LispValue(builtin_car, 'LAMBDA'),
	'cdr':LispValue(builtin_cdr, 'LAMBDA'),
	'define':LispValue(builtin_define, 'LAMBDA'),
	'let':LispValue(builtin_let, 'LAMBDA'),
	'if':LispValue(builtin_if, 'LAMBDA'),	
	'not':LispValue(builtin_not, 'LAMBDA'),
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
	# code = '(write-line "1+2=" (+ 1 2) \'(1 3))'
	# code = '(+ 1 (* 2 3) (- 8 7))'
	# code = '((lambda (a b c d) (+ a b c d)) 2 3 4 5)'
	# code = '(cons 1 (cons 2 nil))'
	# code = '(define addOne (lambda (x) (+ x 1))) (addOne 2)'
	# code = '(define (addOne x) (+ x 1)) (addOne 2)'
	# code = '(define fac (lambda (a) (if (> a 0) (* a (fac (- a 1))) 1))) (fac 3)'
	# code = '(not #t)'
	# code = ' (let ((i 1) (j 2)) (+ i j))'
	code = "a"
	# code = '(= 1 2)'
	# code = '()'
	# code = '(cdr \'(\'(* 2 3) (- 8 7)))'
	print code
	# for token, typ in lex(code):
	# 	print(token, typ)
	roots = parse(lex(code))
	for root in roots:
		print "value: " + str(eval(root))

if __name__ == '__main__':
	main()