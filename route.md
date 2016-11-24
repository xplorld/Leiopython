lexer:
	"(", ")", "'", "[a-zA-Z]\w*", "\d+", "\""
parser:
	program = stack of bracket
	bracket = "(" element+ ")"
	element = token | number | string
	string = "\"" anything "\""
interpreter:
	inside-out: from deepest
	if quote pass else eval
	need some API: 
		print
		define, need some symbol table?
REPL:
	well, just REPL
have fun!

Etymology:
	Leiopython is a kind of white-lipped python in New Guinea.