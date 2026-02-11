all: calculator

calculator: parser.y lexer.flx
	bison -d parser.y
	flex lexer.flx
	gcc -o calculator parser.tab.c lex.yy.c -lfl

clean:
	rm -f calculator lex.yy.c parser.tab.c parser.tab.h