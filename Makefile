all:
	happy -gca ParZaklang.y
	alex -g LexZaklang.x
	ghc --make Interpreter.hs -o interpreter
	ghc --make TypeTest -o typeTest

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
