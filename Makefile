all:
	java -cp lib/antlr-4.5.1-complete.jar org.antlr.v4.Tool -Dlanguage=Python3 Scheme.g4
