Control.Print.printDepth := 2000;
exception syntaxErr
structure codeLrVals = codeLrValsFun(structure Token = LrParser.Token)
structure codeLex = codeLexFun(structure Tokens = codeLrVals.Tokens);
structure codeParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = codeLrVals.ParserData
     	       structure Lex = codeLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) = 
		    	( TextIO.output(TextIO.stdOut, "Syntax Error:"^Int.toString(pos)^":"^Int.toString(pos)^":"^s) ; raise syntaxErr )
		in
		    codeParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  codeParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = codeLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = codeParser.Stream.get lexer
    in
      result
		
    end

fun read (infile:string) =
   let 
        val instream = TextIO.openIn infile
	fun loop instream =
	    String.implode(String.explode(TextIO.inputAll instream))

    in
	    loop instream before TextIO.closeIn instream
    end
open AST	

val parseString = parse o stringToLexer 
val parseFile = parse o stringToLexer o read 
val it = symbolTable

