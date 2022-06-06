structure Tokens= Tokens
  exception invalidTokenErr
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token
  val line = ref 1
  val column = ref 0
  val pos = ref 1
  val eof = fn () => Tokens.EOF(!line, !column)
  val error = fn (e,l,c) => (
  print("Error at " ^  (Int.toString l) ^ " : " ^ (Int.toString l) ^ " with the error message: Error at " ^ e ^ "\n");
  raise invalidTokenErr
  )
  
%%
%header (functor codeLexFun(structure Tokens:code_TOKENS));
letter=[A-Za-z];
digit = [0-9];
ws = [\ \t];
sign = [+~];
%%
\n       => (line := (!line) + 1;
            column := 0;
            pos := (!pos) + 1; lex());
{ws}+    => (lex());
"~" => (column := (!column) + size yytext;
           
            Tokens.NEGATE(!line,!column));
"="    => (column := (!column) + size yytext;
        
            Tokens.EQUAL(!line,!column));
"<"    => (column := (!column) + size yytext;
          
            Tokens.LESS(!line,!column));   
">"    => (column := (!column) + size yytext;
         
            Tokens.GREAT(!line,!column));  
"<="    => (column := (!column) + size yytext;
    
            Tokens.LESSEQ(!line,!column));   
">="    => (column := (!column) + size yytext;
  
            Tokens.GREATEQ(!line,!column)); 
"<>"    => (column := (!column) + size yytext;
      
            Tokens.BOOLSYMBOL(!line,!column)); 
"+"    => (column := (!column) + size yytext;
       
            Tokens.PLUS(!line,!column));
"-"    => (column := (!column) + size yytext;
         
            Tokens.MINUS(!line,!column));
"*"    => (column := (!column) + size yytext;
          
            Tokens.MUL(!line,!column));
"/"    => (column := (!column) + size yytext;
         
            Tokens.DIV(!line,!column));
"%"    => (column := (!column) + size yytext;
            
            Tokens.REM(!line,!column));
"if"    => (column := (!column) + size yytext;
         
            Tokens.IF(!line,!column));
"then"    => (column := (!column) + size yytext;
          
            Tokens.THEN(!line,!column));
"else"    => (column := (!column) + size yytext;
           
            Tokens.ELSE(!line,!column));

"int"    => (column := (!column) + size yytext;
         
            Tokens.INT(!line,!column));
"bool"    => (column := (!column) + size yytext;
           
            Tokens.BOOL(!line,!column));
":"    => (column := (!column) + size yytext;
     
            Tokens.LISTYPE(!line,!column));
"::" => (column := (!column) + size yytext;
       
            Tokens.COLON(!line,!column));
":=" => (column := (!column) + size yytext;
            
            Tokens.ASSIGN(!line,!column));

"tt"    => (column := (!column) + size yytext;
            
            Tokens.TT(!line,!column)); 
"ff"    => (column := (!column) + size yytext;
           
            Tokens.FF(!line,!column));
"!"    => (column := (!column) + size yytext;
          
            Tokens.NOT(!line,!column));
"&&"    => (column := (!column) + size yytext;
             
            Tokens.AND(!line,!column));
"||"    => (column := (!column) + size yytext;
             
            Tokens.OR(!line,!column));
"("      => (column := (!column) + size yytext;
         
            Tokens.LEFTP(!line,!column));
")"      => (column := (!column) + size yytext;
           
              Tokens.RIGHTP(!line,!column));
"{"      => (column := (!column) + size yytext;
            
            Tokens.BRACEL(!line,!column));
"}"      => (column := (!column) + size yytext;
       
              Tokens.BRACER(!line,!column));
";"      => (column := (!column) + size yytext;
         
              Tokens.TERM(!line,!column));
","      => (column := (!column) + size yytext;
        
              Tokens.COMMA(!line,!column));
"program"=> (column := (!column) + size yytext;
           
            Tokens.PROGT(!line,!column));
"var" =>  (column := (!column) + size yytext;
      
            Tokens.VAR(!line,!column));
"read" =>  (column := (!column) + size yytext;
          
            Tokens.READ(!line,!column));
"write" =>  (column := (!column) + size yytext;
           
            Tokens.WRITE(!line,!column));
"endif" =>  (column := (!column) + size yytext;
           
            Tokens.ENDIF(!line,!column));
"while" =>  (column := (!column) + size yytext;
           
            Tokens.WHILE(!line,!column));
"do" =>  (column := (!column) + size yytext;
      
            Tokens.DO(!line,!column));
"endwh" =>  (column := (!column) + size yytext;
       
            Tokens.ENDWH(!line,!column));

{sign}?{digit}+ => (column := (!column) + size yytext;
                
            Tokens.NUM(valOf(Int.fromString(yytext)),!line,!column));
{letter}(({letter}|{digit})*) => (column := (!column) + size yytext;
            
            Tokens.ID(yytext,!line,!column));            
.     => (error (yytext,!line,!column);
             lex());
