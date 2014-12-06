/*****************************************************************************

		Copyright (c) 1984 - 2000 Prolog Development Center A/S

 FileName: CGITOOLS.PRO
 Purpose: No description
 Written by: Visual Prolog
 Comments:
******************************************************************************/

ifndef iso_prolog   


/******************************************************
	String -> List by a separator
 Ex. str2list("aa,bb,cc",',',List)
******************************************************/
include "editor.pre"
include "database.pre"
include "cgitools.pre"
include "russian.pre"


PREDICATES
/* Global predicates*/
  procedure cgi_GetString(STRING String) - (o)
  procedure lookupParm(string ParmName,parmList,string Default,string Value)-(i,i,i,o)
  procedure remove_empty_params(parmList,parmList)-(i,o)
  % Return all the body text from a HTML file
/*
  procedure STRING gethtmlfromfile(STRING FileName)-(i)
*/


  procedure str2list(STRING,CHAR Seperator,SLIST)


/*=============================================================================
                        decode_CGI_value(EncodedString,DecodedString)
        Decodes URL-encoded CGI string value
=============================================================================*/
PREDICATES
  procedure cgi_DecodeString(STRING EncodedString,STRING DecodedString)
  procedure cgi_DecodeString1(STRING EncodedString,STRING DecodedString)
  procedure cgi_DecodeString2(STRING EncodedString,STRING DecodedString)
  procedure decodelist(SLIST EncodedList,SLIST DecodedList)
  procedure char_hex(char,integer)

PREDICATES
  procedure str_namelist(STRING STR,PARMLIST)-(i,o)
  procedure getparm(STRING Str,STRING RestStr, PARM Parm)-(i,o,o)
  determ search_Pos_Nextparm(STRING Str,UNSIGNED Pos1,UNSIGNED PosNext)-(i,i,o)
  procedure str_parm(STRING Str,PARM)-(i,o)
  procedure delete_r_char (STRING In,STRING Aux,STRING Out) -  (i,i,o)

PREDICATES
  retrieve_POST_string(STRING)-(o)
  convert_to_string(INTEGER,BINARY,STRING)-(i,i,o)

PREDICATES
  retrieve_GET_string( string )

PREDICATES
  procedure get_CGI_string_by_method(STRING method,STRING cgiString)
  string_to_ilist(string, ILIST)
  ilist_to_string(ILIST, string)
  ilist_to_slist(ILIST,SLIST)
  proc_utf8(ILIST, ILIST) - (i,o)



CLAUSES


elsedef

:- module(cgitools,[
  cgi_GetParmList/1
]).

:- style_check(+string).
:- set_prolog_flag(unknown, fail).

:- use_module(vp52_compat).
:- use_module(russian).


enddef

  string_to_ilist("",[]) :- !.
  string_to_ilist(Str,[First|RestList]) :- !,
	frontchar(Str,Char,RestStr),
	char_int(Char, First),
	string_to_ilist(RestStr,RestList).

ilist_to_slist([],[]) :- !.
ilist_to_slist([Code|T], [Str|T1]) :- !,
        char_int(Char, Code),
        str_char(Str, Char),
	ilist_to_slist(T,T1).
	
   	
 
  ilist_to_string([], "") :- !.
  ilist_to_string(List, Str) :- !,
        ilist_to_slist(List, SList),
	list_to_string(Slist,"",Str).        
	
  


  str2list(Str,Seperator,[First|RestList]):-
	searchchar(Str,Seperator,Pos),
ifndef iso_prolog
	Len = Pos-1,
elsedef
	Len is Pos - 1,
enddef        
	frontstr(Len,Str,First,RestStr0),
	frontchar(RestStr0,_,RestStr),!,
	str2list(RestStr,Seperator,RestList).
  str2list(Str,_,[Str]).

  cgi_DecodeString(EncodedString,DecodedString):-
	str2list(EncodedString,'+',SLIST),
	list_to_string(SLIST," ",StringWithPlusConverted),
ifndef iso_prolog	
	cgi_DecodeString1(StringWithPlusConverted,DecodedString1),
	cgi_DecodeString2(DecodedString1, DecodedString).
elsedef	
	cgi_DecodeString1(StringWithPlusConverted,DecodedString).
enddef	 


  cgi_DecodeString1("",""):-!.
  cgi_DecodeString1(EncodedString,DecodedString):-
	str2list(EncodedString,'%',SLIST),
	SLIST = [H|T],!,
	decodelist(T,DecodedList),
	list_to_string([H|DecodedList],"",DecodedString).
  cgi_DecodeString1(_,_):-errorexit.



  proc_utf8([], []) :- !.
  proc_utf8([C], [C]) :- !.

  proc_utf8([C1|[C2|Rest]], [WinCode|T]) :-
      c1251_utf8(WinCode, C1, C2), !,
      proc_utf8(Rest, T).

  proc_utf8([C|Rest], [C|Rest1]) :- !,
      proc_utf8(Rest,Rest1).
      
   
  cgi_DecodeString2("",""):-!.
  cgi_DecodeString2(EncodedString,DecodedString):-
	string_to_ilist(EncodedString,ILIST),
	proc_utf8(ILIST, ILIST2),
	ilist_to_string(ILIST2, DecodedString),!.
  cgi_DecodeString2(_,""):-!.
  


  decodelist([],[]):-!.
  decodelist([H|T],[DecodedH|DecodedT]):-
        frontchar(H,C0,Rest1),
        frontchar(Rest1,C1,Rest),!,
        char_hex(C0,HighQu),
        char_hex(C1,LowQu),
ifndef iso_prolog
        AsciiCode = HighQu * 16 + LowQu,
elsedef
        AsciiCode1251 is HighQu * 16 + LowQu,
        c1251_utf(AsciiCode1251, AsciiCode), % convert 1251 to unicode
enddef        
        char_int(Symb,AsciiCode),
        frontchar(DecodedH,Symb,Rest),
	decodelist(T,DecodedT).
	
  decodelist(_,_):-errorexit.

  % Primitive char_to_hex convertion
ifndef iso_prolog  
  char_hex(C,HV) :-
	'0' <= C , C <= '9' ,!,
	HV = C - '0' .

  char_hex(C,HV) :-
	'A' <= C , C <= 'F' ,!,
	HV = C - 'A' + 10.

  char_hex(C,HV) :-
	'a' <= C , C <= 'f' ,!,
	HV = C - 'a' + 10.

  char_hex(_,_) :-
	errorexit.
elsedef
  char_hex(C,HV) :-
	char_type(C,  xdigit(HV)).
enddef

/*=============================================================================
	str_namelist
=============================================================================*/

  str_namelist("",[]):-!.
  str_namelist(STR,[PARM|RestList]):-
	getparm(STR,RestStr, PARM),
	str_namelist(RestStr,RestList).

  getparm(Str,RestStr,Parm):-
	search_Pos_Nextparm(Str,0,Pos),
ifndef iso_prolog
	Len = Pos - 1,
elsedef
	Len is Pos - 1,
enddef
	frontstr(Len,Str,ParmStr,RestStr0),
	frontchar(RestStr0,_,RestStr),!, % Ignore the '&'
	str_parm(ParmStr,Parm).
	
  getparm(Str,"",Parm):-!,
	str_parm(Str,Parm).
	
  getparm(_,_,_):-errorexit.
  
  search_Pos_Nextparm(Str,_Pos1,_PosNext):-  	
  	not(searchchar(Str,'&',_)),!,
  	fail.
  search_Pos_Nextparm(Str,Pos1,PosNext):-  	
  	searchchar(Str,'&',Pos),
  	frontstr(Pos,Str,_Str1,Str2),
  	fronttoken(Str2,_Name,Rest1),   % ??? нужно было бы проверить находится ли _Name в списке имен
  	frontchar(Rest1,'=',_Rest2),!, %случай, когда после & идет  имя параметра
  ifndef iso_prolog
	PosNext=Pos1+Pos.
  elsedef
	PosNext is Pos1+Pos.
  enddef
	
  search_Pos_Nextparm(Str,Pos1,PosNext):-  	
  	searchchar(Str,'&',Pos),
  	frontstr(Pos,Str,_Str1,Str2),
  	%fronttoken(Str2,_Name,Rest1),
  	%frontchar(Rest1,'=',_Rest2,)-     случай, когда после & идет не имя параметра
  	
  ifndef iso_prolog
	Pos2=Pos1+Pos,
  elsedef
	Pos2 is Pos1+Pos,
  enddef
	search_Pos_Nextparm(Str2,Pos2,PosNext).


  str_parm(Str,cgiparm(Name,Val)):-
	searchchar(Str,'=',Pos),
ifndef iso_prolog
	Len = Pos - 1,
elsedef
	Len is Pos - 1,
enddef
	frontstr(Len,Str,EncodedName,RestStr0),
	frontchar(RestStr0,_,EncodedVal),!, % Ignore the '='
	cgi_DecodeString(EncodedName,Name),
	cgi_DecodeString(EncodedVal,Val1),
	delete_r_char (Val1,"",Val). % убираются \r из значения
	
  str_parm(_,_):-errorexit.
  
  delete_r_char (Str,StrAux,StrRes):-
  	searchstring(Str,"\r",Pos),
  	L=Pos-1,
  	frontstr(L,Str,StrL,RestStr),
  	concat(StrAux,StrL,StrAux1),
	concat("\r",Str1,RestStr),!,
	delete_r_char (Str1,StrAux1,StrRes).
  delete_r_char (Str,StrAux,StrRes):-
  	concat(StrAux,Str,StrRes).	
 
  	
/*=============================================================================
 Retrieves the query string sent by the server with "POST" request method .
    If request is made with "POST" method we are to read the data from standard input.
    The data size we need to read from standard input
    is located by WEB-server in ENV variable "CONTENT_LENGTH"
=============================================================================*/
  retrieve_POST_string(CGI_String) :-
        envsymbol("CONTENT_LENGTH",LenStr),
        str_int(LenStr,Len),!,
ifndef iso_prolog        
        readblock(Len,PasteBlock),
	convert_to_string(Len,PasteBlock,CGI_String).
elsedef
	readstring(Len, CGI_String).
enddef	
	
	
  retrieve_POST_string(_) :-
	errorexit.

ifndef iso_prolog
  % Creates the string with given length from given binary
  convert_to_string(Len,Bin,Str) :-
        str_len(Str,Len),
        Src=cast(ULONG,Bin),
        Dst=cast(ULONG,Str),
        movmem(Src,Dst,Len).
enddef        


/*=============================================================================
      Retrieves the query string sent by the server with "GET" request method .
 If request is made with "GET" method the data is placed
 in the "QUERY_STRING" environment variable
=============================================================================*/
  retrieve_GET_string( CGI_String ) :-
        envsymbol("QUERY_STRING",CGI_String),!.
  retrieve_GET_string("").


/*=============================================================================
                        get_CGI_string(CGI_String)
      Retrieves the query string sent by the server .
=============================================================================*/
  get_CGI_string_by_method("POST",CGI_String) :-
        retrieve_POST_string(CGI_String),!.
  get_CGI_string_by_method("GET",CGI_String) :-
        retrieve_GET_string(CGI_String),!.
  get_CGI_string_by_method(_,_) :- 
	errorexit.

  cgi_GetString( CGI_String ) :-
        envsymbol("REQUEST_METHOD",Method),!,
        get_CGI_string_by_method(Method,CGI_String).
  cgi_GetString("").


         

ifdef iso_prolog

save_term_file(T,File) :-
    open(File,write, Stream, [close_on_abort(true)]),
    write_term(Stream, T, [quoted(true)]),
    write(Stream, '.'),
    close(Stream).

load_term_file(T,File) :-
    open(File,read, Stream, [close_on_abort(true)]),
    read_term(Stream, T, [double_quotes(string)]),
    close(Stream).

enddef

  cgi_GetParmList( ParmList ) :- % для отладки загрузка из файла
         existfile("param_cgi.txt"), 
         openread(input, "param_cgi.txt"),
         readdevice(input),
         readterm(parmList, ParmList),!,
         readdevice(keyboard),
         closefile(input).
  cgi_GetParmList([]) :- 
         existfile("param_cgi.txt"),
         readdevice(keyboard),
         closefile(input),
         fatalErrorMsg("Параметры в файле param_cgi.txt написаны неверно."),
         !. 


  cgi_GetParmList( ParmList ) :- % реальная загрузка по CGI
	cgi_GetString(CGI_String),
	str_namelist(CGI_String,ParmList),
ifdef iso_prolog
	save_term_file(ParmList, "param_cgi_out.txt")
elsedef	
        openwrite(myfile,"param_cgi_out.txt"),
        writedevice(myfile),
        write(ParmList),       % сохранение для отладки
        writedevice(screen),
        closefile(myfile)

enddef
	.

/*=============================================================================
	Return HTML from the <BODY> Section in a file
=============================================================================*/
/*
  gethtmlfromfile(FileName,BODY):-
	syspath(ExeStartupPath,_ProgName),
	filenamepath(FullName,ExeStartupPath,FileName),
	file_str(FullName,TEXT),
  	upper_lower(TEXT,LOWERTEXT),
	searchstring(LOWERTEXT,"<body",LEN1),
	frontstr(LEN1,LOWERTEXT,_,TXT1),
	searchchar(TXT1,'>',BODY_TAG_LEN),

ifndef iso_prolog
	STARTPOS = LEN1 + BODY_TAG_LEN + 1,
elsedef
	STARTPOS is LEN1 + BODY_TAG_LEN + 1,
enddef

	searchstring(LOWERTEXT,"</body>",ENDPOS),!,

ifndef iso_prolog
	LEN = ENDPOS - STARTPOS,
elsedef
	LEN is ENDPOS - STARTPOS,
enddef

	substring(TEXT,STARTPOS,LEN,BODY).
  gethtmlfromfile(_,_):-
	errorexit.
*/

/*=============================================================================
	Lookup parameter in a parameterlist; if not found return default value
=============================================================================*/

  lookupParm(_,[],Default,Default).
  lookupParm(ParmName,[cgiparm(ParmName,Value)|_],_,Value):-!.
  lookupParm(ParmName,[_|List],Default,Value):-
	lookupParm(ParmName,List,Default,Value).


/*=============================================================================
	Remove empty parameter values
=============================================================================*/

  remove_empty_params([],[]).
  remove_empty_params([cgiparm(_,"")|Rest],Filtered):-!,
	remove_empty_params(Rest,Filtered).
  remove_empty_params([H|Rest],[H|Filtered]):-
	remove_empty_params(Rest,Filtered).


