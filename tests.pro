
ifndef iso_prolog

include "database.pre"
include "impose.pre"
include "tests.pre"

clauses
elsedef

:-module(tests,[
run_parser_tests/0
]).

:- style_check(+string).
:- use_module(database).
:- set_prolog_flag(double_quotes, string).

enddef

run_parser_tests :-
write("runing parser tests..."),
consult("kernel.tmt",templates), %загрузка шаблонов
consult("init.ntn", concept), %загрузка основной среды
write("1+2?  "),
  impose_all("1+2?",Term,Type),
write(Term),nl.
