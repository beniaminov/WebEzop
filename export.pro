include "editor.pre"
include "dbctrl.pre"
include "export.pre"

predicates

determ export_ilist(ilist) 
export_error(string)
export_name_file(String,String, String, String)
export_name_files


clauses

export_ilist([H]) :- !,
	write("\""), write(H), write ("\"").
	
export_ilist([T|H]) :-
	write("\""), write(T), write ("\","),
	export_ilist(H).
	
export_ilist([]) :- !.	


export_error(Str) :-
        writedevice(Dev),
        writedevice(stderr),
	write("Error! "),
	write(Str), write("\n"),
        writedevice(Dev).
	%exit(1).	        
        
        
        
        
%
% "3244" => array ( name => "имя понятия", text => "текст понятия", "env_id" => "id среды", "used" => array ("id1", "id2", "id3") ),
%

export_name_file("",_, _, _) :- !,
	export_error("Empty Name"). 
	
export_name_file(_,"", _, _) :- !,
	export_error("Empty File"). 

export_name_file(_,_, "", _) :- !,
	export_error("Empty Env"). 

export_name_file(_,_, _, "") :- !,
	export_error("Empty Text"). 

export_name_file(Name,File, Env, Text) :- !,
	%name_file(Name,File, Env, Text),
	str_int(File, Id),
	write("\""), write(File), 
	write ("\" => array ( \"name\" => \'"), write(Name), write("\',"),
	write(" \"env_id\" => \""), write(Env), write("\","),
	write(" \"used\" => array ("), 
		findall(IdUsedConcept, use(_, Id, IdUsedConcept), T),
		export_ilist(T),
	write("),"),
	%encode_endlines(Text, Text1), 
	write(" \"text\" => <<<EOD\n"),write(Text), write("\nEOD\n"),
	
	write("),\n").
	

export_name_files :- 
	name_file(Name,File, Env, Text),
	export_error(Name),
	export_name_file(Name,File, Env, Text),
	fail.

export_name_files.



run_export :- !,
	load_message_ezp,
	load_config, 
	%open_msg_file,
	consult("kernel.tmt",templates),
 	filenamepath(Path, "BaseExample", "BaseExamples.ezp"),
	open_notion_base(Path),	

	write("exporting data...\n"),
	writedevice(Dev),
        openwrite(myfile,"ezop_data.php"),
        writedevice(myfile),
        write("<?php\n$ezop = array(\n"),
        export_name_files,
        !,
        write(");\n?>"),
        flush(myfile),
        closefile(myfile),
        writedevice(Dev),
	write("done...\n").
	
run_export :- !,
	write("run_export failed"),
	exit.	

