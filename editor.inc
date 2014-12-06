/*****************************************************************************

		 Copyright (c)  2000 -2003 RSUH, 
		 						Cafedra of Mathematics, Logic and Intellectual Systems
			

 Project:  WinEzop
 FileName: EDITOR.PRO
 Purpose: Editor
 Written by:  Eugene Beniaminov
 Comments: EZOP  is the system for  knowledge represntation 
******************************************************************************/
/*
CONSTANTS
%BEGIN_CONST Project configuration constants
  use_mdi = 1
  use_3dctrl = 1
  use_dlgpack = 1
  use_editor = 1
  use_message = 1
  use_treebrowser = 1
%END_CONST
*/
%include "editor.con"

include "types.dom"		
include "pdcrunt.dom"		
include "pdcrunt.pre"





/*****************************************************************************
			User's Declarations.
*****************************************************************************/

GLOBAL DOMAINS
  DB_SELECTOR = browselist_db; concept_dubl; concept_dubl_inter	% For treebrowser tool
  FILE = fileselector1; fileselector2; input; myfile; messages % To be edited


global domains
  zip_unzip = zip; unzip_concept; unzip_env; del_zip_concept
 
  id      =       string     %идентификатор термов внутри онтологии 
% list_id = id*
 
  idTemplate = id_t(integer IdConcept, id)
  list_idTemplate = idTemplate*
  type_msg = warning; 			err;		 did; 		         gr_an;
 	   %"Предупреждение"; "Ошибка",	 "Сделано", "Грамматический анализ", 
           calc;           approx;			rwrt;	   		diagn;	
 	   %"Вычисление", "Изменение в аппроксимации", "Правило переписывания",    "Диагностика"
 		prov
 	   %''Док."
  expr         = undef; r(real); nc(string); ex(id,expr_list)   ; v(string,expr) % локальная переменная v(Name,Type)  % c(string);  убрано
  expr_list    = expr *
  twodimlist = slist *
  meta_list = slist *
  type_msg_list = type_msg *
  w_v_t =w(string); var(expr); t(expr); v
  list_w_v_t = w_v_t*

  chlist = char *

  action = text_scanned; plates_imposed; type_checked

%*************************************	THIS FACT IS FOR DEBUGGING!	*************************************										
/*
GLOBAL DATABASE - fordebugging
	parToCheck (string)
*/	
	
%************************************************************************************************************	

/*     
GLOBAL DATABASE - edit_window
	%con_list_window(WINDOW)
	%edit_window(WINDOW,STRING FileName,STRING Data)
	count(INTEGER,STRING PreFile,STRING PostFile)
	%edwin(string,boolean)
*/


GLOBAL DATABASE - globalParams
	determ location (string) 
	determ kbname (string) 
	text_to_execute (string, string)
	determ dialog_answer (string)
	determ new_cnpt_name (string)
	single readonly_concept (string)
	determ curid (string)
	determ oldid (string)
	nondeterm my_message (string)
	determ inset(string)

	

GLOBAL DATABASE - execute_state
 done(action)
 determ	fatal_error
 determ	execute_command_error
 determ	state_interpret_well
 determ	glob_int(integer)
 determ	glob_ilist(ilist)
 nondeterm	glob_slist(slist)
 determ  glob_string(string)
 determ  mode_answer
 determ	marker_present
 determ answer_text (string, expr)  


GLOBAL DATABASE - status
 determ concept_changed
 determ current_concept_name(string)

GLOBAL DATABASE - config
 tmessage(type_msg, string Text, string On_Off) 
 determ deduce_on_approx(string On_Off)
 determ dlg_ask_do_element(string On)
 determ maxlong_deduce(integer)

GLOBAL DATABASE - message_ezp
 msg_text(integer,string)

GLOBAL DATABASE - environ
 determ environment(string FileName)
 
GLOBAL DATABASE - msg_list
 single textlist (slist)
 single typelist (type_msg_list)
 single statelist (slist)

GLOBAL DATABASE - templates
 template(integer  IdConcept,  id Id_template, string Text,     string Opname, string  Комментарий, list_w_v_t ListTemplate,   expr ResultType)
 dir_dtt(integer N_1_OR_2,string Dir,string SubDir) %распределение подпапок шаблонов по папкам
 dir_tt(integer N_1_OR_2,string Dir,  integer  IdConcept,  id Id_template) %распределение шаблонов по папкам
 determ dir_con_tt(slist)   %текущий  каталог окна списка шаблонов
 determ dir_papki_tt(slist)  %текущий каталог окна папки шаблонов


Global DATABASE - dic_windows
 dic_window(expr Type)
 dir_dt(string Dir,string SubDir) %распределение подпапок шаблонов по папкам
 dir_t(string Dir,  integer  IdConcept,  id Id_template) %распределение шаблонов по папкам
    dir_con_t(slist)   %текущий  каталог окна списка шаблонов
    dir_papki_t(slist)  %текущий каталог окна папки шаблонов

Global DATABASE - notion_base
        single  notion_base(string NameBase)

GLOBAL DATABASE - id_4_xml
  single id_4_xml (integer)
  
GLOBAL DATABASE - concept_names
 nondeterm name_file(string Name,string File, string FileEnvironment, string Defin1)
 use(integer OforUseOr1forParent,integer IdConcept, integer IdUsedConcept)
 dir_c(string Dir, string NameConcept) %распределение онтологий по папкам
 dir_dc(string Dir, string SubDir) %распределение подпапок по папкам
 determ  dir_con(slist)   %текущий каталог окна списка онтологий
 determ  dir_papki(slist)  %текущий каталог окна папки онтологий

%!!!! При изменении в concept 
%изменить term_ToConceptOrTmp из calctls.pro и changeIdInConcept в editor.pro 

GLOBAL DATABASE - concept 
 determ idConcept(integer)
 in_template(integer IdDostup_0_1_2, integer ID_Concept,id Id_template, string Text, string Opname, string Комментарий, list_w_v_t Список_шаблона,expr TypeRes)
 exec_templ(integer ID_Concept,string Opname,expr_list LcalcVar,expr TermChek,expr TermExec)
 del_template(integer IdConcept,string Opname,list_w_v_t List,expr Type)
 used_concept(integer OforUseOr1forParent,integer ID_Concept_where, integer ID_Concept_used)
 element_concept(integer ID_Concept, expr ElObject, integer RefConcept, integer ID_Concept_Type) 
 resalt_op(integer ID_Concept,expr,expr)
 rwt_rule(integer ID_C, id, string Msg,expr_list ListVar,expr Lterm,expr Rterm,expr PredCond,expr PostCond)
 descriptor(integer ID_Concept,expr,integer Вес)
 not_eq_desc(integer ID_Concept,expr,expr)     %используется только в make_equal.pro
 subobject(integer ID_Concept,expr Sub,expr Super) %пары сравнимых областей, между которыми нет нетривиальной подобласти
 not_subobject(integer ID_Concept,expr,expr)
 element(integer ID_Concept,expr Element,expr Ob) % элемент и наименьшая подобласть, содержащая этот элемент
 not_element(integer ID_Concept,expr,expr)
 current_inconsist (integer ID_Concept)
 inconsist(integer ID_Concept,expr,expr)
 defin1(integer ID_Concept,string)
 defin2(integer ID_Concept,string)
 last_cmd(integer ID_Concept,string) 

Facts - sintax  %временные данные для разбора предложения
 word_of_sentence(integer Nword, integer Nplace, integer NplaceEnd,w_v_t)
 determ restText(string)  
 determ lenText(integer) 
 determ  lastTerm(expr Term,integer NnextWord)

Facts - dictionaries
 word_dictionary(string,list_idTemplate,list_idTemplate) %слово, список номеров шаблонов1, %список номеров шаблонов2


%Anna)


/* Добавила 6.02.03 */

GLOBAL Facts - number_win_grammar
 db_win_grammar(string)

/* конец добавления */
%include "editor.pre"
%include "product.pre"


