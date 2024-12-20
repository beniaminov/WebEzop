
ifndef iso_prolog

include "editor.pre"
include "calc.pre"
include "impose.pre"
include "dbctrl.pre"
include "make_eq.pre"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
%include "form\\make_eq.pro"
/******************************************************/
/*     ���������  make_equal ������ ������� ���       */
/*  ex-���������  � ����������� ����������� ��������� */
/*    ������������ � ��������� �� ����������������    */
/*                                                    */
/*    ��������� make_not_equal ���������� ��������    */
/*      ����������� ���� ��������� � ��������� ��     */
/*               ����������������                     */
/******************************************************/
 /* � ���� ������ ���������:                                                                               */
 /*  resalt_op(Expr,Desc) -  ������������ ex(Op,Desclist) -> Desc ;         */
 /* eq_desc(Desc1,Desc2) - �������� �� ������������,�.�. ������ Desc1 �� Desc2 (� ����� �������� ������)   */
 /*                        Desc1>Desc2;                                                                    */
 /* not_eq_desc(Desc1,Desc2) -  �� ������ ����������� �� ���� ����������;                                  */
 /* modificatin(Ex1,Ex2) - ��������������� ��������, �������� ���������� ������ resalt_op (� ����� ������);*/
 /* descriptor(Desc,Weight) - ����������� � ������;                                                        */

%include "editor.inc"

                       

          
 
 PREDICATES

member(expr,expr_list) - (i,i)
member(pair,pair_list) - (i,i)
member(string, slist) - determ (i,i)
member(integer, ilist) - determ (i,i)


 determ maxlong(integer) %M����������� ����� ������ � replace. �� ��������� 100.


%order_expr(expr,expr,expr,expr) (i,i,i,i)
 
 determ order_desc(expr,expr,expr,expr)
         /* factor_set_desc - ����������� ����������� 
         ��������� ������������ �� �������� ���������������,
         ������������ ���������� eq_desc � ���������� ��������� 
         ����������� �� ������ ���������� ��� �������� �������� �� resalt_op */  
  factor_set_desc
         /*  replace_in_resalt_op(D1,D2) - ��������  � resalt_op(ex(Op,Arg),D) 
         ���������� D1 �� D2, ���� D1 ������ � Arg ��� D,
         � ���������� ��������� � modification_resalt_op */     
  replace_in_resalt_op(expr D1,expr D2)
         /* replace_in_descriptor(D1,D2) - �������� � descriptor(D,W) 
          ���������� D1 �� D2, ���� D1 ������ � D,
         � ���������� ��������� � modification_resalt_op */
  replace_in_descriptor(expr D1,expr D2)
  memberEx(expr,expr_list) - determ (i,i)
         /* replace_in_eq_desc(D1,D2) - �������� � eq_desc D1->D2,
         �� ����� eq_desc(D,D)*/
  replace_in_eq_desc(expr D1,expr D2)   
   /*  replace_in_list(Desclist,RDesclist,D1,D2) - �������� 
         � Desclist   D1 ->D2 */
  %replace_in_list(expr_list,expr_list Res,expr D1,expr D2) 
         /* new_equalities - ��������������� ����������� ����� 
                  �� modification_resalt_op �,  ���� ������� ����� � ���� �� 
                  ����������� � resalt_op � modification_resalt_op,
                  �� ��������� ��������� eq_desc  */
  	new_equalities
         /* assert_list_in_eq_desc(Desclist,D) - ��������� � eq_desc 
         ������������������� ���� (D1,D),��� D1 �� Desclist */
  	
  	assert_list_in_eq_desc(expr_list,expr)
  	assert_in_eq_if_not(expr,expr) /* ��������� � eq_desc, ���� ����� � ��� ��� */
         /* min_desc(Desclist,Dmin) ������� ����������� ����������
          � �������� ������ Desclist */
  	min_desc(expr_list,expr)
         /* ��������� ��� ������ � not_eq_desc */           
 	 replace_in_not_eq_desc(expr,expr)
  	replace_not_message(expr,expr)
  	message_make_equal(expr,expr)
  	message_inconsist(expr,expr,string)
 
	 make_subobject_desc(expr,expr) -determ (i,i)
	 make_equal_1(expr,expr,expr,expr)  -determ (i,i,i,i)
 	make_equal_desc(expr,expr)  -determ (i,i)
	accord(expr Ob1,expr Ob2, expr SupOb) -determ (i,i,o)  %������������ ����� Type � Ob
	ask_synonym_desc(expr,expr)-determ (i,i)
	ask_do_element(expr,expr, expr)  - determ (i,i,i)
	getElement_NotIn_OnApprox(expr,expr, expr)   - determ (i,i,o)
		/* ��������� replace_in_subobject  */
nondeterm replace_in_subobject(expr,expr)
  	find_all_equal_desc(expr,expr,expr_list)
	replace_list_in_subobject(expr_list,expr)
  	%delete_element(expr,expr)  % ���� D2 ��� Dmin - ��������,Dmin 
                                     % ���������� �� subobject ��� ��������� 
		/* ��������� replace_in_element */
 	replace_in_element(expr,expr)
  	repl_arg1_element(expr,expr)
  	is_not_eq_desc(expr,expr)- (i,i)
	is_not_nil(expr)
  	inconsist_if_two_elements_in(expr)
  	message_eq_with_point(expr,expr_list,expr)  
  	rewrite(string,expr,expr)    
  	pairlist_to_lists(pair_list, expr_list, expr_list) - (i,o,o)
  	unificate(expr Term,expr TermWithVar, pair_list AuxList, pair_list ResList) - (i,i,i,o)
  	unificate_list(expr_list Term,expr_list TermWithVar, pair_list AuxList, pair_list ResList) - (i,i,i,o)
  	member_var(expr Var, pair_list) - (i,i)
  	modi_subobject
  	
   	is_subobject_desc(expr,expr) -(i,i)
  	is_element_desc(expr,expr) -(i,i)
	getSubob_NotSubob_OnApprox(expr,expr, expr) - (i,i,o) % ������ ���������� ������� ���������, ������� ����� �� ���������� ������� 
	make_element_desc(expr,expr) -(i,i)
	%create_new_constant(string Name,expr Ob,expr Resalt)   -(i,i,o)          %������� ����� ������� � ������� Ob
nondeterm  immed_element(expr,expr)
nondeterm  immed_element_list(expr,expr_list)
   	clear_in_bigger(expr,expr)
   	%msg_make_equal_el_ob(expr,expr)
 	 any_two_elements(expr,expr,expr)  /*  any_two_elements(Ob,Elmnt1,Elmnt2) */       
        eq_known(expr,expr) /* ��������� ��������� */
             /* rewrite(NAME,LEXPR,REXPR) */
        reduce_list(integer,expr_list,expr_list,expr_list) - determ (i,i,i,o)
      turn_list(expr_list,expr_list,expr_list)  - determ (i,i,o)
nondeterm message_deduce(expr,expr)
deduce_eq(expr,expr)
   minob_of_element(expr El,expr Minob) - (i,o)
  intersect_list(expr_list,expr,expr) - (i,i,o)
nondeterm  members(expr,expr_list)
  
  %dlg_note_ex(expr) - (i) %������������ ��� ������� !!!!!
  
   /*********************************************/           
   /*********************************************/
   /*           ������� �����������             */
   /*      rewrite(Name,Lex,Rex)  �����������   */
   /*      ������ ������ ��������� ������       */
   /*********************************************/
   /*********************************************/

append(slist,slist,slist) - (i,i,o)
append(expr_list,expr_list,expr_list) - (i,i,o)



 CLAUSES
 
elsedef

:- module(make_eq, [
create_new_constant/3, %(expr Name,expr Ob,expr Resalt)   -(i,i,o) %(i,i,r(o))         %������� ����� ������� � ������� Ob
replace_in_list/4, %(expr_list,expr_list Res,expr D1,expr D2)  -(i,o,i,i)   /*  replace_in_list(Desclist,RDesclist,D1,D2) - �������� � expr_list   D1 ->D2 */
reduce/3, %(integer,expr,expr) - determ (i,i,o)
make_not_equal/2, %(expr,expr) - (i,i)
order_expr/4, %(expr,expr,expr,expr) - (i,i,i,i) (i,i,o,o)
make_element/2, %(expr,expr) - (i,i)
is_equal/2, %(expr,expr) - (i,i)    %�� �������������;  ������������ �����  � cateval, basiq_equalities
make_equal/2, %(expr,expr) - (i,i)
is_subobject/2, %(expr,expr) - determ (i,i)
make_subobject/2, %(expr,expr) - (i,i)
make_inclusion/3, %(expr,expr,expr) - (i,i,i)
is_equal_not_message/2, %(expr,expr) - (i,i)
isomor_with_point/1, %(expr) - (i)
between/3, %(expr,expr,expr) - (i,i,o)
is_element/2, %(expr,expr) - (i,i)
element_of_object/2 %(expr,expr) - (o,i)
]).
:- style_check(+string).
:- use_module(vp52_compat).
:- use_module(editor).
:- use_module(calc).
:- use_module(impose).
:- use_module(dbctrl).
:- use_module(db).


%include "editor.pre"


enddef 
 

append([],L,L):-!.
append([H|T1],L,[H|T2]):-append(T1,L,T2).

 
member(H,[H|_]):-!.
member(H,[_|T]):-
	member(H,T).
 
 
   /*         ����������� ������� ���������      rul */
            
       rewrite("������",ex("()",[Ex]),Ex):-!.
   
   
    rewrite("���������� ������",
    			ex("intersect",[Ob1,Ob2]),
    			Ob1):-
    			is_subobject(Ob1,Ob2),!.
    rewrite("���������� �����",
    			ex("intersect",[Ob1,Ob2]),
    			Ob2):-
    			is_subobject(Ob2,Ob1),!.
       			
     rewrite("��������������� �����������",
      			ex("intersect",[Ob1,Ob2]),
      			ex("intersect",[Min,Max])) :-
				calc_on_approx(Ob1,Desc1),calc_on_approx(Ob2,Desc2),
				order_desc(Desc1,Desc2,Max,Min),Desc1=Max,!.			
    rewrite("��������������� �����������",
      			ex("intersect",[ex("intersect",[Ob1,Ob2]),Ob3]),
      			ex("intersect",[Ob1,ex("intersect",[Ob2,Ob3])])) :-!.
	rewrite("������������ ���� �����������",
      			ex("intersect",[Ob1,ex("intersect",[Ob2,Ob3])]),
      			ex("intersect",[Min,ex("intersect",[Max,Ob3])])) :-
				calc_on_approx(Ob1,Desc1),calc_on_approx(Ob2,Desc2),
				order_desc(Desc1,Desc2,Max,Min), Desc1=Max,!.
		

      
       rewrite("������� ����������� �������������",
               ex(dom,[ex(id,[T])]),
               T):- !.
       rewrite("������� �������� ��������������",
               ex(cod,[ex(id,[T])]),
               T):- !.
                  
       rewrite("������� ����������� ����������",
               ex(dom,[ex(com,[_,F2])]),
               ex(dom,[F2])):- !.
       rewrite("������� �������� ����������",
               ex(cod,[ex(com,[F1,_])]),
               ex(cod,[F1])):- !.             
               
       rewrite("���������� � ������������� ������",
               ex(com,[ex(id,[_]),F]),
               F):- !.
       rewrite("���������� � ������������� �����",
               ex(com,[F,ex(id,[_])]),
               F):- !.
               
       rewrite("��������������� ����������",
               ex(com,[ex(com,[F1,F2]),F3]),
               ex(com,[F1,ex(com,[F2,F3])])):- !.
               
               
     /*  �����������  ��������� � ���������  ��������  */
     
       rewrite("������� ����������� ����������� � �����",
                ex(dom,[ex(fin,[T])]),
                T):- !.
       rewrite("������� �������� ����������� � �����",
                ex(cod,[ex(fin,[_])]),
                ex(final,[])):- !.
       rewrite("�������������� ����������� � �����",
                Exmor,
                ex(fin,[ex(dom,[Exmor])])):-          
                               calc_type(Exmor,T),
                               calc(ex("mor",[]),Mor),
                               T=Mor,
                               not(Exmor = ex(fin,[_])),
                               eq_known(ex(cod,[Exmor]),ex(final,[])),
                               !.
                


     /*  �����������  ��������� � �����������  ��������  */
     
       rewrite("������� �������� ����������� �� �����",
                ex(cod,[ex(init,[T])]),
                T):- !.
       rewrite("������� ����������� ����������� �� �����",
                ex(dom,[ex(init,[_])]),
                ex(initial,[])):- !.
       rewrite("�������������� ����������� �� �������",
                Exmor,
                ex(init,[ex(cod,[Exmor])])):-          
                               calc_type(Exmor,T),
                               calc(ex("mor",[]),Mor),
                               T=Mor,
                               not(Exmor = ex(init,[_])),
                               not(Exmor = ex(fin,[ex(initial,[])])),
                               eq_known(ex(dom,[Exmor]),ex(initial,[])),
                               !. 



     /*  ����������� ��������� � �������������      */
     
       rewrite("������� ����������� �������� 1",
               ex(dom,[ex(proj1,[X,Y])]),
               ex(prod_ob,[X,Y])):- !.
       rewrite("������� �������� �������� 1",
               ex(cod,[ex(proj1,[X,_])]),
               X):- !.
               
       rewrite("������� ����������� �������� 2",
               ex(dom,[ex(proj2,[X,Y])]),
               ex(prod_ob,[X,Y])):- !.
       rewrite("������� �������� �������� 2",
               ex(cod,[ex(proj2,[_,Y])]),
               Y):- !.
               
      rewrite("���������� ������������ ����������� � ��������� 1",
              ex(com,[ex(proj1,[X,Y]),ex(prod_mor,[F,G])]),
              F) :-
                    eq_known(X,ex(cod,[F])),        
                    eq_known(Y,ex(cod,[G])),
                    !.
      rewrite("���������� ������������ ����������� � ��������� 2",
              ex(com,[ex(proj2,[X,Y]),ex(prod_mor,[F,G])]),
              G) :-
                    eq_known(X,ex(cod,[F])),        
                    eq_known(Y,ex(cod,[G])),
                    !.  

                          
     rewrite("������������ �������� ��������� ������������",
              ex(prod_mor,
                          [ex(com,
                                  [ex(proj1,[X,Y]),
                                  F]),
                           ex(com,
                                  [ex(proj2,[X,Y]),
                                  F])]),
              F) :-
                    eq_known(ex(prod_ob,[X,Y]),ex(cod,[F])),
                    !.
   
    rewrite(Msg,Ex,Res):-
        rwt_rule(_,_,Msg,_Vars,Lterm,Rterm,PreCond,PostCond),  %������� � ����� �����
    	unificate(Ex,Lterm,[], Pairs_Var_Term), 
  	pairlist_to_lists(Pairs_Var_Term, VarList, TermList),
  	replace_var_on_ex(TermList, VarList, PreCond, PreCondTerm),
  	calc(PreCondTerm,PreCondDesc),
  	calc_on_approx(ex("true_bool",[]), DescTrue),
  	PreCondDesc=DescTrue,
  	replace_var_on_ex(TermList, VarList, Rterm, Res),
  	replace_var_on_ex(TermList, VarList, PostCond, PostCondTerm),
  	calc(PostCondTerm,PostCondDesc),
  	PostCondDesc=DescTrue,!.
  
  
  
  unificate(Term,v(X,TypeX),AuxList,AuxList):-
  	member(v_t(v(X,TypeX),Term), AuxList),!,
  	type(Term,TypeTerm),
  	is_subobject(TypeTerm,TypeX).
  unificate(Term,v(X,TypeX),AuxList, [v_t(v(X,TypeX),Term)|AuxList]):-
  	not(member_var(v(X,TypeX), AuxList)),!,
  	type(Term,TypeTerm),
  	is_subobject(TypeTerm,TypeX).
  unificate(r(R),r(R),AuxList,AuxList):-!.
  unificate(nc(STR),nc(STR),AuxList,AuxList):-!.
  unificate(ex(Op,ArgList),ex(Op,ArgListRule),AuxList,ResList):-
  	unificate_list(ArgList,ArgListRule, AuxList, ResList).
  	
  unificate_list([],[], ResList, ResList):-!.
  unificate_list([H|T],[HRule|TRule], AuxList, ResList):-
  	unificate(H,HRule, AuxList, ResList1),
	unificate_list(T,TRule, ResList1, ResList).
 
 member_var(Var, [v_t(Var,_)|_]):-!.
 member_var(Var, [_|T]):-
	 member_var(Var, T).
 
 
 
  pairlist_to_lists([], [], []):-!.
  pairlist_to_lists([v_t(Var,Term)|T], [Var|TVarList],[Term|TTermList]):-
  	pairlist_to_lists(T, TVarList, TTermList).
  
  
  
 % dlg_note_ex(Ex):-expr_to_str(Ex,Str),dlg_note(Str).   %������������ ��� ������� !!!!!  ����� ��������
  
  any_two_elements(Ob,ex(X,[]),ex(Y,[])):-!,bound(Ob),
             element_of_object(X1,Ob), X1=ex(X,[]),
             element_of_object(Y1,Ob),Y1=ex(Y,[]),
             not(X=Y),!.
  
  /* ��������� ���������� make_subobject_desc, is_subobject_desc, subobject
                 ������ ���� ������������� */
   
  /*    is_subobject ��������, ���� ��������� subobject */
  /*    �� �������� ������ ��� � �� ����� ������        */                
   
is_subobject_desc(D,D):-!. /* ���� A,B - ������ ������� */
             %D1=D2,!.
is_subobject_desc(ex("initial",[]),_):-!.   /* ����� ���������� �� ���� */
/*             
is_subobject_desc(D1,_):-   
             calc_on_approx(ex("initial",[]),Desc),
             D1=Desc,!.
*/
is_subobject_desc(Desc1,Desc2):-
            subobject(_,Desc1,Desc2),!. /*Desc1 ���������������� ��������� Desc2*/
     %!!!!!!! ���������� � ������ ������, ��� � road
is_subobject_desc(Desc1,Desc2):-
            subobject(_,Desc1,D), 
            is_subobject_desc(D,Desc2),!.
       
is_subobject(A,B):-
	is_equal(B,ex("term",[])),
	type(A,TypeA),
	is_subobject(TypeA,ex("ob",[])),!.
is_subobject(A,B):-
            calc_on_approx(A,Desc1),
            calc_on_approx(B,Desc2),
            is_subobject_desc(Desc1,Desc2),!. 
            
make_subobject(nc(Name),B):-!,
	  create_new_constant(nc(Name),ex("ob",[]),Res),
	  make_subobject(Res,B).
make_subobject(A,B):-  
             calc(A,Desc1),
             calc(B,Desc2),
            isa_calc_type(Desc1,ex("ob",[])),
            isa_calc_type(Desc2,ex("ob",[])),
            make_subobject_desc(Desc1,Desc2),!.
make_subobject(A,B):-
            not(isa_calc_type(A,ex("ob",[]))),!,
            calc_type(A,TypeA),
            retractall(fatal_error),
    	    assert(fatal_error),
    	    expr_to_str(TypeA,StrTypeA),
    	    expr_to_str(A,StrA),
    	    expr_to_str(B,StrB), 
    	  %  expr_to_str(ex("ob",[]),StrOb), 
    	   %format(Prompt,"�� ���� ������� % ����������� %, ��� ��� %  ����� ��� % � �� �������� ��������.",StrA,StrB,StrA,StrTypeA),
    	   msg_n(err,22,[StrA,StrB,StrA,StrTypeA],b_false),
    	   fail.
make_subobject(A,B):-
            not(isa_calc_type(B,ex("ob",[]))),!,
            retractall(fatal_error),
    	    assert(fatal_error),
    	    expr_to_str(A,StrA),
    	    expr_to_str(B,StrB), 
    	    expr_to_str(ex("ob",[]),StrOb), 
    	   %format(Prompt,"�� ���� ������� % ����������� %, ��� ��� % �� ����� ��� %.",StrA,StrB,StrB,StrOb),
    	   msg_n(err,23,[StrA,StrB,StrB,StrOb],b_false),
    	   fail.
make_subobject(A,B):-
	    retractall(fatal_error),
    	    assert(fatal_error),
    	   expr_to_str(A,StrA),
    	   expr_to_str(B,StrB), 
    	   %format(Prompt,"�� ���� ������� % ����������� %.",StrA,StrB),
    	   msg_n(err,24,[StrA,StrB],b_false),
    	   fail.
            
make_subobject_desc(A,B):-  /* ��� ���������� */
             is_subobject_desc(A,B),!.
%Begin  ������, ����� A ������ ������� ����������� B.           
 make_subobject_desc(A,B):-    
     	    not_subobject(_,A,B),!,
	   expr_to_str(A,StrA),
    	   expr_to_str(B,StrB), 
    	   %format(Prompt,"��������, ��� % �� ����� ���� ����������� ������� %.",StrA,StrB),
    	   msg_n(warning,25,[StrA,StrB],b_false),
    	   fail.
make_subobject_desc(A,B):-  %  � �������������  � A ���� ����������, ������� �� ����� ����������� � B.
      	getSubob_NotSubob_OnApprox(A,B, Subob),!,   
    	expr_to_str(A,StrA),
    	expr_to_str(B,StrB), 
    	expr_to_str(Subob,StrSubob), 
    	%format(Prompt,"% ���������� % � �� ����� ���� ����������� ������� %.",StrSubob,StrA,StrB),
    	msg_n(warning,26,[StrSubob,StrA,StrB],b_false),
    	fail.
make_subobject_desc(A,B):-  %  � ������������� ���� ������� � A, ������� �� ����� ������������ B.
      	getElement_NotIn_OnApprox(A,B, El),!,   
    	expr_to_str(A,StrA),
    	expr_to_str(B,StrB), 
    	expr_to_str(El,StrEl), 
    	%format(Prompt,"�������  % ����������� % � �� ����� ������������ ������� %.",StrEl,StrA,StrB),
    	msg_n(warning,27,[StrEl,StrA,StrB],b_false),
    	fail.
%End  ������, ����� A������ ������� �����������.
    
make_subobject_desc(A,B):-   /*  B - ���������� A. ������������� A � B �����. */
             is_subobject(B,A),!,
             make_equal(A,B).     
make_subobject_desc(A,B):-  /*  � ������������� ���� ����������� A � B. */
            calc_on_approx(ex("intersect",[A,B]),Dintersekt),!,
             make_equal(A,Dintersekt).
make_subobject_desc(Desc1,Desc2):-  /* �������� ������ */
            idconcept(Id),
            assert(subobject(Id,Desc1,Desc2)),
             expr_to_str(Desc1,StrDesc1),
             expr_to_str(Desc2,StrDesc2),
            %format(Prompt, "������� % �����  ����������� ������� %.", StrDesc1, StrDesc2),
            msg_n(approx,39,[StrDesc1, StrDesc2],b_false),
             modi_subobject,
             !.
             
     modi_subobject:-
            subobject(_,Desc1,Desc2),
            idconcept(Id),
            retractall(subobject(Id,Desc1,Desc2)),
            not(is_subobject_desc(Desc1,Desc2)),
            idconcept(Id),
            asserta(subobject(Id,Desc1,Desc2)),
            fail.
     modi_subobject.
        
     make_inclusion(A,J,B):-
           make_subobject(A,B),
           calc(ex("inclusion",[A,B]),I),
           make_equal(J,I). 

getSubob_NotSubob_OnApprox(A,B, Subob):-
             not_subobject(_,Subob,B),
             is_subobject_desc(Subob,A),!.
             
getElement_NotIn_OnApprox(A,B, El):-
	not_element(_,El, B),
	is_element(El,A),!.
/***************     ��������� make_elements, is_element   ***************/
/* � ��������� ������������ �������� ��  element(DescElement,DescObject),*/
/* ���������� ��������������� �������������� (��� ������������� ��������),*/
/* � not_element(DescElement,DescObject), ����������� ���� ��������� ������� .*/
/*************************************************************************/
               
  %������������ ����� Type � Ob         
accord(Type,Ob,Type):-is_subobject(Ob,Type),!.
accord(Type,Ob,SupOb):-
		subobject(_,Type,X),
		accord(Ob,X,SupOb),!.
		  
ask_do_element(DescEl,TypeEl, DescB):-
	    expr_to_str(TypeEl,StTEl),
            expr_to_str(DescEl,StEl),
            expr_to_str(DescB,StB),
            format(Prompt1,"��������� \"%\"  ����  % �������� ��������� ������� %.",StEl,StTEl,StB),
	    msg_n(warning,28,[StEl,StTEl,StB],b_false),
	    dlg_ask_do_element("on"),!,
	    msg_text(28,Prompt1),!,
	    format(Prompt,"%<BR>����������?",Prompt1),
	    dlgAsk_call(Prompt, Bool),
	    Bool = "true".
	    %resp_default= dlg_Ask(Prompt,["&��","&���"]).
ask_do_element(_,_,_).
  		 
%create_new_constant(Name,Ob)             ������� ����� ������� � ������� Ob
create_new_constant(El,B,_Res):-
	    not(isa_calc_type(B,ex("ob",[]))),!,
	    calc_type(B,Type),
	    retractall(fatal_error),
    	    assert(fatal_error),
    	   expr_to_str(Type,StrType),
    	   expr_to_str(B,StrB), 
    	   expr_to_str(El,StrEl),
    	   %format(Prompt,"�� ���� ������� % ��������� ����� \"%\" ���� %, \n��� ��� % �� �������.",StrEl,StrB,StrType,StrB),
    	   msg_n(err,29,[StrEl,StrB,StrType,StrB],b_false),
    	   fail.
create_new_constant(El,B,Res):- % El ��� �������� � ������� B.
	  not(El=nc(_)),
	  isa_calc_type(El,B),!,
	  calc(El,Res).
create_new_constant(nc(Name),_Ob,_Res):-
        not(fronttoken(Name,_,_)),!,
        %msg(err,"��� �������� �� ����� ���� ������."),
        msg_n(err,30,[],b_false),
        fail.
create_new_constant(nc(Name),Ob,_Res):-
         impose_all_not_message(Name,Term,Type),
         accord(Type,Ob,_SupOb),  %������������ ����� Type � Ob
         bound(Term),!,
         calc(Term, _Res).
	 %expr_to_str(Ob,StrOb),
	% expr_to_str(SupOb,StrSupOb),
         %format(Str, "��������� \"%\" ��� ���� � ������� \"%\", ������� �������� ������� \"%\".",Name,StrSupOb,StrOb),
          %msg_n(err,31,[Name,StrSupOb,StrOb],b_false),
          %fail.
create_new_constant(nc(Name),Type,ex(Op,[])):-!,
	calc(Type,T),
	upper_lower(Name, LowerName),
	scan(LowerName,List),
	listStr_listWord(List,WList),
	idconcept(IdC),
	str_int(SIdC,IdC),
	gen_term_num(IdN),
	str_int(Id,IdN),
	concat("c",SIdC,S1),
	concat(S1,".",S2),
	concat(S2,Id,Op),
	expr_to_str(T,StrT),
	format(Str,"������� ���� %",StrT),
	assert(in_template(1,IdC,Id,Name,Op,Str,WList,T)),
	assert(descriptor(IdC,ex(Op,[]),1)).

	
           
make_element(El,B):-is_element(El,B),!. % ��� �������.
make_element(El,B):-
	    not(isa_calc_type(B,ex("ob",[]))),!,
	    calc_type(B,Type),
	    retractall(fatal_error),
    	    assert(fatal_error),
    	     expr_to_str(Type,StrType),
    	   expr_to_str(El,StrEl),
    	   expr_to_str(B,StrB), 
    	   %format(Prompt,"�� ���� ������� \"%\" ��������� ����� \"%\" ���� %, \n��� ��� % �� �������.",StrEl,StrB,StrType,StrB),
    	   msg_n(err,32,[StrEl,StrB,StrType,StrB],b_false),
    	   fail.
 make_element(Elmt,Ob):-     /* Ob �����. ���������� ������� */
         is_equal(Ob,ex("initial",[])),!,
         retractall(fatal_error),
    	 assert(fatal_error),
        expr_to_str(Elmt,St1),
        expr_to_str(Ob,St2),
         %format(Prompt,"������� ������� % ��������� ������ ������� \"%\".",St1,St2),
         msg_n(err,33,[St1,St2],b_false),
         fail.
make_element(nc(Name),Ob):-!,    %����� �������.
	create_new_constant(nc(Name),Ob,Res),
	calc(Ob,DescOb),
	make_element_desc(Res,DescOb).
make_element(El,B):-   %������� B - ���������� ������� ���� �������� El. ������!
            calc(El,DescEl),
            calc(B,DescB),
            calc_type(DescEl,TypeEl),
            is_subobject(DescB, TypeEl),
            make_element_desc(DescEl,DescB),!.
make_element(El,B):- %������������ ������. �� ��������� �������� ���������.
            calc(El,DescEl),
            calc(B,DescB),
            calc_type(DescEl,TypeEl),
            /****
            ask_do_element(DescEl,TypeEl,DescB),
            ****/
            calc(ex("intersect",[TypeEl,DescB]),B1),
            make_element(DescEl,B1),!.
%����� ����� ���� ������, ����� ������ �� ����. ��������, ����������� �� ��������. 
%� ���� ������: ��������� �� �������, ������ ��������������, ����������� fail. 
make_element(El,B):-
	    retractall(fatal_error),
    	    assert(fatal_error),
    	   expr_to_str(El,StrEl),
    	   expr_to_str(B,StrB), 
    	   %format(Prompt,"�� ���� ������� % ��������� ������� %.",StrEl,StrB),
    	   msg_n(err,34,[StrEl,StrB],b_false),
    	   fail.
    	   
    	   
make_element_desc(DescEl,DescOb):-  %���������� �������� � �������. ������� ����� ���� � ���������� ��������. 
           idconcept(Id),
           assert(element(Id,DescEl,DescOb)),!,
           expr_to_str(DescEl,StrDescEl),
           expr_to_str(DescOb,StrDescOb),
           %format(Prompt, "% ���� ��������� ������� %.", StrDescEl, StrDescOb),
           msg_n(approx,35,[StrDescEl, StrDescOb],b_false),
           clear_in_bigger(DescEl,DescOb).
 
 clear_in_bigger(El,Ob):-
           subobject(_,Ob,BigOb),
           retractall(element(_,El,BigOb)),
           fail.
 clear_in_bigger(_,_).

/***** isomor_with_point(Ob) ������ ��������� ������������� ����������� �
       ������. ��������� �������      *********/
     isomor_with_point(Ob):- /* �� ��� ������ ����������� */
           is_equal_not_message(Ob,ex("final",[])),!.
     isomor_with_point(Ob):-
            calc(Ob,Desc),
            element(_,Desc,_),!.
     isomor_with_point(ex(image,[_,Ob])):-
            isomor_with_point(Ob),!.
             
                   
    is_element(Elmt,Ob):-
           calc(Elmt,DescEl),
           calc(Ob,DescOb),
           is_element_desc(DescEl,DescOb),!.
   is_element(Elmt,Ob):-
   	isa_calc_type(Elmt,Ob).

     is_element_desc(DescEl,DescOb):-
           immed_element(DescEl,DescOb),!.        
     is_element_desc(DescEl,Ob):-
         % immed_element(DescEl,Ob1),
         %  member(Ob1,ListOb).
           subobjects_of(Ob,ListOb),
           immed_element_list(DescEl,ListOb),!.
    
     immed_element(El,Ob):-
           element(_,El,Ob).           
     immed_element(r(X),Ob):-
     	   bound(X),
           calc_on_approx(ex("real",[]),Desc),
           Desc=Ob,!.
     immed_element(nc(X),Ob):-       %immed_element(ex(X,[]),Ob):-
            calc_on_approx(ex("concepts",[]),Desc),
           Desc=Ob,!,
           name_file(X1,_,_,_),
           upper_lower(X1, X).
    immed_element(X,Ob):-
            calc_on_approx(ex("terms",[]),Desc),
           Desc=Ob,!,
           descriptor(_,X,1).
     immed_element(X,Ob):-
           calc_on_approx(ex("objcts",[]),Desc),
           Desc=Ob,!,
           descriptor(_,X,1),
           isa_calc_type(X,ex("ob",[])).
     immed_element(X,Ob):-
           calc_on_approx(ex("maps",[]),Desc),
           Desc=Ob,!,
           descriptor(_,X,1),
           isa_calc_type(X,ex("mor",[])).      
   
   /*  
     element_of_object(DescEl,DescOb):- bound(DescOb),     
           isomor_with_point(DescOb),!,
           DescEl=DescOb.
    */
     element_of_object(DescEl,DescOb):- 
           subobjects_of(DescOb,ListOb),
           immed_element_list(DescEl,[DescOb|ListOb]).
           
     immed_element_list(DescEl,[H|_]):-
            immed_element(DescEl,H).
     immed_element_list(DescEl,[_|T]):-
            immed_element_list(DescEl,T).




  eq_known(Ex1,Ex2):- calc_on_approx(Ex1,Res1), %������ �������������� �� ������������� ��� ������
  											%� �� ��������� ��, � ������� �� is_equal_not_message
                      calc_on_approx(Ex2,Res2),
                      Res1=Res2.
                      
  is_equal(Ex1,Ex2):- % �� ���������� �� eq_known. ����� ��������������, ��� ���� �������� ����������. ��������� �������������! 
          calc_on_approx(Ex1,Res1),
          calc_on_approx(Ex2,Res2),
   %expr_to_str(Res1,S1), expr_to_str(Res2,S2), format(Str,"Res1= %,  Res2= %",S1,S2 ), dlg_note("�������� ���������",Str),
          % calc(Ex1,Res1),
          %calc(Ex2,Res2),
          Res1=Res2,!.
   /*  
        % dlg_note("\n ��������� "),
         %dlg_note_ex(Ex1), 
          %dlg_note(" � "),
          %dlg_note_ex(Ex2),
          %dlg_note(" ��������, ��� ��� ��� ��������� ����� "),
         % dlg_note_ex(Res1),
          %dlg_note(".").
                            
  is_equal(Ex1,Ex2):- 
          dlg_note("�������� ���������"),
          calc_on_approx(Ex1,Res1),
          calc_on_approx(Ex2,Res2),
          Res1=c(X),       
          Res2=c(Y),
          element(c(X),_),
          element(c(Y),_),
          not(X=Y),!,
         
          dlg_note("\n ��������, ��� ��������� "),
          dlg_note_ex(Ex1), dlg_note(" � "),
          dlg_note_ex(Ex2),
          dlg_note(" �� �����."),
          fail.  
                  
    is_equal(Ex1,Ex2):- 
          dlg_note("�������� ���������"),
          calc_on_approx(Ex1,Res1),
          calc_on_approx(Ex2,Res2),
          order_desc(Res1,Res2,Dmax,Dmin),
          not_eq_desc(Dmax,Dmin),!,
          dlg_note("\n ��������, ��� ��������� "),
          dlg_note_ex(Ex1), dlg_note(" � "),
          dlg_note_ex(Ex2),
          dlg_note(" �� �����."),
          fail.          
  is_equal(Ex1,Ex2):-
          dlg_note("�������� ���������"),
          dlg_note("\n �� ����, ����� �� "),
          dlg_note_ex(Ex1),dlg_note(" � "),
          dlg_note_ex(Ex2),dlg_note("."),
          dlg_note("\n ������ ��������,��� ���������  "),
          dlg_note_ex(Ex1),dlg_note(" � "),
          dlg_note_ex(Ex2),dlg_note(" �����.").
          
  is_equal(Ex1,Ex2):-
          dlg_note("�������� ���������"),!,
          dlg_note("\n ���������  "),
          dlg_note_ex(Ex1),dlg_note(" � "),
          dlg_note_ex(Ex2),dlg_note(" �� �����."),          
          fail.
 
  is_equal(Ex1,Ex2):-
          is_equal_not_message(Ex1,Ex2).
 */         
  is_equal_not_message(Ex1,Ex2):-                          
          eq_known(Ex1,Ex2),!.
  is_equal_not_message(Ex1,Ex2):-
          calc_on_approx(Ex1,Res1),
          calc_on_approx(Ex2,Res2),
          order_desc(Res1,Res2,Dmax,Dmin),
          not_eq_desc(_,Dmax,Dmin),!,
          fail.          
  is_equal_not_message(Ex1,Ex2):-
          calc_on_approx(Ex1,Res1),
          calc_on_approx(Ex2,Res2),
          Res1=ex(X,[]),       
          Res2=ex(Y,[]),
          element(_,ex(X,[]),_),
          element(_,ex(Y,[]),_),
          not(X=Y),!,  
          fail.                  
  is_equal_not_message(Ex1,Ex2):-          
          deduce_on_approx("on"),
          deduce_eq(Ex1,Ex2).   
                
/*
  menudeduce(Ex1,Ex2,StartChoice):-
  	vmenu(4,36,defclr,
    	" ������� ������ ����������� ������ ",
       [v(" ���������� �������� ��� ���������",13),
        v(" �������� ������� ��� ��������� ",11),
        v(" �������� ��� ��������� ���������",25),
        v(" �������� ��� �������������� ���������",2),
        v(" �� ���� ������ ���������� ��� ���",5)],StartChoice,CHOICE),
         case(CHOICE,Ex1,Ex2).
         
         */
         /*
         
  case(1,Ex1,Ex2):- deduce_eq(Ex1,Ex2),!.
  case(1,Ex1,Ex2):- !, shiftwindow(4),
                    write("\n ����� ��������� �� ����������."),
                    write("\n �� ����, ����� �� "),                    
                    dlg_note_ex(Ex1),write(" � "),
                    dlg_note_ex(Ex2),write("."),                    
                    menudeduce(Ex1,Ex2,1).
  case(2,Ex1,Ex2):- 
            calc_type(Ex1,Ex1_type),Ex1_type="mor",
            expr_to_str(ex(make_equal_mor,[Ex1,Ex2]),Str), 
            make_equal(Ex1,Ex2),!,
            update_description(Str),
            message_make_inconsist. 
  case(2,Ex1,Ex2):- 
            calc_type(Ex1,Ex1_type),Ex1_type="ob",
            expr_to_str(ex(make_equal_ob,[Ex1,Ex2]),Str), 
            make_equal(Ex1,Ex2),!,
            update_description(Str),
            message_make_inconsist. 
  case(2,Ex1,Ex2):-!,
            shiftwindow(4),
            write("\n �� ���� �������� ������� "),
            dlg_note_ex(Ex1),write(" � "),
            dlg_note_ex(Ex2),write(",��� ��� ��� ��������� ������ �����."),
            fail.
            
  case(3,Ex1,Ex2):- 
            expr_to_str(ex(not_equal,[Ex1,Ex2]),Str), 
            make_not_equal(Ex1,Ex2),!,
            update_description(Str),
            message_make_inconsist,
            fail.                          
  case(3,Ex1,Ex2):-!, 
            write("\n �� ���� ������� ��������� "),                    
            dlg_note_ex(Ex1),write(" � "),
            dlg_note_ex(Ex2),write("."),                    
            menudeduce(Ex1,Ex2,3).
            
  case(4,Ex1,Ex2):- 
            makewindow(6,main_clr,main_clr,"��������������",2,0,22,80), 
            write("\n    �������������� ���������:"),
            nl, dlg_note_ex(Ex1), write(" = "),
            dlg_note_ex(Ex2), write(" ."),
            waitkey_esc(Key),not(Key=esc),
            calc(Ex1,Res1),
            calc(Ex2,Res2),
            nl,
            write("\n ��������,��� "),
            dlg_note_ex(Ex1),
            write(" = "),
            dlg_note_ex(Res1),
            write(" � "),
            dlg_note_ex(Ex2),
            write(" = "),
            dlg_note_ex(Res2),
            write("."),
            waitkey_esc(RKey),not(RKey=esc),
            prove_eq(Ex1,Ex2),!,
            removewindow(6,1).
  case(4,Ex1,Ex2):- !,
            removewindow(6,1),
            shiftwindow(4),  
            write("\n �������� �� �������."),
            write("\n �� ����, ����� �� "),                    
            dlg_note_ex(Ex1),write(" � "),
            dlg_note_ex(Ex2),write("."),
            shiftwindow(2),                    
            menudeduce(Ex1,Ex2,4).            
  case(5,Ex1,Ex2):- !, 
                    shiftwindow(4),  
                    write("\n ��� ����������� ���� �� ��������. "),
                    write("\n �� ����, ����� �� "),                    
                    dlg_note_ex(Ex1),write(" � "),
                    dlg_note_ex(Ex2),write("."),
                    menudeduce(Ex1,Ex2,5).      
  */
  
/**********************************************/  
/*    ���������  ������ ��������� ����������  */
/*    ex-���������   (deduce_eq)              */
/**********************************************/

   /*reduce(N,Ex,REx)  �������� ���������� ���������
            � ������������� ���� , ���� ��� ������*/ 

 
  deduce_eq(Ex1,Ex2):-
     message_deduce(Ex1,Ex2),
     reduce(0,Ex1,REx1), reduce(0,Ex2,REx2),
     calc(REx1,Res1),  calc(REx2,Res2),
     Res1=Res2,!,
     make_equal(Ex1,Res1),
     make_equal(Ex2,Res1).           

  message_deduce(Ex1,Ex2):-
    %dlg_note("�����"),
        expr_to_str(Ex1,Str1),
        expr_to_str(Ex2,Str2),
  	%format(String,"�������� ����� ��������� % = %.", Str1,Str2),
	 msg_n(calc,36,[Str1,Str2],b_false).
	 % dlg_note(String).    
  message_deduce(_,_).
 

maxlong(N):-maxlong_deduce(N),!.							
maxlong(100).        
    

reduce(N,Ex,Ex):- maxlong(M),N>=M,!,
                  %format (String,"����� �������������� ��������� ������������ �����,\n ������ %.",M),
                  %dlg_note(String),
                  str_int(Mstr,M),
                 msg_n(warning,37,[Mstr],b_true).
                                  
  reduce(N,Ex,Res):-  maxlong(M),N<M,
                  rewrite(Msg,Ex,Res1),!,
              	  msg(rwrt,Msg),
                  N1=N+1,   
                  reduce(N1,Res1,Res).                 
  reduce(N,ex(Op,Arg),Res):-
                  maxlong(M),N<M,
                  reduce_list(N,Arg,[],Reslist),
                  not(Arg=Reslist),!,
                  N1=N+1,
                  reduce(N1,ex(Op,Reslist),Res).
reduce(_,Ex,Ex).             
      
        

  reduce_list(_,[],LEx,Res):-!,
  	turn_list(LEx,[],Res).
  reduce_list(N,[H|T],List,Res):- reduce(N,H,ResH),
                                     reduce_list(N,T,[ResH|List],Res).

turn_list([],Res,Res):-!.
turn_list([H|T],L,Res):-
	turn_list(T,[H|L],Res).


 
    /************ ����� ��������� ������ �������������� ***********/     
 
 
minob_of_element(El,Minob):-
 	calc(El,DescEl),
 	findall(Ob,element(_,DescEl,Ob),ListOb),
 	calc_type(El,Type),
 	intersect_list([Type|ListOb],ex("initial",[]),Minob).
 	
intersect_list([Ob],ex("initial",[]),Ob):-!.
intersect_list([],Res,Res):-!.
intersect_list([H|T],B,Res):-!,
		calc(ex("intersect",[H,B]),B1),
		intersect_list(T,B1,Res).   
     
make_equal(nc(Name),Ex2):-!,
		minob_of_element(Ex2,Type),
		create_new_constant(nc(Name),Type,Res),
		make_equal(Res,Ex2).
make_equal(Ex1,nc(Name)):-!,
		make_equal(nc(Name),Ex1).
make_equal(Ex1,Ex2):-
	calc(Ex1,D1),
	calc(Ex2,D2),!,
	make_equal_1(Ex1,Ex2,D1,D2).		
make_equal(Ex1,Ex2):-
           not(calc(Ex1,_)),!,
           expr_to_str(Ex1,StrEx1),
           expr_to_str(Ex2,StrEx2),
           %format(Str,"�� ����������� ��������� \"%\".\n�� ���� ���������� ��������� \"%\" � \"%\".",StrEx1,StrEx1,StrEx2),
	   msg_n(diagn,38,[StrEx1,StrEx1,StrEx2],b_false),	
	   fail.
make_equal(Ex1,Ex2):-
           not(calc(Ex2,_)),!,
           expr_to_str(Ex1,StrEx1),
           expr_to_str(Ex2,StrEx2),
           %format(Str,"�� ����������� ��������� \"%\".\n�� ���� ���������� ��������� \"%\" � \"%\".",StrEx2,StrEx1,StrEx2),
	    msg_n(diagn,40,[StrEx2,StrEx1,StrEx2],b_false),
	    fail.
 
make_equal_1(_,_,D1,D2):-
              D1=D2,!.             

make_equal_1(Ex1,Ex2,D1,D2):- % ��� ������� ����� �������� �������� �������
             calc_type(D1,T1),
             calc_type(D2,T2),
             is_subobject(T1,T2),!,
             make_equal_desc(D1,D2),
             message_make_equal(Ex1,Ex2).
make_equal_1(Ex1,Ex2,D1,D2):- % ��� ������� ����� �������� �������� �������
             calc_type(D1,T1),
             calc_type(D2,T2),
             is_subobject(T2,T1),!,
             make_equal_desc(D1,D2),
             message_make_equal(Ex1,Ex2).

make_equal_1(Ex1,Ex2,D1,D2):- % ���������� ����������� �����  ������
             calc_type(D1,T1),
             calc_type(D2,T2),
            calc_on_approx(ex("intersect",[T1,T2]),_),!,
             make_equal_desc(D1,D2),
             message_make_equal(Ex1,Ex2).

make_equal_1(Ex1,Ex2,_,_):- % ���� ������ �����������.
             calc_type(Ex1,T1),
             calc_type(Ex2,T2),
             expr_to_str(Ex1,StrEx1),
             expr_to_str(Ex2,StrEx2),
             expr_to_str(T1,Typ1),
             expr_to_str(T2,Typ2), 
             %format(Str,"����������� ����  \"%\" � \"%\" ������ % � \%.\n�� ���� ���������� ��� �����.\n�������� �������������� ����� ��������� ����������� �� �����.",Typ1,Typ2,StrEx1,StrEx2),
		msg_n(err,41,[Typ1,Typ2,StrEx1,StrEx2],b_false),
		retractall(fatal_error),
		assert(fatal_error),
		fail.           
     
/* message_make_equal - ������ ��������� � ����� ���������, 
                        ���� ��� ����������� � ���� ���������*/
 
message_make_equal(Ex1,Ex2):-
             expr_to_str(Ex1,Str1),
             expr_to_str(Ex2,Str2),!,
             %format(Str,"���������� ��������� % = % ���������.",Str1,Str2),
             msg_n(calc,42,[Str1,Str2],b_false).            
    
/* make_equal_desc - ������������ ��� �����������  */
 
make_equal_desc(D1,D2):-
             order_desc(D1,D2,Dmax,Dmin),
             retractall(eq_desc(_,_)),
             retractall(modification_resalt_op(_,_)),
             assert(eq_desc(Dmax,Dmin)),
             factor_set_desc,
             not(fatal_error).  

/********************************************************************/             
/* factor_set_desc - ����������� ����������� ��������� ������������ */
/*     �� �������� ���������������, ������������ ���������� eq_desc */
/*     � ���������� ��������� ����������� �� ������ ���������� ���  */
/*     �������� �������� �� resalt_op                               */
/********************************************************************/
factor_set_desc:-
             retract(eq_desc(D1,D2)),
             replace_in_not_eq_desc(D1,D2),             
             replace_in_resalt_op(D1,D2),
             replace_in_subobject(D1,D2),
             replace_in_element(D1,D2),
             replace_in_eq_desc(D1,D2),
             replace_in_descriptor(D1,D2),
             new_equalities,
             fail.

factor_set_desc.
  
/*  replace_in_not_eq_desc(D1,D2) �������� D1->D2 � not_eq_desc � ���������
    �� ���������������� */
replace_in_not_eq_desc(El,Nil):- /*  */
             calc_on_approx(ex("initial",[]),Desc),
             Desc=Nil,
             is_not_nil(El),!,
             retractall(fatal_error),
            assert(fatal_error),
            message_inconsist(El,ex("initial",[]),"�������������� ��������  � �����."),
             replace_not_message(El,Nil).
             
replace_in_not_eq_desc(Nil,El):- 
             calc_on_approx(ex("initial",[]),Desc),
             Desc=Nil,
             is_not_nil(El),!,
             retractall(fatal_error),
            assert(fatal_error),
             message_inconsist(El,ex("initial",[]),"�������������� ��������  � �����."),
             replace_not_message(El,Nil). 
  
replace_in_not_eq_desc(ex(Op1,[]),ex(Op2,[])):- 
             descriptor(_,ex(Op1,[]),1),
             descriptor(_,ex(Op2,[]),1),
             not(Op1=Op2),
             ask_synonym_desc(ex(Op1,[]),ex(Op2,[])),!,
  	     get_in_template(IdDostup1,ID_Concept1, Id_template1,Text1, Op1,Comment1,L1,T1),
             get_in_template(_IdDostup2,_ID_Concept2, _Id_template2,Text2, Op2,_Comment2,_L2,_T2),
             !,
  	     idconcept(IdConc),
	     retractall(in_template(IdDostup1,ID_Concept1, Id_template1,Text1, Op1,Comment1,L1,T1)),
	     format(Comment,"%\n\t������� ������� %.",Comment1, Text2),
	     assert(in_template(IdDostup1,IdConc, Id_template1,Text1, Op2,Comment,L1,T1)),
	     replace_not_message(ex(Op1,[]),ex(Op2,[])).
	     %;
	     %dialog_answer("no"),
	     %write("no"),
	     %exit.

replace_in_not_eq_desc(Desc1,Desc2):- 
             descriptor(_,Desc1,1),
             descriptor(_,Desc2,1),
             
             write("3 clause\n", Desc1),
             
             is_not_eq_desc(Desc1,Desc2),!,
             retractall(not_eq_desc(_,Desc1,Desc2)),
             message_inconsist(Desc1,Desc2,"��������������  ���������."),
             replace_not_message(Desc1,Desc2). 
  	%�������������� ��������� � ����������
  replace_in_not_eq_desc(Desc1,Desc2):- 
             descriptor(_,Desc1,1),
             Desc2=ex("set",_),
             not(Desc1=Desc2),!,
             retractall(not_eq_desc(_,Desc1,Desc2)),
             message_inconsist(Desc1,Desc2,"��������������  ��������� � ���������."),
             replace_not_message(Desc1,Desc2). 
  replace_in_not_eq_desc(Desc1,Desc2):- 
             Desc1=ex("set",_),
             descriptor(_,Desc2,1),
             not(Desc1=Desc2),!,
             retractall(not_eq_desc(_,Desc1,Desc2)),
             message_inconsist(Desc1,Desc2,"�������������� ��������� � ���������."),
             replace_not_message(Desc1,Desc2). 
  replace_in_not_eq_desc(Desc1,Desc2):- 
              Desc1=ex("set",A),
              Desc2=ex("set",B),
             not(A=B),!,
             retractall(not_eq_desc(_,Desc1,Desc2)),
             message_inconsist(Desc1,Desc2,"�������������� ���������."),
             replace_not_message(Desc1,Desc2). 
   %������ ��������� � ����� �� ����������. ��� ������� ����.
    replace_in_not_eq_desc(Desc1,Desc2):- 
             not(Desc1=ex(_,_)),
             not(Desc2=ex(_,_)),
             is_not_eq_desc(Desc1,Desc2),!,
             retractall(not_eq_desc(_,Desc1,Desc2)),
             message_inconsist(Desc1,Desc2,"��������������  ���������."),
             replace_not_message(Desc1,Desc2). 
                         
  replace_in_not_eq_desc(D1,D2):-  
             not_eq_desc(_,D1,D2),!,
             retractall(not_eq_desc(_,D1,D2)),
             message_inconsist(D1,D2, ""),
             replace_not_message(D1,D2).
  replace_in_not_eq_desc(D1,D2):-
             replace_not_message(D1,D2). 
          
 ask_synonym_desc(ex(Op1,[]),ex(Op2,[])):-
  	     synonym_desc(ex(Op1,[]),ex(Op2,[])),!.
 ask_synonym_desc(ex(Op1,[]),ex(Op2,[])):-
  	     expr_to_str(ex(Op1,[]),St1),
             expr_to_str(ex(Op2,[]),St2),
             format(Prompt1,"�������� ������� ������� <BR> %  �  %, ������� �������������� ���������.",St1,St2),
	     msg_n(warning,75,[St1,St2],b_false),
	     format(Prompt,"%\<BR>������� �� ����������?",Prompt1 ),
	     %not (dialog_answer("no")),
	     dlgAsk_call_eq(Prompt),
	     %dialog_answer(A),
	     %write(A),
	     %retractall (dialog_answer(_)),
	     %Bool = "true",
	     %resp_default= dlg_Ask("������������",Prompt,["&��","&���"]),
	     %tnotion_window(WinTn,"tnotion",_),
	     expr_to_str(ex("synonym",[ex(Op2,[]),ex(Op1,[])]),Text),
	     format(_SText,"<BR>%.",Text),
	    % edit_AppendStr(WinTn,SText),
	     /***
	     retractall(tnotion_window(WinTn,"tnotion",_)),
	  
	NewText=edit_GetText(WinTn),
	assert(tnotion_window(WinTn,"tnotion",NewText)),
	retractall(edit_window(WinTn,_,_)),
	assert(edit_window(WinTn,"tnotion",NewText)),
	   ***/
	        /***TO SUBSTITUTE THE STRING BELOW ***/
	        NewText = "�����������",
	put_defin1(NewText),
	     !.
	     
	             
          
  replace_not_message(D1,D2):-
             not_eq_desc(_,D1,D),
             retractall(not_eq_desc(_,D1,D)),
             order_desc(D2,D,Dmax,Dmin),
             retractall(not_eq_desc(_,Dmax,Dmin)),
             idconcept(Id),
             assert(not_eq_desc(Id,Dmax,Dmin)),
             fail.
  replace_not_message(D1,D2):-
             not_eq_desc(_,D,D1),
             retractall(not_eq_desc(_,D,D1)),
             order_desc(D2,D,Dmax,Dmin),
             retractall(not_eq_desc(_,Dmax,Dmin)),
             idconcept(Id),
             assert(not_eq_desc(Id,Dmax,Dmin)),
             fail.
  replace_not_message(_,_).                        

  message_inconsist(D1,D2,Str):-
 		expr_to_str(D1,St1),
          	expr_to_str(D2,St2),
             	format(Prompt1,"�������� ������� \n %  �  %, ������� �������������� ���������.",St1,St2),
	     	msg(warning,Prompt1),
	     	format(Prompt,"%\n %\n����������?",Prompt1,Str),
	     	%resp_default= dlg_Ask("������������",Prompt,["&��","&���"]),
	     	dlgAsk_call_eq(Prompt),!,
	     	%Bool = "true", !,
	     	idconcept(Id),
	     	assert(inconsist(Id,D1,D2)),
             	retractall(current_inconsist(_)),
             	assert(current_inconsist(Id)).
  message_inconsist(_,_,_):-
  		retractall(fatal_error),
  		assert(fatal_error),
  		fail. 
   
      
  		
  is_not_eq_desc(r(X),r(Y)):-!,
  		Z=X-Y,
  		W=abs(Z),
  		W > (0.1e-100).
  is_not_eq_desc(Desc1,Desc2):-
  	not(Desc1=Desc2).		             
  		             
  is_not_nil(Ob):- isomor_with_point(Ob),!.
  is_not_nil(Ob):- element_of_object(_,Ob),!.
             
             
/*  replace_in_resalt_op(D1,D2)  �������� � resalt_op(Ex,D)    D1->D2 */ 
  replace_in_resalt_op(D1,D2):-
  		retractall(descriptor(_,D1,_)),
               %not(calc_on_approx(D1,_)), %����� ������� ������������
               idconcept(Id),
               assert(resalt_op(Id,D1,D2)),
  		fail.
  replace_in_resalt_op(D1,D2):-
  	     resalt_op(_,ex(Op,Arg),D),
  	     memberEx(D1,[D|Arg]),
             retractall(resalt_op(_,ex(Op,Arg),_)),
             replace_in_list(Arg,ArgNew,D1,D2),
             replace_in_list([D],ListDnew,D1,D2),
             ListDnew=[Dnew],
             not(Dnew=ex(Op,ArgNew)),
             assert(modification_resalt_op(ex(Op,ArgNew),Dnew)),
             not(calc_on_approx(ex(Op,ArgNew),_)),
             idconcept(Id),
             assert(resalt_op(Id,ex(Op,ArgNew),Dnew)),
             fail.
 replace_in_resalt_op(_,_).

  replace_in_descriptor(D1,D2):-
  	     descriptor(_,ex(Op,Arg),_),
  	     memberEx(D1,Arg),
             replace_in_list(Arg,ArgNew,D1,D2),
             retractall(descriptor(_,ex(Op,Arg),_)),
             assert(modification_resalt_op(ex(Op,ArgNew),ex(Op,ArgNew))),
             not(calc_on_approx(ex(Op,ArgNew),_)),
             weigh(ex(Op,ArgNew),W),
             idconcept(Id),
             assert(descriptor(Id,ex(Op,ArgNew),W)),
              fail.
  replace_in_descriptor(_,_).

 
memberEx(D,[D|_]):-!.
memberEx(D,[ex(_,Arg)|_]):-memberEx(D,Arg),!.
memberEx(D,[_|T]):-memberEx(D,T),!.

/************** ���������  replace_in_subobject(D1,D2) ********/ 
/*  ������� ��� ������ ����������� Desc1 � Desc2, �.�. �����, */
/*  ��� Desc1 �������� Desc2 � �������� Desc2 �������� Desc1  */
/*  ��� �������������� ���������  D1 = D2. ����� ��� ������   */
/*  ��� ��������� � eq_desc � �������� ������ ����������� �   */
/*  subobject �� ����������� ���������.                       */
/*         �����������: is_subobject ��������, ���� ��������� */
/*         subobject �� �������� ������ ��� � �� ����� ������.*/
/**************************************************************/
    /* replace_in_subobject(D1,_):-  ������ D1->D2 ��� �������� make_equal 
             calc_type(D1,T1),
             not(is_subobject(T1,ex("ob",[]))),!.   */
     replace_in_subobject(D1,_):- 
             not(subobject(_,D1,_)),
             not(subobject(_,_,D1)),!.             
     replace_in_subobject(D1,D2):- 
             find_all_equal_desc(D1,D2,List_desc),
             min_desc(List_desc,Dmin),
             replace_in_list(List_desc,List_without_D1,D1,D2),
             assert_list_in_eq_desc(List_without_D1,Dmin),
             replace_list_in_subobject(List_desc,Dmin).
            % delete_element(D2,Dmin). % ���� D2 ��� Dmin - ��������,Dmin 
                                     % ���������� �� subobject ��� ��������� 
     replace_in_subobject(_,_).
     
     find_all_equal_desc(D1,D2,[D1,D2|List_desc]):-
             findall(Desc1,between(D1,D2,Desc1),List1),
             findall(Desc2,between(D2,D1,Desc2),List2),
             append(List1,List2,List_desc).

ifdef iso_prolog
:- redefine_system_predicate(between/3).
enddef
     /*between(D1,D2,D) �� ������ D- �������D2, ����,��� D1<D */         
     between(D1,D2,_):-      
            not(is_subobject(D1,D2)),!,             
            fail.
     between(_,D2,D2).
     between(D1,D2,Desc):-    /* ����� �� ������ Desc ,������ D2   */
            subobject(_,Desc,_),
            is_subobject(D1,Desc),
            is_subobject(Desc,D2).
     
     replace_list_in_subobject(List_desc,Dmin):-  /* ���� Dmin  ����� */
            is_equal_not_message(Dmin,ex(initial,[])),
            members(Desc,List_desc),
            retractall(subobject(_,Desc,_)),
            retractall(subobject(_,_,Desc)),
            fail.
     replace_list_in_subobject(List_desc,Dmin):-  /* ������ ���1 � subobject */
            members(Desc1,List_desc),
            subobject(_,Desc1,Desc2),
            retractall(subobject(_,Desc1,Desc2)),
            retractall(subobject(_,Dmin,Desc2)),
            not(Dmin=Desc2),
            idconcept(Id),
            asserta(subobject(Id,Dmin,Desc2)),  
            fail.
     replace_list_in_subobject(List_desc,Dmin):-  /* ������ ���2 � subobject */
            members(Desc2,List_desc),
            subobject(_,Desc1,Desc2),
            retractall(subobject(_,Desc1,Desc2)),
            retractall(subobject(_,Desc1,Dmin)),
            not(Desc1=Dmin),
            idconcept(Id),
            asserta(subobject(Id,Desc1,Dmin)),  
            fail.   
     replace_list_in_subobject(_,_):- 
            modi_subobject.   /* ������� ������ �� �������������� ���� */

/*
     delete_element(_,X):-  % �������� �������� �� subobject,
                                 %���� D2 ��� Dmin - ��������       
            element(_,X,_),!,
            retractall(subobject(_,X,_)).
     delete_element(X,Dmin):-  
            element(_,X,_),!,
            retractall(subobject(Dmin,_)).
     delete_element(_,_).
   */  
      members(X,[X|_]).
members(X,[_|L]):-members(X,L).   
           
          /*********** ����� ��������� replace_in_subobject ********/          


/****    ������ D1 -> D2  � ��������� �� element (Element,Object)     ****/    
/* � ��������� ������������ �������� ��  element(DescElement,DescObject),*/
/* ���������� ��������������� �������������� (��� ������������� ��������)*/
/*      ��� ���������� ������ ��������� ����������� ������������� ���:   */
/*             element(X,a) � element(a,Y)  ������������                 */
/*************************************************************************/

 replace_in_element(D1,D2):- /* ���� ������� D1 � D2 ��������� ����� */
             isomor_with_point(D1),
             isomor_with_point(D2),!,
             repl_arg1_element(D1,D2).
 replace_in_element(D1,D2):- /* ���� ������� D2 ��������� �����,�� ����� ����� */
             isomor_with_point(D2),!,
             inconsist_if_two_elements_in(D1),
             findall(DescEl,element_of_object(DescEl,D1),List1),
             list_not_repeat([D2|List1],ListDesc),
             retractall(element(_,_,D1)),
             repl_arg1_element(D1,D2),
             min_desc(ListDesc,Dmin),
             assert_list_in_eq_desc(ListDesc,Dmin),
             message_eq_with_point(D1,ListDesc,Dmin).
 replace_in_element(D1,D2):- /* ���� ������� D1 ��������� �����,�� ����� ����� */             
             isomor_with_point(D1),!,
             inconsist_if_two_elements_in(D2),
             findall(DescEl,element_of_object(DescEl,D2),List1),
             list_not_repeat([D2|List1],ListDesc),
             retractall(element(_,_,D2)),
             repl_arg1_element(D1,D2),
             min_desc(ListDesc,Dmin),
             assert_list_in_eq_desc(ListDesc,Dmin),
             message_eq_with_point(D1,ListDesc,Dmin).
 replace_in_element(D1,D2):- /* ������ ������� D1 �� D2 �� ������ ��������� */
             element(_,D,D1),
             retractall(element(_,D,D1)),
             retractall(element(_,D,D2)),
             idconcept(Id),
             assert(element(Id,D,D2)),
             fail.
 replace_in_element(_,_).

 repl_arg1_element(D1,D2):- /* ������ �������� D1 �� D2 � ������ ��������� */
             element(_,D1,D),
             retractall(element(_,D1,D)),
             retractall(element(_,D2,D)),
             idconcept(Id),
             asserta(element(Id,D2,D)),
             retractall(element(_,D,D2)),
             fail. 
 repl_arg1_element(_,_).
 
 
             
 inconsist_if_two_elements_in(Ob):-
             any_two_elements(Ob,X,Y),!,
           %  element_of_object(X,Ob),X=c(_),
           %  element_of_object(Y,Ob),Y=c(_),
           %  not(X=Y),!,
              message_inconsist(X,Y,"���������� ��� �������� ������� �������� ��������������.").                
 inconsist_if_two_elements_in(_).     
 
 


  message_eq_with_point(_,_,_).  
    /********  ����� ���������   replace_in_element  ********/

/* replace_in_eq_desc(D1,D2) - �������� � eq_desc D1->D2,�� ����� eq_desc(D2,D2)*/
  replace_in_eq_desc(D1,D2):-
             eq_desc(D1,D),
             retractall(eq_desc(D1,D)),
             order_desc(D2,D,Dmax,Dmin),
             assert_in_eq_if_not(Dmax,Dmin),
             fail.
  replace_in_eq_desc(D1,D2):-
             eq_desc(D,D1),
             retractall(eq_desc(D,D1)),
             order_desc(D2,D,Dmax,Dmin),
             assert_in_eq_if_not(Dmax,Dmin),             
             fail.
  replace_in_eq_desc(_,_).
    
/*  replace_in_list(Desclist,RDesclist,D1,D2) - �������� � Desclist   D1 ->D2 */
  replace_in_list([],[],_,_):- !.
  replace_in_list([D1|T1],[D2|T2],D1,D2):-!,
              replace_in_list(T1,T2,D1,D2).
 replace_in_list([ex(Op,Larg)|T1],[ex(Op,LargD2)|T2],D1,D2):-!,
              replace_in_list(Larg,LargD2,D1,D2),
              replace_in_list(T1,T2,D1,D2).  
  replace_in_list([H|T1],[H|T2],D1,D2):-
              replace_in_list(T1,T2,D1,D2).

/* new_equalities - ��������������� ����������� ����� �� modification_resalt_op �,
                  ���� ������� ����� � ���� �� ����������� � resalt_op � modification_resalt_op,
                  �� ��������� ��������� eq_desc  */
  new_equalities:-
              retract(modification_resalt_op(Exp,D)),
              findall(Res,modification_resalt_op(Exp,Res),Desclist1),
              findall(Desc,resalt_op(_,Exp,Desc),Desclist2),   
              append(Desclist1,Desclist2,Desclist),
              retractall(modification_resalt_op(Exp,_)),
              calc_on_approx(Exp,D1),
              min_desc([D1,D|Desclist],Dmin), 
              assert_list_in_eq_desc([D1,D|Desclist],Dmin), 
              not(Desclist2=[]),
              retractall(resalt_op(_,Exp,_)),
              idconcept(Id),
              assert(resalt_op(Id,Exp,Dmin)),
              fail.
  new_equalities.            
  
 /* assert_list_in_eq_desc(Desclist,D) - ��������� � eq_desc 
          ������������������� ���� (D1,D),��� D1 �� Desclist */
  assert_list_in_eq_desc([],_):-!.
  assert_list_in_eq_desc([D|T],D):-!,
              assert_list_in_eq_desc(T,D).
  assert_list_in_eq_desc([D1|T],D):-
              order_desc(D1,D,Dmax,Dmin),
              assert_in_eq_if_not(Dmax,Dmin),
              assert_list_in_eq_desc(T,D).
              
  assert_in_eq_if_not(D1,D2):-
              retractall(eq_desc(D1,D2)),
              assert(eq_desc(D1,D2)).            

/* min_desc(Desclist,Dmin) ������� ����������� ���������� 
                           � �������� ������ Desclist */
 min_desc([D],D):-!.
 min_desc([D,D],D):-!.
 min_desc([D1,D2],Dmin):-
              order_desc(D1,D2,_,Dmin),!.
  min_desc([D1,D2],Dmin):-!,
              order_expr(D1,D2,_,Dmin).
 min_desc([D1|T],Dmin):-
              min_desc(T,D2),
              min_desc([D1,D2],Dmin).          

/* order_desc(D1,D2,Dmax,Dmin) ������������� �����������; �� ������ - fail */
 order_desc(D1,D2,D1,D2):-
              descriptor(_,D1,W1),
              descriptor(_,D2,W2),
ifndef iso_prolog              
              W1 > W2,
elsedef
              W1 @> W2,
enddef              
              !.

 order_desc(D1,D2,D2,D1):-
              descriptor(_,D1,W1),
              descriptor(_,D2,W2),
ifndef iso_prolog              
              W1 < W2,
elsedef
              W1 @< W2,
enddef              
               !.
               
 order_desc(D1,D2,Dmax,Dmin):-
              descriptor(_,D1,_),
              descriptor(_,D2,_),
              order_expr(D1,D2,Dmax,Dmin),!.
              
order_desc(r(X),r(X),_,_):-!,fail.
order_desc(r(X),D,D,r(X)):-!.
order_desc(D,r(X),D,r(X)):-!.

order_desc(ex("set",Arg),ex("set",Arg),_,_):-!,fail.
order_desc(D1,ex("set",Arg),D1,ex("set",Arg)):-!.
order_desc(ex("set",Arg),D2,D2,ex("set",Arg)):-!.

/*
order_desc(ex("emptyset",[]),ex("emptyset",[]),_,_):-!,fail.
order_desc(D1,ex("emptyset",[]),D1,ex("emptyset",[])):-!.
order_desc(ex("emptyset",[]),D2,D2,ex("emptyset",[])):-!.
 */             


/*  order_expr(Ex1,Ex2,Exmax,Exmin)  ����������������� ������������� 
                                   ��� ex-���������; �� ������ - fail */                            
/* ������ 23.12.02
 order_expr(ex(Op,Arg),c(A),c(A),ex(Op,Arg)):-
 				const(A,c("var")),!.
 order_expr(c(A),ex(Op,Arg),c(A),ex(Op,Arg)):-
 				const(A,c("var")),!.

 order_expr(c(A),ex(Op,[]),c(A),ex(Op,[])):-!. 
 order_expr(ex(Op,[]),c(A),c(A),ex(Op,[])):-!.

 order_expr(c(A),ex(Op,Arg),ex(Op,Arg),c(A)):-!. 
 order_expr(ex(Op,Arg),c(A),ex(Op,Arg),c(A)):-!.

 order_expr(c(B),c(A),c(B),c(A)):-
              B>A,!. 
 order_expr(c(A),c(B),c(B),c(A)):-
              B>A,!. 
*/
 order_expr(r(A),r(A),_,_):-!,fail.
/* �������� ��-�� �������� 20.01.03 
 order_expr(r(A),D,D,r(A)):-!. 
 order_expr(D,r(A),D,r(A)):-!.*/
% �������� 20.01.03 ������ ��-�� ��������
 order_expr(r(A),r(B),r(A),r(B)) :- 
 ifndef iso_prolog
 	A > B,
 elsedef
 	A @> B,
 enddef	
 	!.
 	
 order_expr(r(A),r(B),r(B),r(A)) :- 
 ifndef iso_prolog
  	B > A,
 elsedef
  	B @> A,
 enddef	
  	!.
 
 order_expr(ex(Op,Arg),r(A),ex(Op,Arg),r(A)) :- !. 
 order_expr(r(A),ex(Op,Arg),ex(Op,Arg),r(A)) :- !. 
 
 order_expr(ex(Op,Arg),nc(A),ex(Op,Arg),nc(A)) :- !.
 order_expr(nc(A),ex(Op,Arg),ex(Op,Arg),nc(A)) :- !. 
 
 order_expr(nc(B),nc(A),nc(B),nc(A)) :- 
 ifndef iso_prolog
  	B > A,
 elsedef
  	B @> A,
 enddef	
 	!. 
 	
 order_expr(nc(A),nc(B),nc(B),nc(A)) :- 
 ifndef iso_prolog
  	B > A,
 elsedef
  	B @> A,
 enddef	
 	!.               

 order_expr(nc(B),r(A),nc(B),r(A)) :- !.
 
 order_expr(r(A),nc(B),nc(B),r(A)) :- !.               
 
%����� ������         

 order_expr(Ex1,Ex2,Ex1,Ex2):-
 	expr_to_str(Ex1,StrEx1),
 	expr_to_str(Ex2,StrEx2),  
 	str_len(StrEx1,L1), 
 	str_len(StrEx2,L2),  
        L1 > L2,
         !. 
         
 order_expr(Ex1,Ex2,Ex2,Ex1):-
 	expr_to_str(Ex1,StrEx1),
 	expr_to_str(Ex2,StrEx2),  
 	str_len(StrEx1,L1), 
 	str_len(StrEx2,L2),  
        L2 > L1,
         !. 
        
order_expr(Ex1,Ex2,Ex2,Ex1):-
 	expr_to_str(Ex1,StrEx1),
 	expr_to_str(Ex2,StrEx2),           
 ifndef iso_prolog
        StrEx1 < StrEx2,
 elsedef
        StrEx1 @< StrEx2,
 enddef	
        !. 
              
 order_expr(Ex1,Ex2,Ex2,Ex1):-
 	expr_to_str(Ex1,StrEx1),
 	expr_to_str(Ex2,StrEx2),           
 ifndef iso_prolog
        StrEx1 < StrEx2,
 elsedef
        StrEx1 @< StrEx2,
 enddef	
        !. 
        
 order_expr(Ex1,Ex2,Ex1,Ex2):-!.
 
 /*
 order_expr(ex(Op1,Arg1),ex(Op2,Arg2),ex(Op1,Arg1),ex(Op2,Arg2)):-
              Op1>Op2,!. 
 order_expr(ex(Op2,Arg2),ex(Op1,Arg1),ex(Op1,Arg1),ex(Op2,Arg2)):-
              Op1>Op2,!. 
 order_expr(ex(Op,Arg1),ex(Op,Arg2),ex(Op,Argmax),ex(Op,Argmin)):-
              order_expr_list(Arg1,Arg2,Argmax,Argmin).
 */             
              
 /*************** ����� ��������� factor_set_desc ***************/
 
/**************************************************************************/   
/*      ��������� �������� ��������� ��� ��������� make_not_equal         */
/*                  ������� �������� �� ����������������                  */ 
/**************************************************************************/
  make_not_equal(Ex1,Ex2):-
            not(deduce_eq(Ex1,Ex2)),!,
            calc(Ex1,Res1),
            calc(Ex2,Res2),
            order_desc(Res1,Res2,Dmax,Dmin),
            idconcept(Id),
            assert(not_eq_desc(Id,Dmax,Dmin)). /* ������������ �����������*/

/*
  make_not_equal(Ex1,Ex2):-
            makewindow(26,74,74,"",6,10,7,40),
            write("\n ����� �������� ��������� ���������\n"),
            dlg_note_ex(Ex1),write(" � "),
            dlg_note_ex(Ex2),write(",\n������� �� ���� ������� �� ���������."),
            !,           
            waitkey,
            removewindow(26,1),
            fail.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
