
/* ['var','x',';','x',':=','(',5,'*',2,')',';','return','(','x','+',1,')','.'] */

/* [declaredVar(x),assignedVar(trm( *, 5, 2), x),returnedExpr(expression( +, x, 1))] */

/*_____________________________________________________________________________*/
% 			Predicates for Syntax
/*_____________________________________________________________________________*/

keyword(X) :- member(X,['var','return','if','else','while','do', '+', '-','/','*',':=',';','.']).

keyword(X) :- number(X).

id(ID) :- atom(ID), \+ keyword(ID).

/*******************************************************************************/
% 			Syntax(TokenList To AST)
/*******************************************************************************/

prog([L|R]) --> declaration(L), [';'], prog(R).

prog([L|R]) --> assignment(L), [';'], prog(R).

prog([X]) --> retStatement(X), ['.'].

/*___________________________________________________________________________*/

declaration(declaredVar(ID)) --> ['var'], [ID], {id(ID)}.

assignment(assignedVar(Base,ID)) --> [ID], {id(ID)}, [':='], base(Base).

retStatement(returnedExpr(Base)) --> ['return'], base(Base).

/*____________________________________________________________________________*/

base(Num) --> [Num] , {number(Num)}.
base(ID) --> [ID] , {id(ID)}.
base(Expr) --> ['('], expr(Expr),[')'].

/*____________________________________________________________________________*/

mulDivPredicate(Op) :- member(Op, ['*', '/']).
addSubPredicate(Op) :- member(Op, ['+', '-']).

mulOp(MulDiv) --> [MulDiv], {mulDivPredicate(MulDiv)}.
addOp(AddSub) --> [AddSub], {addSubPredicate(AddSub)}.

/*_____________________________________________________________________________*/

term(X) --> base(X).
term(trm(MulDiv, Expr, Term)) --> base(Expr), mulOp(MulDiv), term(Term).

/*_____________________________________________________________________________*/

expr(X) --> term(X).
expr(expression(AddSub, Term, Expr)) --> term(Term), addOp(AddSub), expr(Expr).

/*_____________________________________________________________________________*/

parse(TokenList, AST) :- phrase(prog(AST),TokenList).

/*_____________________________________________________________________________*/
% 				Semantics(evaluations)
/*______________________________________________________________________________*/


    eval(ID, State, State, Val) :-
		id(ID), get_assoc(ID, State, Val),
		Val \== nil.
                  
    eval(declaredVar(ID), PreState, PostState, _)    :-
		\+ get_assoc(ID, PreState, _), 
		put_assoc(ID, PreState, nil, PostState). 
 	

    eval(assignedVar(Base, ID), PreState, PostState, _)    :- 
		get_assoc(ID, PreState, _), 
		eval(Base, PreState, PreState, Val),
		put_assoc(ID, PreState, Val, PostState). 

   

    eval(returnedExpr(Base), PreState, PostState, Val)    :-   eval(Base, PreState, PostState, Val).


    eval(Num, State, State, Num)               :- number(Num).
   
    eval(trm(Op, L, R), State, State, Var)     :- 
                				eval(L, State, State, Var1), eval(R, State, State, Var2), 
						computeTrm(Op, Var1, Var2, Var).

    eval(expression(Op, L, R), State, State, Var)     :- 
							eval(L, State, State, Var1), eval(R,State, State, Var2), 
                                                	computeExpr(Op, Var1, Var2, Var).

    eval([Statement], State, State, Val)    :-   eval(Statement, State, State, Val).

    eval([Statement|Rest], PreState, PostState, Val)   :- eval(Statement, PreState, MiddleState, _), 
							eval(Rest, MiddleState, PostState, Val).

	computeTrm(Mul, L, R, Val) :- Mul = '*', Val is L * R.
	computeTrm(Div, L, R, Val) :- Div = '/', Val is L / R.
	computeExpr(Add, L, R, Val) :- Add = '+', Val is L + R.
	computeExpr(Sub, L, R, Val) :- Sub = '-', Val is L - R.

    evaluate(AST,ANS) :-	
	  empty_assoc(State),			% passing an empty assoc to start with,
  		eval(AST, State, _, ANS).		% no PostState needed

/*__________________________________________________________________________________*/


