%     ['var','x',';','x','<-','(',5,'*',2,')',';','return','(','x','+',1,')','.']



parse(List, AST) :- Tree = prog(AST), phrase(Tree, List, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prog(prog(X)) --> retStatement(X), ['.'].
prog(prog(X,Y)) --> declaration(X), [';'], prog(Y).
prog(prog(X,Y)) --> assignment(X), [';'], prog(Y).
prog(prog(X,Y)) --> declAssignment(X), [';'], prog(Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

retStatement(retStatement(X)) --> ['return'] , base(X).
declaration(declaration(X)) --> ['var'] , [X], {variable(X)}.
assignment(assignment(X,Y)) --> [X], {variable(X)} , ['<-'], base(Y).
declAssignment(declAssignment(X,Y)) --> ['var'], [X], {variable(X)}, ['<-'], base(Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

base(N) --> [N], {number(N)}.
base(Var) --> [Var] , {variable(Var)}.
base(Expr) --> ['('], expression(Expr),[')'].
base(Term) --> ['('], term(Term),[')'].

factor(factor(B)) --> base(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
term(F) --> factor(F).

term(T) --> factor(F1),
            mulOp(MulDiv),
            left_assoc_term(T, term(F1), MulDiv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expression(T) --> term(T).

expression(E) -->
            term(T),
            addOp(AddSub),
            left_assoc(E, expression(T), AddSub).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

left_assoc(expression(T1, AddSub, T2), T1, AddSub) --> term(T2).

left_assoc(E, T1, AddSub) -->
                         term(T2),
                         addOp(AddSub2),
                         left_assoc(E, expression(T1, AddSub, T2), AddSub2).


left_assoc_term(term(F1, MulDiv, F2), F1, MulDiv) --> factor(F2).

left_assoc_term(T, F1, MulDiv) -->
                            factor(F2),
                            mulOp(MulDiv2),
                            left_assoc_term(T, term(F1, MulDiv, F2), MulDiv2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addOp(addOp('+')) --> ['+'].
addOp(addOp('-')) --> ['-'].
mulOp(mulOp('*')) --> ['*'].
mulOp(mulOp('/')) --> ['/'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reserved(X) :- member(X,['var','return','<-', '+', '-','/','*',';','.']).
variable(X) :- \+ reserved(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute(X,Y, AddSub, Val):- AddSub = '+', Val is X + Y.
compute(X,Y, AddSub, Val):- AddSub = '-', Val is X - Y.
compute(X,Y, MulDiv, Val):- MulDiv = '*', Val is X * Y.
compute(X,Y, MulDiv, Val):- MulDiv = '/', Val is X / Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



eval(declaration(X), PreAssoc, PostAssoc, _) :-
                                \+get_assoc(X, PreAssoc, _),
                                put_assoc(X, PreAssoc, nil, PostAssoc).

eval(assignment(X,Y), PreAssoc, PostAssoc, _) :-
                                get_assoc(X, PreAssoc, _),
                                eval(Y, PreAssoc, PreAssoc, Val),
                                put_assoc(X, PreAssoc, Val, PostAssoc).

eval(declAssignment(X,Y), PreAssoc, PostAssoc, _) :-
                                \+get_assoc(X, PreAssoc, _),
                                eval(Y, PreAssoc, PreAssoc, Val),
                                put_assoc(X, PreAssoc, Val, PostAssoc).

eval(retStatement(X), PreAssoc, PostAssoc, Val) :-
                                eval(X, PreAssoc, PostAssoc, Val).

eval(expression(X), Assoc, Assoc, Val) :-
                                eval(X, Assoc, Assoc, Val).

eval(expression(X, addOp(Op),Y), Assoc, Assoc, Val) :-
                                eval(X, Assoc, Assoc, Val1),
                                eval(Y, Assoc, Assoc, Val2),
                                compute(Val1,Val2,Op,Val).

eval(term(X), Assoc, Assoc, Val) :-
                                eval(X, Assoc, Assoc, Val).

eval(term(X,mulOp(Op),Y), Assoc, Assoc, Val) :-
                                eval(X, Assoc, Assoc, Val1),
                                eval(Y, Assoc, Assoc, Val2),
                                compute(Val1,Val2,Op,Val).

eval(Number, Assoc, Assoc, Number) :-
                                number(Number).

eval(Variable, Assoc, Assoc, Val) :-
                                variable(Variable),
                                get_assoc(Variable, Assoc, Val),
                                Val \== nil.

eval(factor(X), Assoc, Assoc, Val) :-
                                eval(X, Assoc, Assoc, Val).

eval(prog(X), PreAssoc, PostAssoc, Val) :-
                                eval(X, PreAssoc, PostAssoc, Val).

eval(prog(X,Y), PreAssoc, PostAssoc, Val) :-
                                eval(X, PreAssoc, MidAssoc, _),
                                eval(Y, MidAssoc, PostAssoc, Val).


eval([Last], Assoc, Assoc, Val) :-
                                eval(Last, Assoc, Assoc, Val).


eval([Head|Last], PreAssoc, PostAssoc, Val) :-
                                eval(Head, PreAssoc, MidAssoc, _),
                                eval(Last, MidAssoc, PostAssoc, Val).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evaluate(AST, ANS) :- empty_assoc(Assoc), eval(AST, Assoc, _, ANS).
















