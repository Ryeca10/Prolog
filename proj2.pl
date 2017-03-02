%   prog ::= retStatement ’.’ | funcDecl ’;’ prog | statement ’;’ prog
%   base ::= < id > | < number > | ’(’ expression ’)’ | funcCall

%   condition ::= base comp base | ’(’ condition logOp condition ’)’ | boolean | ’!’ ’(’ condition ’)’
%   funcDecl ::= ’function’ < id > ’(’ < id > ’)’ ’{’prog ’}’

%   funcCall ::= < id > ’(’ base ’)’
%   retStatement ::= ’return’ base

%   statementSeq ::= statement ’.’ | statement ’;’ statementSeq
%   statement ::= declaration | assignment | declAssignment | conditional | loop

%   loop ::= ’while’ ’(’ condition ’)’ ’do’ statementSeq ’done’
%   conditional ::= ’if’ ’(’ condition ’)’ ’then’ statementSeq [ ’else’ statementSeq ] ’endif’

%   declAssignment ::= var < id > ’<-’ base
%   declaration ::= var < id >
%   assignment ::= < id > ’<-’ base

%   expression ::= [expression addOp] term
%   term ::= [term mulOp] factor
%   factor ::= base

%   addOp ::= ’+’ | ’–’
%   mulOp ::= ’*’ | ’/’
%   logOp ::= ’&&’ | ’||’
%   comp ::= ’==’ | ’<’ | ’>’ | ’<=’ | ’>=’ | ’! =’
%   boolean ::= true | false

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  ['var','x',';','x','<-','(',5,'*',2,')',';','return','(','x','+',1,')','.']
%  ['var', 'x', '<-', 1, ';', 'if', '(', 'x', '<', 0, ')', 'then', 'x', '<-', 10, '.', 'else', 'x', '<-', 20, '.', 'endif', ';', 'return', 'x', '.']
%  ['function', 'f', '(', 'x', ')', '{', 'return', 'x', '.', '}', ';', 'return', 'f', '(', '(', 10, '+', 1, ')',  ')',  '.']

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prog(prog(X)) --> retStatement(X), ['.'].
prog(prog(X,Y)) --> statement(X), [';'], prog(Y).
prog(prog(X,Y)) --> funcDecl(X), [';'], prog(Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

retStatement(retStatement(X)) --> ['return'] , base(X).
declaration(declaration(X)) --> ['var'] , [X], {variable(X)}.
assignment(assignment(X,Y)) --> [X], {variable(X)} , ['<-'], base(Y).
declAssignment(declAssignment(X,Y)) --> ['var'], [X], {variable(X)}, ['<-'], base(Y).
funcDecl(funcDecl(X,Y,Z)) --> ['function'], [X], {variable(X)}, ['('], [Y],{variable(Y)}, [')'], ['{'], prog(Z), ['}'].
loop(loop(X,Y)) --> ['while'], ['('], condition(X), [')'], ['do'], statementSeq(Y), ['done'].
conditional(conditional(X,Y)) --> ['if'], ['('], condition(X), [')'], ['then'], statementSeq(Y), ['endif'].
conditional(conditional(X,Y,Z)) --> ['if'], ['('], condition(X), [')'], ['then'], statementSeq(Y), ['else'], statementSeq(Z), ['endif'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

base(base(N)) --> [N], {number(N)}.
base(base(Var)) --> [Var] , {variable(Var)}.
base(base(Expr)) --> ['('], expression(Expr),[')'].
base(base(Term)) --> ['('], term(Term),[')'].
base(base(Func)) --> [Func] , {variable(Func)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

statement(statement(X)) --> declaration(X).
statement(statement(X)) --> assignment(X).
statement(statement(X)) --> declAssignment(X).
statement(statement(X)) --> conditional(X).
statement(statement(X)) --> loop(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

statementSeq(statementSeq(X)) --> statement(X), ['.'].
statementSeq(statementSeq(X,Y)) --> statement(X), [';'], statementSeq(Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

condition(condition(X)) --> boolean(X).
condition(condition(X)) --> ['!'], ['('], condition(X), [')'].
condition(condition(X,Y,Z)) --> base(X), compOp(Y), base(Z).
condition(condition(X,Y,Z)) --> ['('], condition(X), logOp(Y), condition(Z), [')'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

factor(B) --> base(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

term(F) --> factor(F).

term(T) --> factor(F1),
            mulOp(MulDiv),
            left_assoc_term(T, term(F1), MulDiv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expression(T) --> term(T).

expression(E) --> term(T),
                  addOp(AddSub),
                  left_assoc(E, expression(T), AddSub).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

left_assoc(expression(T1, AddSub, T2), T1, AddSub) --> term(T2).

left_assoc(E, T1, AddSub) -->   term(T2),
                                addOp(AddSub2),
                                left_assoc(E, expression(T1, AddSub, T2), AddSub2).

left_assoc_term(term(F1, MulDiv, F2), F1, MulDiv) --> factor(F2).

left_assoc_term(T, F1, MulDiv) -->  factor(F2),
                                    mulOp(MulDiv2),
                                    left_assoc_term(T, term(F1, MulDiv, F2), MulDiv2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addOp(addOp('+')) --> ['+'].
addOp(addOp('-')) --> ['-'].
mulOp(mulOp('*')) --> ['*'].
mulOp(mulOp('/')) --> ['/'].
logOp(logOp('&&')) --> ['&&'].
logOp(logOp('||')) --> ['||'].
compOp(compOp('==')) --> ['=='].
compOp(compOp('!=')) --> ['!='].
compOp(compOp('<')) --> ['<'].
compOp(compOp('<=')) --> ['<='].
compOp(compOp('>')) --> ['>'].
compOp(compOp('>=')) --> ['>='].
boolean(boolean('true')) --> ['true'].
boolean(boolean('false')) --> ['false'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reserved(X) :- member(X,['var','return','<-', '+', '-','/','*',';','.','==','!=','&&',
                         '||','<','<=','>','>=','function','while','do','if','.', ';','done',
                         'else', 'false', 'true', '(', ')', '{', '}', 'then', 'endif']).

variable(X) :- \+ reserved(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute(X,Y, AddSub, Val):- AddSub = '+', Val is X + Y.
compute(X,Y, AddSub, Val):- AddSub = '-', Val is X - Y.
compute(X,Y, MulDiv, Val):- MulDiv = '*', Val is X * Y.
compute(X,Y, MulDiv, Val):- MulDiv = '/', Val is X / Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

comparison(X,Y,CompOP) :- CompOp = '==', X=Y.
comparison(X,Y,CompOP) :- CompOp = '!=', X\=Y.
comparison(X,Y,CompOP) :- CompOp = '<', X<Y.
comparison(X,Y,CompOP) :- CompOp = '<=', X=<Y.
comparison(X,Y,CompOP) :- CompOp = '>=', X>=Y.
comparison(X,Y,CompOP) :- CompOp = '>', X>=Y.
comparison(X,Y,CompOP) :- CompOp = '&&', X, Y.
comparison(X,Y,CompOP) :- CompOp = '||', (X;Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval(declaration(X), PreAssoc, PostAssoc, _) :- \+get_assoc(X, PreAssoc, _),
                                                put_assoc(X, PreAssoc, nil, PostAssoc).

eval(assignment(X,Y), PreAssoc, PostAssoc, _) :-  get_assoc(X, PreAssoc, _),
                                                  eval(Y, PreAssoc, PreAssoc, Val),
                                                  put_assoc(X, PreAssoc, Val, PostAssoc).

eval(declAssignment(X,Y), PreAssoc, PostAssoc, _) :-  \+get_assoc(X, PreAssoc, _),
                                                      eval(Y, PreAssoc, PreAssoc, Val),
                                                      put_assoc(X, PreAssoc, Val, PostAssoc).

eval(retStatement(X), PreAssoc, PostAssoc, Val) :-  eval(X, PreAssoc, PostAssoc, Val).

eval(expression(X), Assoc, Assoc, Val) :-  eval(X, Assoc, Assoc, Val).

eval(expression(X, addOp(Op),Y), Assoc, Assoc, Val) :- eval(X, Assoc, Assoc, Val1),
                                                       eval(Y, Assoc, Assoc, Val2),
                                                       compute(Val1,Val2,Op,Val).

eval(term(X), Assoc, Assoc, Val) :-  eval(X, Assoc, Assoc, Val).

eval(term(X,mulOp(Op),Y), Assoc, Assoc, Val) :-  eval(X, Assoc, Assoc, Val1),
                                                 eval(Y, Assoc, Assoc, Val2),
                                                 compute(Val1,Val2,Op,Val).

eval(Number, Assoc, Assoc, Number) :- number(Number).

eval(Variable, Assoc, Assoc, Val) :-  variable(Variable),
                                      get_assoc(Variable, Assoc, Val),
                                      Val \== nil.

eval(factor(X), Assoc, Assoc, Val) :-  eval(X, Assoc, Assoc, Val).

eval(prog(X), PreAssoc, PostAssoc, Val) :-  eval(X, PreAssoc, PostAssoc, Val).

eval(prog(X,Y), PreAssoc, PostAssoc, Val) :-  eval(X, PreAssoc, MidAssoc, _),
                                              eval(Y, MidAssoc, PostAssoc, Val).

eval([Last], Assoc, Assoc, Val) :-  eval(Last, Assoc, Assoc, Val).


eval([Head|Last], PreAssoc, PostAssoc, Val) :-  eval(Head, PreAssoc, MidAssoc, _),
                                                eval(Last, MidAssoc, PostAssoc, Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(List, AST) :- Tree = prog(AST), phrase(Tree, List, []).

evaluate(AST, ANS) :-  empty_assoc(Assoc), eval(AST, Assoc, _, ANS).

