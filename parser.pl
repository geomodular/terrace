:- module(parser, [parse//2]).

% category  -> CATEGORY mline ( entry | comment )* mline
% category+ -> CATEGORY pline ( entry | comment )* pline
% entry     -> indent pipe (category+ | var )?
% comment   -> indent COMMENT
% var       -> KEY VALUE

parse([H|T], [EH|ET]) -->
    category(H, EH), !, parse(T, ET).
parse([H|T], [EH|ET]) -->
    eat_comment(Comment, _, _), !, { H = comment(Comment), EH = [] }, parse(T, ET).
parse([], []) --> [].

category(X, Err) -->
    eat_category(Name, _, Len0), eat_minus_line(_, Len1), entry(Entries, E), eat_minus_line(_, _), { Len0 =:= Len1 }, !,
    { X = category(Name, Entries), Err = E }.
categoryp(X, Err) -->
    eat_category(Name, _, _), eat_indent(_, _), eat_plus_line(_, _), entry(Entries, E), eat_indent(_, _), eat_plus_line(_, _), !,
    { X = category(Name, Entries), Err = E }.
categoryp(X, Err) -->
    eat_category(Name, _, _), eat_indent(_, _), eat_plus_line(_, _), entry(Entries, E), !,
    { X = category(Name, Entries), Err = E }.

entry([H|T], [EH|ET]) -->
    eat_indent(_, _), eat_comment(Comment, _, _), !,
    { H = comment(Comment), EH = [] }, entry(T, ET).
entry([H|T], [EH|ET]) -->
    eat_indent(_, _), eat_pipe(_, _), eat_key(Key, _, _), eat_value(Value, _, _), !,
    { H = var(Key, Value), EH = [] }, entry(T, ET).
entry([H|T], [EH|ET]) -->
    eat_indent(_, _), eat_pipe(_, _), categoryp(Categories, Err), !,
    { H = Categories, EH = Err }, entry(T, ET).
entry(X, Err) --> eat_indent(_, _), eat_pipe(_, _), !, entry(X, Err).
entry([], []) --> [].

eat_category(Name, Offset, Len) --> [token{ type: category, value: Name, offset: Offset, len: Len }].
eat_minus_line(Offset, Len) --> [token{ type: line, value: '-', offset: Offset, len: Len }].
eat_plus_line(Offset, Len) --> [token{ type: line, value: '+', offset: Offset, len: Len }].
eat_indent(Offset, Len) --> [token{ type: indent, value: [], offset: Offset, len: Len }].
eat_pipe(Offset, Len) --> [token{ type: pipe, value: [], offset: Offset, len: Len }].
eat_key(Name, Offset, Len) --> [token{ type: key, value: Name, offset: Offset, len: Len }].
eat_value(Value, Offset, Len) --> [token{ type: value, value: Value, offset: Offset, len: Len }].
eat_comment(Value, Offset, Len) --> [token{ type: comment, value: Value, offset: Offset, len: Len }].

:- begin_tests(parser).
:- use_module(scanner).
:- use_module(utility).

test(parser, [setup(read_input_data("./tests/t1.tr", Data))]) :-
    scan(Tokens, [], Data, []),
    parse(AST, [[]], Tokens, []),
    AST = [category(`Category`, [])].

% test(parser, [setup(read_input("./tests/t2.tr", Data))]) :-
%     scan(Tokens, [], Data, []),
%     parse(AST, [warning(lineSize)], Tokens, []),
%     AST = terrace([entry(category(`Category`), [])]).

test(parser, [setup(read_input_data("./tests/t3.tr", Data))]) :-
    scan(Tokens, [], Data, []),
    parse(AST, [[], []], Tokens, []),
    AST = [category(`Category1`, []), category(`Category2`, [])].

% test(parser, [setup(read_input("./tests/t4.tr", Data))]) :-
%     scan(Tokens, [], Data, []),
%     parse(AST, [warning(lineSize)], Tokens, []),
%     AST = terrace([entry(category(`Category1`), []), entry(category(`Category2`), [])]).

test(parser, [setup(read_input_data("./tests/t5.tr", Data))]) :-
    scan(Tokens, [], Data, []),
    parse(AST, [[[]]], Tokens, []),
    AST = [category(`Category1`, [category(`Category2`, [])])].

test(parser, [setup(read_input_data("./tests/t6.tr", Data))]) :-
    scan(Tokens, [], Data, []),
    parse(AST, [[[]]], Tokens, []),
    AST = [category(`Server`, [comment(`My comment`)])].

test(parser, [setup(read_input_data("./tests/t7.tr", Data))]) :-
    scan(Tokens, [], Data, []),
    parse(AST, _, Tokens, []),
    AST = [category(`Category`, [var(`key`, `value`)])].

test(parser, [setup(read_input_data("./tests/t8.tr", Data))]) :-
    scan(Tokens, [], Data, []),
    parse(AST, [[[],[],[],[[],[]]]], Tokens, []),
    AST = [category(`Server`, [comment(`Address`), var(`host`, `localhost`), var(`port`, `8080`), category(`Nodes`, [var(`pc1`, `127.9.9.1`), var(`pc2`, `127.9.9.2`)])])].

test(parser, [setup(read_input_data("./tests/comment.tr", Data))]) :-
    scan(Tokens, [], Data, []),
    parse(AST, [[], [], []], Tokens, []),
    AST = [comment(`This is a comment`), comment(`Category starts with a capital letter`), comment(`Key/Value pair starts with a low letter`)].

:- end_tests(parser).

%terrace(X) -->
    %category(Name, _), minus_line(_), entries(Entries), minus_line(_), !,
    %{ X = entry(category(Name), Entries) }.
%terrace(X) -->
    %category(Name, _), minus_line(_), minus_line(_), !,
    %{ X = entry(category(Name), []) }.
% terrace(X) -->
%     category(Name, L1), minus_line(L2), !,
%     { L1 == L2 -> X = entry(category(Name), []); throw(error(syntax_error(categoryDeclaration), context(loc, "the category name is not aligned with a line below"))) }.
%terrace(X) -->
    %category(Name, L), minus_line(L), !,
    %{ X = entry(category(Name), []) }.
%terrace(_) -->
    %category(_, L1), minus_line(L2), { L1 =\= L2 }, !,
    %{ throw(error(syntax_error(categoryAlignement), context(loc, "the category name is not aligned with a line"))) }. % throw nie je vhodny?
%terrace(X) -->
    %category(Name, _), !,
    %{ X = entry(category(Name), []) }.

%entries([H|T]) -->
    %indent(_), pipe, key(Key), value(Value), !,
    %{ H = var(Key, Value) }, entries(T).
%entries(HT) --> indent(_), comment(_), !, entries(HT).
%entries([H|T]) --> indent(_), pipe, category(Name, _), indent(_), plus_line, entries(Entries), indent(_), plus_line, !,
    %{ H = entry(category(Name), Entries) }, entries(T). 
%entries(HT) --> indent(_), pipe, !, entries(HT).
%entries([]) --> [].
