:- module(scanner, [scan//2]).

% Two type of eaters. One returns token, the second one not.
% https://www.swi-prolog.org/pldoc/man?section=preddesc

incr(X, X0) --> { X0 is X + 1 }.

peek_alnum, [H] --> [H], { char_type(H, alnum) }.
peek_upper, [H] --> [H], { char_type(H, upper) }.
peek_lower, [H] --> [H], { char_type(H, lower) }.
peek(X), X --> X.

eat_word(Word, Len) --> eat_word(Word, 0, Len).
eat_word([H|T], Acc, Len) --> peek_alnum, !, [H], incr(Acc, Acc0), eat_word(T, Acc0, Len).
eat_word([], 0, _) --> !, { fail }.
eat_word([], Len, Len) --> [].

eat_spaces(Len) --> eat_spaces(0, Len).
eat_spaces(Acc, Len) --> ` `, !, incr(Acc, Acc0), eat_spaces(Acc0, Len).
eat_spaces(0, _) --> !, { fail }.
eat_spaces(Len, Len) --> [].

eat_until_end(Line, Len) --> eat_until_end(Line, 0, Len).
eat_until_end([], Len, Len), `\n` --> `\n`, !.
eat_until_end([H|T], Acc, Len) --> [H], !, incr(Acc, Acc0), eat_until_end(T, Acc0, Len).
eat_until_end([], 0, _) --> !, { fail }.
eat_until_end([], Len, Len) --> [].

eat_category(token{ type: category, value: Name, len: Len }) -->
    peek_upper, eat_word(Name, Len).

eat_key(token{ type: key, value: Name, len: Len }) -->
    peek_lower, eat_word(Name, Len). % is lower necessary?

% eat_pipe(-Token, -Err)// is semidet.
eat_pipe(token{ type: pipe, value: [], len: 2 }, []) --> `| `, !.
eat_pipe(token{ type: pipe, value: [], len: 1 }, []), `\n` --> `|\n`, !.
eat_pipe(token{ type: pipe, value: [], len: 1 }, [warning(missingSpace)]) --> `|`.

% eat_indent(-Token)// is semidet.
eat_indent(token{ type: indent, value: [], len: Len }) --> eat_spaces(Len).

% eat_comment(-Token, -Err)// is semidet.
eat_comment(token{ type: comment, value: Line, len: Len }, []) -->
    `/ `, !, eat_until_end(Line, Len0), { Len is Len0 + 2 }.
eat_comment(token{ type: comment, value: Line, len: Len }, [warning(missingSpace)]) -->
    `/`, eat_until_end(Line, Len0), { Len is Len0 + 1 }.

% eat_value(-Token, -Err)// is semidet.
eat_value(token{ type: value, value: Line, len: Len }, []) -->
    `= `, !, eat_until_end(Line, Len0), { Len is Len0 + 2 }.
eat_value(token{ type: value, value: Line, len: Len }, [warning(missingSpace)]) -->
    `=`, !, eat_until_end(Line, Len0), { Len is Len0 + 1 }.

eat_line_(Len) --> eat_line_(none, 0, Len).
eat_line_(none, Acc, Len) --> `+`, !, incr(Acc, Acc0), eat_line_(43, Acc0, Len).
eat_line_(_, Acc, Len) --> `-`, !, incr(Acc, Acc0), eat_line_(45, Acc0, Len).
eat_line_(45, Acc, Len) --> `+`, !, incr(Acc, Len).
eat_line_(43, Acc, Len) --> `+`, !, incr(Acc, Len).
eat_line_(43, Len, Len) --> !.
eat_line_(_, 0, _) --> { fail }.
eat_line(token{ type: line, value: '-', len: Len }) -->
    peek(`-`), !, eat_line_(Len).
eat_line(token{ type: line, value: '+', len: Len }) -->
    peek(`+`), !, eat_line_(Len).

% add_offset(+Token, +Cursor, -Token0, -Cursor0)// is semidet?
add_offset(Token, Cur, Token0, Cur0) -->
    { Token0 = Token.put(_{ offset: Cur }), Cur0 is Token.len + Cur }.

% expand_err(+Token, +Cursor, -Token0)// is semidet?
expand_err(Token, [E], Token0) -->
    !, { Token0 = Token.put(_{ err: E }) }.
expand_err(_, [], []) --> [].

% scan(-Tokens)// is det?
scan(Tokens, Err) --> scan(Tokens, 0, 0, Err0), { flatten(Err0, Err) }.
% scan(-Tokens, -TokenNumber, -Cursor, -Err)// is det?
scan(HT, _, Cur, Err) --> `\n`, !, incr(Cur, Cur0), scan(HT, 0, Cur0, Err).
scan([H|T], 0, Cur, Err) --> eat_indent(Token), !, add_offset(Token, Cur, H, Cur0), scan(T, 1, Cur0, Err).
scan(HT, TN, Cur, Err) --> ` `, !, incr(Cur, Cur0), scan(HT, TN, Cur0, Err).
scan([H|T], TN, Cur, Err) --> eat_category(Token), !, incr(TN, TN0), add_offset(Token, Cur, H, Cur0), scan(T, TN0, Cur0, Err).
scan([H|T], TN, Cur, Err) --> eat_key(Token), !, incr(TN, TN0), add_offset(Token, Cur, H, Cur0), scan(T, TN0, Cur0, Err).
scan([H|T], TN, Cur, [EH|ET]) --> eat_value(Token, Err), !, incr(TN, TN0), add_offset(Token, Cur, H, Cur0), expand_err(H, Err, EH), scan(T, TN0, Cur0, ET).
scan([H|T], TN, Cur, [EH|ET]) --> eat_pipe(Token, Err), !, incr(TN, TN0), add_offset(Token, Cur, H, Cur0), expand_err(H, Err, EH), scan(T, TN0, Cur0, ET).
scan([H|T], TN, Cur, Err) --> eat_line(Token), !, incr(TN, TN0), add_offset(Token, Cur, H, Cur0), scan(T, TN0, Cur0, Err).
scan([H|T], TN, Cur, [EH|ET]) --> eat_comment(Token, Err), !, incr(TN, TN0), add_offset(Token, Cur, H, Cur0), expand_err(H, Err, EH), scan(T, TN0, Cur0, ET).
scan([], _, _, []) --> [].

:- begin_tests(tokenizer_units).

test(incr) :- incr(1, 2, [], _).

test(eat_word) :- eat_word(`myword`, 6, `myword`, []).
test(eat_word) :- eat_word(`myword`, 6, `myword!rest`, `!rest`).
test(eat_word) :- eat_word(`myword123`, 9, `myword123!rest`, `!rest`).
test(eat_word, [fail]) :- eat_word(_, 0, [], _).
test(eat_word, [fail]) :- eat_word(_, 0, ` myword`, _).

test(eat_spaces) :- eat_spaces(4, `    word`, `word`).
test(eat_spaces, [fail]) :- eat_spaces(0, `word`, _).
test(eat_spaces, [fail]) :- eat_spaces(0, [], _).

test(eat_until_end, [fail]) :- eat_until_end(_, 0, [], _).
test(eat_until_end) :- eat_until_end([], 0, `\n`, `\n`).
test(eat_until_end) :- eat_until_end(`funny`, 5, `funny`, []).
test(eat_until_end) :- eat_until_end(`funny`, 5, `funny\n`, `\n`).
test(eat_until_end) :- eat_until_end(`funny`, 5, `funny\nme`, `\nme`).
test(eat_until_end) :- eat_until_end(`funny`, 5, `funny\nme\n`, `\nme\n`).
test(eat_until_end) :- eat_until_end(`funny me `, 9, `funny me \nme`, `\nme`).

test(eat_line_) :- eat_line_(1, `+`, []).
test(eat_line_, [fail]) :- eat_line_(_, `-`, _).
test(eat_line_) :- eat_line_(2, `++`, []).
test(eat_line_) :- eat_line_(2, `-+`, []).
test(eat_line_) :- eat_line_(3, `--+`, []).
test(eat_line_) :- eat_line_(2, `-+ rest`, ` rest`).
test(eat_line_, [fail]) :- eat_line_(_, ` -+ rest`, _).

test(eat_line) :- eat_line(token{ type: line, value: '-', len: 4 }, `---+Rest`, `Rest`).
test(eat_line) :- eat_line(token{ type: line, value: '+', len: 4 }, `+--+Rest`, `Rest`).
test(eat_line) :- eat_line(token{ type: line, value: '+', len: 1 }, `+`, []).
test(eat_line, [fail]) :- eat_line(_, ` +--+`, _).
test(eat_line, [fail]) :- eat_line(_, [], _).

test(eat_category) :- eat_category(token{ type: category, value: `Cat1`, len: 4 }, `Cat1_Rest`, `_Rest`).
test(eat_category, [fail]) :- eat_category(_, `cat1_Rest`, _).
test(eat_category, [fail]) :- eat_category(_, [], _).

test(eat_key) :- eat_key(token{ type: key, value: `key`, len: 3 }, `key = value`, ` = value`).
test(eat_key, [fail]) :- eat_key(_, ` key = value`, _).
test(eat_key, [fail]) :- eat_key(_, [], _).

test(eat_pipe) :- eat_pipe(token{ type: pipe, value: [], len: 2 }, [], `| key = value`, `key = value`).
test(eat_pipe) :- eat_pipe(token{ type: pipe, value: [], len: 1 }, [warning(missingSpace)], `|key = value`, `key = value`).
test(eat_pipe) :- eat_pipe(token{ type: pipe, value: [], len: 1 }, [], `|\nkey = value`, `\nkey = value`).
test(eat_pipe, [fail]) :- eat_pipe(_, _, ` key = value`, _).
test(eat_pipe, [fail]) :- eat_pipe(_, _, [], _).

test(eat_indent) :- eat_indent(token{ type: indent, value: [], len: 4 }, `    word`, `word`).
test(eat_indent, [fail]) :- eat_indent(_, `word`, _).
test(eat_indent, [fail]) :- eat_indent(_, [], _).

test(eat_comment) :- eat_comment(token{ type: comment, value: `my comment`, len: 12 }, [], `/ my comment`, []).
test(eat_comment) :- eat_comment(token{ type: comment, value: `my comment`, len: 12 }, [], `/ my comment\n`, `\n`).
test(eat_comment) :- eat_comment(token{ type: comment, value: `my comment`, len: 11 }, [warning(missingSpace)], `/my comment`, []).
test(eat_comment, [fail]) :- eat_comment(_, _, ` / my comment`, _).

test(eat_value) :- eat_value(token{ type: value, value: `123`, len: 5 }, [], `= 123`, []).
test(eat_value) :- eat_value(token{ type: value, value: `123`, len: 5 }, [], `= 123\n`, `\n`).
test(eat_value) :- eat_value(token{ type: value, value: `123`, len: 4 }, [warning(missingSpace)], `=123`, []).
test(eat_value, [fail]) :- eat_value(_, _, `key = 123`, _).

:- end_tests(tokenizer_units).
