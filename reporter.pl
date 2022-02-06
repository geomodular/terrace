:- module(reporter, [report/2]).
:- table (make_arrow/2, ccode/2, ccode/3, ccode/4).

find_line(Fd, Offset, Line, Col, Data) :-
    find_line(Fd, Offset, Line, Col, Data, vars{ line: 0, pos: 0}).

find_line(Fd, Offset, Line, Col, Data, vars{ line: L, pos: N }) :-
    read_line_to_codes(Fd, Codes),
    length(Codes, Len),
    L0 is L + 1,
    N0 is N + Len + 1,
    (   N0 < Offset
    ->  find_line(Fd, Offset, Line, Col, Data, vars{ line: L0, pos: N0 })
    ;   Data = Codes, 
        Col is (Len + 1) - (N0 - Offset),
        Line = L0
    ).

mark(C1, CodesIn, Offset, Len, CodesOut) :-
    ccode(C1, Ccode),
    ccode(color(reset), R),
    nth0(Offset, CodesOut0, Ccode, CodesIn),
    length(CodesIn, CodesInLen),
    length(CodesOut0, CodesOut0Len),
    Offset0 is Offset + Len + (CodesOut0Len - CodesInLen),
    nth0(Offset0, CodesOut1, R, CodesOut0),
    flatten(CodesOut1, CodesOut).

make_arrow(0, []) :- !.
make_arrow(1, `^`) :- !.
make_arrow(N, [45|T]) :-
    !,
    N0 is N - 1,
    make_arrow(N0, T).

color(reset, 0).
color(bright, 1).
color(dim, 2).
color(black, 30).
color(red, 31).
color(green, 32).
color(yellow, 33).
color(blue, 34).
color(magenta, 35).
color(cyan, 36).
color(white, 37).
color(onblack, 40).
color(onred, 41).
color(ongreen, 42).
color(onyellow, 43).
color(onblue, 44).
color(onmagenta, 45).
color(oncyan, 46).
color(onwhite, 47).

ccode(C1, X) :-
    C1 = color(_),
    call(C1, C10),
    format(codes(X), "~c[~dm", [27, C10]).
ccode(C1, C2, X) :-
    C1 = color(_), C2 = color(_),
    call(C1, C10), call(C2, C20),
    format(codes(X), "~c[~d;~dm", [27, C10, C20]).
ccode(C1, C2, C3, X) :-
    C1 = color(_), C2 = color(_), C3 = color(_),
    call(C1, C10), call(C2, C20), call(C3, C30),
    format(codes(X), "~c[~d;~d;~dm", [27, C10, C20, C30]).

report(File, token{ type: comment, value: _, offset: Offset, len: _, err: warning(missingSpace) }) :-
    !, 
    setup_call_cleanup(open(File, read, In),
      find_line(In, Offset, Line, Col, Codes),
      close(In)),
    ccode(color(bright), color(white), C),
    ccode(color(reset), R),
    mark(color(red), Codes, Col, 1, Codes0),
    format("error: no space in between a pipe and a comment, on line ~s~d~s:\n~s\n", [C, Line, R, Codes0]),
    ArrowSize is Col + 1,
    make_arrow(ArrowSize, Arrow),
    format("~s\n", [Arrow]).
report(_, _).

:- begin_tests(reporter).
:- use_module(scanner).
:- use_module(utility).

test(report, [setup(read_input_data("./tests/s4.tr", Data))]) :-
    phrase(scan(_, Err), Data),
    nth0(0, Err, Err0),
    report("./tests/s4.tr", Err0).

test(arrow) :-
    make_arrow(0, A),
    A == ``. % is this a right comparison?

test(arrow) :-
    make_arrow(1, A),
    A == `^`.

test(arrow) :-
    make_arrow(2, A),
    A == `-^`.

test(arrow) :-
    make_arrow(3, A),
    A == `--^`.

:- end_tests(reporter).
