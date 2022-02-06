:- begin_tests(scanner).
:- use_module(scanner).
:- use_module(utility).

test_folder("./tests/").

expand_terrace_file(File, Expanded) :-
    test_folder(Tests),
    string_concat(Tests, File, Path),
    string_concat(Path, ".tr", Expanded).

expand_term_file(File, Expanded) :-
    test_folder(Tests),
    string_concat(Tests, File, Path),
    string_concat(Path, ".pl", Expanded).

prepare([H], Data, Term) :-
    expand_terrace_file(H, Tr),
    expand_term_file(H, Tm),
    read_input_data(Tr, Data),
    read_input_term(Tm, Term),
    !.
prepare([H|T], Data, Term) :-
    expand_terrace_file(H, Tr),
    expand_term_file(H, Tm),
    (   read_input_data(Tr, Data),
        read_input_term(Tm, Term)
    ;   prepare(T, Data, Term)
    ).

test(scan, [forall(prepare(["s1", "s2", "s3"], Data, Expected))]) :-
    phrase(scan(Actual, []), Data),
    ( Expected = Actual -> true; format("\nExpected: ~w\n  Actual: ~w\n", [Expected, Actual]), fail ).

test(scan, [setup(read_input_data("./tests/s4.tr", Data))]) :-
    Expected = [
        token{err:warning(missingSpace), len:8, offset:12, type:comment, value:`comment`}
    ],
    phrase(scan(_, Actual), Data),
    ( Expected = Actual -> true; format("\nExpected: ~w\n  Actual: ~w\n", [Expected, Actual]), fail ).

:- end_tests(scanner).
