:- module(utility, [read_input_data/2, read_input_term/2]).

read_input_data(File, Codes) :-
    setup_call_cleanup(open(File, read, In),
      read_string(In, _, Str),
      close(In)),
    string_codes(Str, Codes).

read_input_term(File, Term) :-
    setup_call_cleanup(open(File, read, In),
      read_term(In, Term, []),
      close(In)).