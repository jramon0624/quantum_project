:- use_module(library(http/json)).

measure_from_file :-
    File = 'state.json',
    open(File, read, Stream),
    json_read_dict(Stream, Dict),
    close(Stream),
    State = Dict.state,
    process_state(State).

process_state(State) :-
    collapse_state(State, Result),
    format('Resultado de la medición: ~w~n', [Result]),
    save_result(Result).

 collapse_state(State, Result) :-
    flatten(State, Flattened),
    calculate_probabilities(Flattened, Probs),
    random(Rand),
    find_collapse(Probs, Rand, Result).

calculate_probabilities(Amplitudes, Probabilities) :-
    maplist(abs_square, Amplitudes, Probabilities).

abs_square(Amp, Prob) :-
    Real = Amp.real,
    Imag = Amp.imag,
    Prob is Real * Real + Imag * Imag.

find_collapse([P|_], Rand, 0) :- Rand =< P, !.
find_collapse([P|Ps], Rand, N) :-
    NextRand is Rand - P,
    find_collapse(Ps, NextRand, N1),
    N is N1 + 1.

random(Value) :-
    Value is random_float().

save_result(Result) :-
    open('result.txt', write, Stream),
    format(Stream, '~w', [Result]),
    close(Stream).