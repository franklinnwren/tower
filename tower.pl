reverse_rows([], []).
reverse_rows([Head|Tale], [Hd|Tl]):-
    reverse(Head, Hd),
    reverse_rows(Tale, Tl).

transpose([], []).
transpose([Head|Tale], Tl):-
    new_transpose(Head, [Head|Tale], Tl).
new_transpose([], _, []).
new_transpose([_|Remaining], Middle, [Head|Tale]):-
    transpose_helper(Middle, Head, Converted),
    new_transpose(Remaining, Converted, Tale).
transpose_helper([], [], []).
transpose_helper([[Head|Tale]|Tl], [Head|Remaining], [Tale|Rest]):-
    transpose_helper(Tl, Remaining, Rest).

valid_counts(Size, counts(Left, Right, Top, Bottom)):-
    length(Left, Size),
    length(Right, Size),
    length(Top, Size),
    length(Bottom, Size).

valid_matrix(Size, Matrix):- 
    length(Matrix, Size),
    valid_rows(Size, Matrix).
valid_rows(_, []).
valid_rows(Size, [Head|Tale]):-
    length(Head, Size),
    valid_rows(Size, Tale).

valid_permutation([], _, _).
valid_permutation([Head|Tale], Result, Postperm):-
    permutation(Result, Head),
    valid_column_permutation(Head, Postperm),
    valid_permutation(Tale, Result, [Head|Postperm]).
valid_column_permutation(_, []).
valid_column_permutation(Current, [Head|Tale]):-
    maplist(not_equal, Current, Head),
    valid_column_permutation(Current, Tale).
not_equal(A, B):-
    A #\= B.

longest_increasing_subsequence([], _, 1).
longest_increasing_subsequence([Head|Tale], Tempmax, Accumulation):-
    Head #> Tempmax,
    Update #= Accumulation - 1,
    longest_increasing_subsequence(Tale, Head, Update).
longest_increasing_subsequence([Head|Tale], Tempmax, Accumulation):-
    Head #=< Tempmax,
    longest_increasing_subsequence(Tale, Tempmax, Accumulation).
give_counts([], _).
give_counts([[Hd|Tl]|Tale], [Counts|Remaining]):-
    longest_increasing_subsequence([Hd|Tl], Hd, Counts),
    give_counts(Tale, Remaining).

plain_tower(0, [], counts([],[],[],[])).
plain_tower(N, T, counts(Top, Bottom, Left, Right)):-
    counts(Top, Bottom, Left, Right) = C,
    valid_counts(N, C),
    valid_matrix(N, T),
    findall(Placeholder, between(1, N, Placeholder), Result),
    valid_permutation(T, Result, []),
    give_counts(T, Left),
    transpose(T, Ready_for_top),
    give_counts(Ready_for_top, Top),
    reverse_rows(T, Ready_for_right),
    give_counts(Ready_for_right, Right),
    reverse_rows(Ready_for_top, Ready_for_bottom),
    give_counts(Ready_for_bottom, Bottom).

tower(0, [], counts([],[],[],[])).
tower(N, T, counts(Top, Bottom, Left, Right)):-
    counts(Top, Bottom, Left, Right) = C,
    valid_counts(N, C),
    valid_matrix(N, T),
    valid_domain(N, T),
    valid_number(T),
    transpose(T, Ready_for_top),
    valid_number(Ready_for_top),
    give_counts(T, Left),
    give_counts(Ready_for_top, Top),
    reverse_rows(T, Ready_for_right),
    give_counts(Ready_for_right, Right),
    reverse_rows(Ready_for_top, Ready_for_bottom),
    give_counts(Ready_for_bottom, Bottom),
    get_results(T).

valid_domain(_, []).
valid_domain(Max_num, [Head|Tale]):-
    fd_domain(Head, 1, Max_num),
    valid_domain(Max_num, Tale).

valid_number([]).
valid_number([Head|Tale]):-
    fd_all_different(Head),
    valid_number(Tale).

get_results([]).
get_results([Head|Tale]):-
    fd_labeling(Head),
    get_results(Tale).

speedup(Ratio):-
    statistics(cpu_time, [Start|_]),
    tower(5, _, counts([2,3,2,1,4],[3,1,3,3,2],[4,1,2,5,2],[2,4,2,1,2])),
    statistics(cpu_time,[Stop|_]),
    Runtime is Stop - Start,
    statistics(cpu_time,[Start_|_]),
    plain_tower(5, _, counts([2,3,2,1,4],[3,1,3,3,2],[4,1,2,5,2],[2,4,2,1,2])),
    statistics(cpu_time,[Stop_|_]),
    Runtime_ is Stop_ - Start_,
    Ratio is Runtime_ / Runtime.

ambiguous(Number,Count,Tower1,Tower2):-
    tower(Number,Tower1,Count),
    tower(Number,Tower2,Count),
    Tower1 \= Tower2.
