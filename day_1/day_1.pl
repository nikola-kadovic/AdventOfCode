read_file(Stream, []) :- at_end_of_stream(Stream).
read_file(Stream, [H|T]) :- 
	\+ at_end_of_stream(Stream), 
	read_line_to_string(Stream, Str), 
	string_chars(Str, H),
	read_file(Stream, T)
.

get_input(Input) :-
	open('input.txt', read, Stream),
	read_file(Stream, Input),
	close(Stream)
.

first_digit([H|_], N) :- is_digit(H), atom_number(H, N).
first_digit([_|T], N) :- first_digit(T, N).

last_digit(L, N) :- reverse(L, LR), first_digit(LR, N).
		
solve(_, [], 0).
solve(Type, [List | T], Num) :-
	solve(Type, T, Prev),
	(Type = q1 ->
		first_digit(List, L),
		last_digit(List, R)
	;
		first_digit_string(List, L),
		last_digit_string(List, R)
	),
	Num is (10 * L) + R + Prev
.

q1 :-
	get_input(Input),
	solve(q1, Input, Ans),
	write('Ans: '), write(Ans), nl
.

num(One, 1) :- string_chars('one', One).
num(Two, 2) :- string_chars('two', Two).
num(Three, 3) :- string_chars('three', Three).
num(Four, 4) :- string_chars('four', Four).
num(Five, 5) :- string_chars('five', Five).
num(Six, 6) :- string_chars('six', Six).
num(Seven, 7) :- string_chars('seven', Seven).
num(Eight, 8) :- string_chars('eight', Eight).
num(Nine, 9) :- string_chars('nine', Nine).
num(['1'], 1). num(['2'], 2). num(['3'], 3). num(['4'], 4). 
num(['5'], 5). num(['6'], 6). num(['7'], 7). num(['8'], 8). 
num(['9'], 9).

first_digit_string(List, Num) :-
	append(Left, _, List),
	num(M, Num),
	append(_, M, Left)
.

last_digit_string(List, Num) :-
	reverse(List, ListR),
	append(Left,_ , ListR),
	num(M, Num),
	reverse(M, MR),
	append(_, MR, Left)
.

q2 :-
	get_input(Input),
	solve(q2, Input, Ans),
	write('Ans: '), write(Ans), nl
.

