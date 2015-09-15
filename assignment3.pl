component(m1, mul, a, c, x).
component(m2, mul, b, d, y).
component(m3, mul, c, e, z).

component(a1, add, x, y, f).
component(a2, add, y, z, g).

multiply(A, B, Result) :-
	Result =:= A * B.

adder(A, B, Result) :-
	Result =:= A + B.

forward(A, B, C, D, E, F, G) :-
	multiply(A, C, X),
	multiply(B, D, Y),
	multiply(C, E, Z),
	adder(X, Y, F),
	adder(Y, Z, G).

backward(A, B, C, D, E, X, Y, Z, F, G) :-
	numbers(L),
	member(A, L),
	member(B, L),
	member(C, L),
	member(D, L),
	member(E, L),
	X is A * C,
	Y is B * D,
	Z is C * E,
	F =:= X + Y,
	G =:= Y + Z.


numbers(List) :-
	numbers(List, 0).
numbers([], 21) :-
	!.
numbers(List, X) :-
	XPlusOne is X + 1,
	numbers(RemainingList, XPlusOne),
	List = [X | RemainingList].
	
go :-
	write('Input five numbers: '),
	read(A),
	read(B),
	read(C),
	read(D),
	read(E),
	forward(A, B, C, D, E, F, G),
	write('Result is : '),
	write(F),
	write(' and '),
	write(G).