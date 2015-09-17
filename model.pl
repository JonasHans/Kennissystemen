:- dynamic val/3.

component(m1, mul, a, c, x).
component(m2, mul, b, d, y).
component(m3, mul, c, e, z).

component(a1, add, x, y, f).
component(a2, add, y, z, g).

val(1, a, 5).
val(1, b, 8).
val(1, c, 10).
val(1, d, 15).
val(1, e, 8).

val(2, f, 170).
val(2, g, 200).

add(X, Y, Z) :-
	Z is X + Y.

mul(X, Y, Z) :-
	Z is X * Y.


numbers(L) :-
	numbers(L, 0).
numbers([], X) :-
	X > 100,
	!.
numbers(L, X) :-
	X2 is X + 1,
	numbers(L2, X2),
	L = [X | L2].
numb(X) :-
	numbers(L),
	member(X, L).	
	
forward(M) :-
	component(_, Type, X1, X2, Y),
	val(M, X1, X1Value),
	val(M, X2, X2Value),
	\+ val(M, Y, _),
	Predicate =.. [Type, X1Value, X2Value, YValue],
	call(Predicate),
	assertz(val(M, Y, YValue)),
	write('Derived '),
	write_ln(val(M, Y, YValue)),
	fail.

backward(M) :-
	component(_, Type, X1, X2, Y),
	val(M, Y, YValue),
	(
		\+ val(M, X1, _),
		\+ val(M, X2, _),
		numb(X1Value),
		numb(X2Value);

		\+ val(M, X1, _),
		val(M, X2, X2Value),
		numb(X1Value);

		val(M, X1, X1Value),
		\+ val(M, X2, _),
		numb(X2Value)
	),
	Predicate =.. [Type, X1Value, X2Value, YValue],
	call(Predicate),
	assertz(val(M, X1, X1Value)),
	assertz(val(M, X2, X2Value)),
	fail.