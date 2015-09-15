component(m1, mul, a, c, x).
component(m2, mul, b, d, y).
component(m3, mul, c, e, z).

component(a1, add, x, y, f).
component(a2, add, y, z, g).

multiply(A, B, Result) :-
	Result =:= A * B.

adder(A, B, Result) :-
	Result is A + B.

forward(A, B, C, D, E, F, G) :-
	multiply(A, C, X),
	multiply(B, D, Y),
	multiply(C, E, Z),
	adder(X, Y, F),
	adder(Y, Z, G).

%backward(A, B, C, D, E, X, Y, Z, F, G) :-
	
	
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