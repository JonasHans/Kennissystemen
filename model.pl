:- [reassert].
:- dynamic val/3.

% Components of model 1
component(m1, mul, a, c, x).
component(m2, mul, b, d, y).
component(m3, mul, c, e, z).
component(a1, add, x, y, f).
component(a2, add, y, z, g).

%% Abstract sketch of model 2
%% 
%% a3 -> m4 - \
%% a4 -> m5 - -> a7 -> r
%% a5 -> m6 - -> a8 -> s
%% a6 -> m7 - /
%% 

%% Components of model 2
component(a3, add, a, b, i).
component(a4, add, c, d, j).
component(a5, add, e, f, k).
component(a6, add, g, h, l).

component(m4, mul, i, j, u).
component(m5, mul, j, k, o).
component(m6, mul, k, j, p).
component(m7, mul, k, l, q).

component(a7, add, u, o, r).
component(a8, add, p, q, s).

%% General priori rates for multipliers and adders
failure_rate(C, 0.0006) :-
	component(C, mul, _, _, _).
failure_rate(C, 0.0004) :-
	component(C, add, _, _, _).

output_var(X) :-
	component(_, _, _, _, X),
	(
		\+ component(_, _, X, _, _),
		\+ component(_, _, _, X, _)
	).

% Model 1, Input values(example for forward chaining)
val(1, a, 5).
val(1, b, 8).
val(1, c, 10).
val(1, d, 15).
val(1, e, 8).

% Model 3, Output values(example for backward chaining)
val(3, f, 170).
val(3, g, 200).

% Model 2, Input values (Example with 10 components)
val(2, a, 9).
val(2, b, 12).
val(2, c, 8).
val(2, d, 6).
val(2, e, 2).
val(2, f, 10).
val(2, g, 16).
val(2, h, 5).

% The defined adder and multiplier components
add(X, Y, Z) :-
	Z is X + Y.
mul(X, Y, Z) :-
	Z is X * Y.

% Construct a list with all valid numbers.
numbers(L) :-
	numbers(L, 0).
numbers([], X) :-
	X > 100,
	!.
numbers(L, X) :-
	X2 is X + 1,
	numbers(L2, X2),
	L = [X | L2].

% Check/generate a number.
numb(X) :-
	numbers(L),
	member(X, L).	
	
% Forward chaining.
forward(M) :-
	% For every possible component...
	component(_, Type, X1, X2, Y),

	% Check the values of the input variables X1 and X2 in the model M.
	val(M, X1, X1Value),
	val(M, X2, X2Value),

	% Check if the value of the variable Y is not already defined in the model M.
	\+ val(M, Y, _),

	% Construct an inner gate predicate.
	Predicate =.. [Type, X1Value, X2Value, YValue],

	% Call the inner gate predicate, and fetch the value of the output variable Y.
	call(Predicate),

	% Assert the newly derived value for variable Y as a new fact in model M.
	reassert(val(M, Y, YValue)),

	write('Derived '),
	write_ln(val(M, Y, YValue)),

	% Continue to make derivations.
	fail.

% Backward chaining.
backward(M) :-
	% For every possible component...
	component(_, Type, X1, X2, Y),

	% Check the value of the value of the output variable Y inside model M.
	val(M, Y, YValue),
	(
		% If neither of the vales of the input variables X1 and X2 are defined in model M...
		(\+ val(M, X1, _),
		\+ val(M, X2, _),

		% Generate possible values for the input variables X1 and X2.
		numb(X1Value),
		numb(X2Value));

		% If one of the values of the input variables X1 and X2 is defined in model M, 
		% derive only the other.
		(\+ val(M, X1, _),
		val(M, X2, X2Value),
		numb(X1Value));

		(val(M, X1, X1Value),
		\+ val(M, X2, _),
		numb(X2Value))
	),

	% Construct an inner gate predicate.
	Predicate =.. [Type, X1Value, X2Value, YValue],
	
	% Check if the values of the input variables X1 and X2 match up to the value of the output variable Y
	call(Predicate),

	% Assert the derived values of the input variables X1 and X2 as new facts in model M.
	reassert(val(M, X1, X1Value)),
	reassert(val(M, X2, X2Value)),

	write('Derived '),
	write(val(M, X1, X1Value)),
	write(', '),
	write_ln(val(M, X2, X2Value)),
	
	% Continue to make derivations.
	fail.

faulty_vars(_, [], []) :-
	!.
faulty_vars(M, Variables, FaultyVariables) :-
	Variables = [H | R],
	measure_var(H, HValue),
	val(M, H, RealHValue),
	(
		(
			HValue = RealHValue,
			MoreFaultyVariables = []
		);
		(
			\+ HValue = RealHValue,
			MoreFaultyVariables = [[H, HValue]]
		)
	),
	faulty_vars(M, R, RemainingFaultyVariables),
	append(RemainingFaultyVariables, MoreFaultyVariables, FaultyVariables).

measure_var(X, Input) :-
	write('What is the value of '),
	write(X),
	write('? '),
	read(Input),
	write_ln(' ').

% find the component linked to the specified output varianle
findComp(OutputVar, Comp):-
	component(Comp, _, _, _, OutputVar).

findFaultyComp([], Answer):-
	write('The faulty component is '),
	writeln(Answer).

%% Find most likely faulty component set
findFaultyCompSet([[OutputVar|_]|_]):-
	%%  Find the set with Highest chance of fault
	findHighestPrioriSet(OutputVar, 0, SetOfComp),
	%% Test for the faulty components set
	testFaultyComp(SetOfComp, []).

% Faulty component is found
testFaultyComp([], FaultyVariable):-
	write('Faulty component is '),
	writeln(FaultyVariable).

% Test for finding faulty component
testFaultyComp([H|T], FaultyVariables):-
	%%  find the input for first component of probable faulty set
	component(H, _, Input1, Input2, _),
	%% If no faulty variable go to next component in probable faulty set
	faulty_vars(1, [Input1,Input2], FaultyVariables),
	testFaultyComp(T, FaultyVariables).

%% Find set of components with Highest faulty rate (RETURNED OP DIT MOMENT EEN LEGE SET, moet anders)
findHighestPrioriSet(X, Rate, Set):-
	member(X,[a,b,c,d,e]),
	write('Highest failure rate found on set '),
	write(Set),
	write(' with rate '),
	writeln(Rate).

% METHODE VIND NU NOG ALLEEN DE MEEST LINKER SET
findHighestPrioriSet(Start, Rate, Set):-
	component(Comp, _, X, _, Start),
	failure_rate(Comp, FR),
	NewRate is Rate + FR,
	append([Comp], Set, NewSet),
	findHighestPrioriSet(X, NewRate, NewSet).

go(M) :-
	\+ forward(M),
	findall(X, output_var(X), OutputVariables),
	faulty_vars(M, OutputVariables, FaultyVariables),
	print(FaultyVariables),
	findFaultyCompSet(FaultyVariables).
	
