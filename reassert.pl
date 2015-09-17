% Re-assert a fact. If it already exists, it is retracted and then re-added.
% This is necessary in order to avoid repeating facts.
reassert(Fact) :-
	(
		% Only retract if the fact already exists.
		call(Fact),
		retract(Fact),
		!;

		% Otherwise do not retract.
		\+ call(Fact)
	),
	assertz(Fact).