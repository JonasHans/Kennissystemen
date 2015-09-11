%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Jonas van Oenen, 10670947 %%%%
%% Jochem Barelds, 5795567   %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

:- abolish(is_subclass_of/2).
:- dynamic is_subclass_of/2.
:- abolish(property/4).
:- dynamic property/4.
:- abolish(property/3).
:- dynamic property/3.

%%%%%%%%%%
% Facts. %
%%%%%%%%%%

is_subclass_of(animal, entity).

is_subclass_of(fish, animal).
is_subclass_of(reptile, animal).
is_subclass_of(bird, animal).
is_subclass_of(mammal, animal).

is_subclass_of(salmon, fish).
is_subclass_of(tortoise, reptile).
is_subclass_of(crocodile, reptile).
is_subclass_of(singingbird, bird).
is_subclass_of(predatorbird, bird).
is_subclass_of(whale, mammal).
is_subclass_of(ape, mammal).
is_subclass_of(elephant, mammal).
is_subclass_of(feline, mammal).

is_subclass_of(salmon, fish).
is_subclass_of(sparrow, singingbird).
%% is_subclass_of(eagle, predatorbird).
is_subclass_of(lion, feline).
is_subclass_of(cat, feline).
is_subclass_of(baboon, ape).

% If the cardinality is missing, assume it is 1.
property(E, P, X, allValuesFrom) :-
	property(E, P, X).

property(reptile, has_legs, leg, 4).
property(bird, has_legs, leg, 2).
property(mammal, has_legs, leg, 4).

% General properties.
property(mammal, temperature, warm_blooded).
property(mammal, has_coat, fur).
property(bird, has_coat, feathers).
property(bird, temperature, warm_blooded).
property(reptile, has_coat, scutes).
property(reptile, temperature, cold_blooded).
property(fish, temperature, cold_blooded).
property(fish, has_coat, scales).

% Specific properties about a type of animal (not individuals).
property(crocodile, size, large).
property(tortoise, size, small).
property(salmon, size, small).
property(cat, size, small).
property(lion, size, large).
property(elephant, size, gigantic).


%%%%%%%%%%%%%%%%
% Derivations. %
%%%%%%%%%%%%%%%%

isA(Entity, X) :-
	is_subclass_of(Entity, X).

isA(Entity, X) :-
	\+ is_subclass_of(Entity, X),
	isA(Y, X),
	isA(Entity, Y).

% Find all parents of the given entity.
parents(entity, []) :-
	!.
parents(Entity, Parents) :-
	is_subclass_of(Entity, Parent),
	parents(Parent, ParentParents),
	Parents = [Parent | ParentParents].

children(Entity, Children):-
	findall(X, is_subclass_of(X, Entity), Children).

% A leaf is an entity without any children.
leaf(Entity, Parent) :-
	isA(Entity, Parent),
	\+ is_subclass_of(_, Entity).

% Collect all leaves of the given entity.
leaves(Entity, Leaves) :-
	findall(X, leaf(X, Entity), Leaves).

% Find all properties of the given entity, recursively.
properties(entity, []).
properties(Entity, Properties) :-
	is_subclass_of(Entity, Parent),
	properties(Parent, ParentProperties),	
	findall([Property, Value, Cardinality], property(Entity, Property, Value, Cardinality), LocalProperties),
	append(ParentProperties, LocalProperties, Properties),
	!.

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

%%%%%%%%%%%%%%%%%%%%%%%%
% Show predicates. %
%%%%%%%%%%%%%%%%%%%%%%%%

show(Concept):-
	write('Showing information on concept : '),
	writeln(Concept),

	parents(Concept, Parents),
	write('Parents : '),
	writeln(Parents),

	children(Concept, Children),
	write('Direct child/children : '),
	writeln(Children),

	properties(Concept, Properties),
	write('Properties : '),
	writeln(Properties).


%%%%%%%%%%%%%%%%%%%%%%%%
% Shortcut predicates. %
%%%%%%%%%%%%%%%%%%%%%%%%

%% Adds a fully new and unrelated concept
go1 :-
	writeln('Plant is added as a new concept, unrelated to all other concepts.'),
	writeln(''),
	reassert(is_subclass_of(plant, entity)),
	show(plant).

%% Adds a subumed concept(eagle), under predatorbird which will inherite all properties from predatorbird 
%% and its parents.
go2 :-
	writeln('Eagle added as a new concept, subsumed under predatorbird.'),
	writeln(''),
	reassert(is_subclass_of(eagle, predatorbird)),
	show(eagle).

%% Adds a concept (gorilla) under ape and at a new property, gorilla will now have that new property and 
%% all it's inherited properties.
go3 :-
	writeln('Gorilla is added as a new concept subsumed under ape but with a unique property, diet : herbivore.'),
	writeln(''),
	reassert(is_subclass_of(gorilla, ape)),
	reassert(property(gorilla, diet, herbivore)),
	show(gorilla).

%% Adds food gathering ass a new concept which 2 animals have.
go4:-
	writeln('New concept, food_gathering, added and 2 animals are given a relation with this concept.'),
	writeln(''),
	reassert(property(lion, food_gathering, hunting)),
	reassert(property(crocodile, food_gathering, hunting)),
	show(lion),
	writeln(''),
	show(crocodile).

%% Adds a new concept with properties and a subsumed concept which inherites these properties.
go5 :-
	writeln('Saltwater fish added as a new concept subsumed under fish.'),
	writeln('Saltwater fish are given the property, environment : saltwater.'),
	writeln('Barracuda is added as a new concept subsumed under saltwater fish and inheriting all properties.'),
	writeln(''),
	reassert(is_subclass_of(saltwater_fish, fish)),
	reassert(property(saltwater_fish, environment, saltwater)),
	reassert(is_subclass_of(barracuda, saltwater_fish)),
	show(saltwater_fish),
	writeln(''),
	show(barracuda).

%% Tests whether an entity can be found by describing its properties.
go6 :-
	writeln('This method finds a concept based on specified properties.'),
	writeln(''),
	properties(Concept, [[temperature, warm_blooded, allValuesFrom], [has_coat, fur, allValuesFrom], [has_legs, leg, 4], [size, small, allValuesFrom]]),
	writeln('properties(E, [[temperature, warm_blooded, allValuesFrom], [has_coat, fur, allValuesFrom], [has_legs, leg, 4], [size, small, allValuesFrom]])'),
	write('Concept = '),
	writeln(Concept).

%% Adds a concept based on where it should belong in the current concepts
go7 :-
	writeln('This method finds a concept with the same properties as : '),
	writeln('[temperature, warm_blooded, allValuesFrom], [has_coat, fur, allValuesFrom], [has_legs, leg, 4], [size, small, allValuesFrom].'),
	writeln('It will then add the concept next to the found matching concept. (In this case it finds cat as a match and adds jaguar)'),
	writeln(''),
	properties(Concept, [[temperature, warm_blooded, allValuesFrom], [has_coat, fur, allValuesFrom], [has_legs, leg, 4], [size, small, allValuesFrom]]),
	is_subclass_of(Concept, Parent),
	reassert(is_subclass_of(jaguar, Parent)),
	show(Concept),
	writeln(''),
	show(jaguar).


