:- abolish(is_subclass_of/2).
:- dynamic is_subclass_of/2.

is_subclass_of(fish, animals).
is_subclass_of(reptiles, animals).
is_subclass_of(birds, animals).
is_subclass_of(mammals, animals).

is_subclass_of(salmon, fish).
is_subclass_of(tortoise, reptiles).
is_subclass_of(crocodile, reptiles).
is_subclass_of(singingbirds, birds).
is_subclass_of(predatorbirds, birds).
is_subclass_of(whale, mammals).
is_subclass_of(apes, mammals).
is_subclass_of(elephant, mammals).
is_subclass_of(feline, mammals).

is_subclass_of(salmon, fish).
is_subclass_of(sparrow, singingbirds).
is_subclass_of(eagle, predatorbirds).
is_subclass_of(lion, feline).
is_subclass_of(cat, feline).
is_subclass_of(baboon, apes).

restriction(mammals, temperature, warm_blooded).
restriction(reptiles, temperature, cold_blooded).

property(whale, size, big).
property(salmon, size, small).
property(tortoise, size, small).

property(cat, size, small).
property(cat, skin, fur).
property(cat, paws, 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% Methoden %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% De taxonomie van een animal
parent_classes(animals, X):-
	write('Parent classes : '),
	writeln(X).

parent_classes(Animal, Taxonomie):-
	parent_animal(Animal, FamilieOuder),
	parent_classes(FamilieOuder, [FamilieOuder|Taxonomie]).

%% De directe familie van een die
parent_animal(Animal, FamilieOuder):-
	is_subclass_of(Animal, FamilieOuder).

getProperties(Animal, _):-
	findall(Y, property(Animal, _, Y), List),
	write('Properties :'),
	write(List).

getRestrictions(Animal,_):-
	findall(Y, restriction(Animal, _, Y), List),
	writeln(List).

getInheritedProperties(animals, List):-
	write('Inherited properties : '),
	writeln(List).

getInheritedProperties(Animal, List):-
	parent_animal(Animal, Parent),
	findall(Y2, restriction(Parent, _, Y2), List2),
	getInheritedProperties(Parent, [List2|List]).

%% Show Concept
show(Animal):-
	parent_classes(Animal, []),
	write('Directe Sub classes : '),
	findall(X, is_subclass_of(X, Animal), List),
	writeln(List),
	getProperties(Animal, []),
	getRestrictions(Animal, []),
	getInheritedProperties(Animal, []).

%% Show tree
tree(Concept):-
	writeln(Concept),
	writeln('        |'),
	findall(X, is_subclass_of(X, Concept), [H|T]),
	writeln([H|T]),
	findall(Y, is_subclass_of(Y, H), List),
	writeln('        |'),
	write(List),
	write('-'),
	findall(Z, is_subclass_of(Z, T), List2),
	writeln(List2).

%% Inserts plants concept into tree
go1 :-
	assertz(is_subclass_of(animals, things)),
	assertz(is_subclass_of(plants, things)),
	tree(things).






