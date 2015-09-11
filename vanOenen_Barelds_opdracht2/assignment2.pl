%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Jonas van Oenen, 10670947 %%%%
%% Jochem Barelds,           %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

% Main diseases with their neccesary symptoms(sufficient symptoms can be derived from child deseases)
disease(malaria, [fever]).
disease(intestinal, [fever, stomach_ache]).
disease(worms, [stool_worms]).

% child parent relation between diseases
child_disease(malaria, malaria_tropicana).
child_disease(malaria, malaria_tertiana).
child_disease(intestinal, bacillary_dysentery).
child_disease(intestinal, giardiasis).
child_disease(worms, threadworms).
child_disease(worms, strongyloides_stereordis).

% Specific diseases with their neccesary and sufficient symptoms
s_diseases(malaria_tropicana, [constant_fever], [cold_shivers, sweat]).
s_diseases(malaria_tertiana, [peaks_fever], [headache, vomiting]).
s_diseases(bacillary_dysentery, [diarhea], [cramps]).
s_diseases(giardiasis, [diarhea], [bloated, nauseous]).
s_diseases(threadworms, [itching_anus, length_1], [stomach_ache]).
s_diseases(roundworms, [length_30], [diarhea, no_appetite, stomach_ache]).

% All valid symptoms.
available_symptoms([weak_feeling, dizziness, nauseous, headache]).

% Check whether the given symptoms are valid.
are_symptoms([]).
are_symptoms(Symptoms) :-
	Symptoms = [Symptom | RemainingSymptoms],
	is_symptom(Symptom),
	are_symptoms(RemainingSymptoms).

% Check whether the given symtpom is valid.
is_symptom(Symptom) :-
	available_symptoms(Symptoms),
	member(Symptom, Symptoms).

% Derive symptoms from temperature.
temperature_symptoms(Temperature, Symptoms) :-
	(
		Temperature > 36,
		Temperature < 38,
		Symptoms = [],
		!;

		Temperature >= 38,
		Temperature < 39,
		Symptoms = [high_temperature],
		!;

		Temperature >= 39,
		Symptoms = [fever],
		!;

		Symptoms = []
	).

% Main predicate. Acts as an interface between the user and the system.
go :-
	write('What is your body temperature? '),
	input_temperature(Temperature),
	write('Which of the following symptoms apply to you?\n'),
	write('You can choose multiple symptoms from the following list: '),
	available_symptoms(AvailableSymptoms),
	writeln(AvailableSymptoms),
	write('Symptoms: '),
	input_symptoms(Temperature, RemainingSymptoms),
	temperature_symptoms(Temperature, TemperatureSymptoms),
	append(RemainingSymptoms, TemperatureSymptoms, Symptoms),
	write('You have the following symptoms: '),
	write(Symptoms).

% Input the temperature.
input_temperature(Temperature) :-
	read(InputTemperature),
	(
		% Only accept positive numbers.
		number(InputTemperature),
		InputTemperature > 0,
		Temperature = InputTemperature,
		!;

		% Repeat until a valid temperature is entered.
		write('Please enter a valid body temperature: '),
		input_temperature(Temperature)
	).

% Input the remaining symptoms.
input_symptoms(Temperature, Symptoms) :-
	read(InputSymptoms),
	(
		% Only accept symptoms in the available symptoms list.
		are_symptoms(InputSymptoms),
		Symptoms = InputSymptoms,
		!;

		% Repeat until valid symptoms are entered.
		write('Please only enter valid symptoms from the list: '),
		input_symptoms(Temperature, Symptoms)
	).

checkDisease(Symptoms, Disease):-
	findall(X, disease(_, X), List),
	checkSymptomsList(List, Symptoms, Disease).

checkSymptomsList([H|T], SpecifiedSymptoms, Disease):-
	member(SpecifiedSymptoms, H),
