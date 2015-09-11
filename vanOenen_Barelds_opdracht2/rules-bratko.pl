/* --- Defining operators --- */

:- op(800, fx, if).
:- op(700, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).

:- dynamic fact/1.

/* --- Example rules --- */

if p then q.

if q and r then s.

if q or p then t.

/* --- Example facts --- */

fact(p).

fact(r).

/* --- A simple backward chaining rule interpreter from Bratko --- */

is_true( P ):-
    fact( P ).

is_true( P ):-
    if Condition then P,
    is_true( Condition ).

is_true( P1 and P2 ):-
    is_true( P1 ),
    is_true( P2 ).

is_true( P1 or P2 ):-
    is_true( P1 )
    ;
    is_true( P2 ).


/* --- A simple forward chaining rule interpreter from Bratko --- */

forward:-
    new_derived_fact( P ),
    !,
    write( 'Derived:' ), write_ln( P ),
    assert( fact( P )),
    forward
    ;
    write_ln( 'No more facts' ).

forward:-
    test_true( P ),
    not( fact( P )),
    assert( fact( P )),
    false.

new_derived_fact( Conclusion ):-
    if Condition then Conclusion,
    not( fact( Conclusion ) ),
    composed_fact( Condition ).

composed_fact( Condition ):-
    fact( Condition ).

composed_fact( Condition1 and Condition2 ):-
    composed_fact( Condition1 ),
    composed_fact( Condition2 ).

composed_fact( Condition1 or Condition2 ):-
    composed_fact( Condition1 )
    ;
    composed_fact( Condition2 ).

/* --- A simple rule interpreter with two modes --- */

backward( P ):-
    test_true( P ).

test_true( P ):-
    fact( P ).

test_true( P ):-
    if Condition then P,
    reduce( Condition ).

reduce( P1 and P2 ):-
    test_true( P1 ),
    reduce( P2 ).

reduce( P1 or P2 ):-
    test_true( P1 )
    ;
    reduce( P2 ).

reduce(P):-
	test_true( P ).
