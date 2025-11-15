consultFiles:-
    ['family_example.pl'].

/* RULES */
deceased_has_children(D, Result):-
    ((parent_child(D, Child, _), person(Child, _, alive))
    ->  Result = true ; Result = false
    ).

/* SPOUSES */
/* husband inherits from wife */
/* has no children */
inherit_from(Deceased, Husband, Share):-
    person(Husband, male, alive),
    spouse(Husband, Deceased),
    deceased_has_children(Husband, false),
    Share = '1/2'.

/* has children */
inherit_from(Deceased, Husband, Share):-
    person(Husband, male, alive),
    spouse(Husband, Deceased),
    deceased_has_children(Husband, true),
    Share = '1/4'.

/* wife inherits from husband */
/* has no children */
inherit_from(Deceased, Wife, Share):-
    person(Wife, female, alive),
    spouse(Deceased, Wife),
    deceased_has_children(Deceased, false),
    Share = '1/4'.

/* has children */
inherit_from(Deceased, Wife, Share):-
    person(Wife, female, alive),
    spouse(Deceased, Wife),
    deceased_has_children(Deceased, true),
    Share = '1/8'.

/* ASCENDANTS */
/* father inherits from deceased */
/* has no children */
inherit_from(Deceased, Father, Share):-
    person(Father, male, alive),
    parent_child(Father, Deceased, _),
    deceased_has_children(Deceased, false),
    spouse(Father, Mother),
    (person(Mother, female, alive) -> Share = '2/3 * T' ; Share = 'T').

/* has children */
inherit_from(Deceased, Father, Share):-
    person(Father, male, alive),
    parent_child(Father, Deceased, _),
    deceased_has_children(Deceased, true),
    (person(Child, male, alive), parent_child(Deceased, Child, _) -> Share = '1/6' ; Share = '1/6 + T').

/* mother inherits from deceased */
/* has no children */
inherit_from(Deceased, Mother, Share):-
    person(Mother, female, alive),
    spouse(Father, Mother),
    parent_child(Father, Deceased, _),
    deceased_has_children(Deceased, false),
    (person(Father, male, alive) -> Share = '1/3 * T' ; Share = '1/3').

/* has children */
inherit_from(Deceased, Mother, Share):-
    person(Mother, female, alive),
    person(Father, male, _),
    spouse(Father, Mother),
    deceased_has_children(Deceased, true),
    Share = '1/6'.



