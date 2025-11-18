consultFiles:-
    consult('C:/Users/ASUS/Documents/Kuliah/Sem 7/RPP/Ontology-HukumWarisIslam-Tubes1RPP/src/parser.pl').

/* RULES */
deceased_has_children(D, Result):-
    ((parent_child(D, Child, _), person(Child, _, "Alive"))
    ->  Result = true ; Result = false
    ).

/* SPOUSES */
/* husband inherits from wife */
/* has no children */
inherit_from(Deceased, Husband, Share):-
    person(Husband, "Male", "Alive"),
    married(Husband, Deceased),
    deceased_has_children(Husband, false),
    Share = '1/2'.

/* has children */
inherit_from(Deceased, Husband, Share):-
    person(Husband, "Male", "Alive"),
    married(Husband, Deceased),
    deceased_has_children(Husband, true),
    Share = '1/4'.

/* wife inherits from husband */
/* has no children */
inherit_from(Deceased, Wife, Share):-
    person(Wife, "Female", "Alive"),
    married(Deceased, Wife),
    deceased_has_children(Deceased, false),
    Share = '1/4'.

/* has children */
inherit_from(Deceased, Wife, Share):-
    person(Wife, "Female", "Alive"),
    married(Deceased, Wife),
    deceased_has_children(Deceased, true),
    Share = '1/8'.

/* ASCENDANTS */
/* father inherits from deceased */
/* has no children */
inherit_from(Deceased, Father, Share):-
    person(Father, "Male", "Alive"),
    parent_child(Father, Deceased, _),
    deceased_has_children(Deceased, false),
    married(Father, Mother),
    married(Deceased, Spouse),
    (person(Mother, "Female", "Alive"), person(Spouse, _, "Alive") -> Share = '2/3 * T' ; Share = 'T').

/* has children */
inherit_from(Deceased, Father, Share):-
    person(Father, "Male", "Alive"),
    parent_child(Father, Deceased, _),
    deceased_has_children(Deceased, true),
    (person(Child, "Male", "Alive"), parent_child(Deceased, Child, _) -> Share = '1/6' ; Share = '1/6 + T').

/* mother inherits from deceased */
/* has no children */
inherit_from(Deceased, Mother, Share):-
    person(Mother, "Female", "Alive"),
    married(Father, Mother),
    parent_child(Father, Deceased, _),
    deceased_has_children(Deceased, false),
    married(Deceased, Spouse),
    (person(Father, "Male", "Alive"), person(Spouse, _, "Alive") -> Share = '1/3 * T' ; Share = '1/3').

/* has children */
inherit_from(Deceased, Mother, Share):-
    person(Mother, "Female", "Alive"),
    person(Father, "Male", _),
    married(Father, Mother),
    deceased_has_children(Deceased, true),
    Share = '1/6'.

/* paternal grandfather */
/* has children */
inherit_from(Deceased, PaternalGrandfather, Share):-
    person(PaternalGrandfather, "Male", "Alive"),
    parent_child(PaternalGrandfather, Father, paternal),
    parent_child(Father, Deceased, paternal),
    person(Father, "Male", deceased),
    deceased_has_children(Deceased, true),
    (person(Child, "Male", "Alive"), parent_child(Deceased, Child, paternal) -> Share = '1/6' ; Share = '1/6 + T').

/* has no children */
inherit_from(Deceased, PaternalGrandfather, Share):-
    person(PaternalGrandfather, "Male", "Alive"),
    parent_child(PaternalGrandfather, Father, paternal),
    parent_child(Father, Deceased, paternal),
    person(Father, "Male", deceased),
    deceased_has_children(Deceased, false),
    Share = 'T'.

/* paternal grandmother */
inherit_from(Deceased, Grandmother, Share):-
    person(Grandmother, "Female", "Alive"),
    married(Grandfather, Grandmother),
    parent_child(Grandfather, Parent, paternal),
    parent_child(Parent, Deceased, paternal),
    Share = '1/6'.

/* maternal grandmother */
inherit_from(Deceased, Grandmother, Share):-
    person(Grandmother, "Female", "Alive"),
    married(Grandfather, Grandmother),
    parent_child(Grandfather, Parent, maternal),
    parent_child(Parent, Deceased, maternal),
    Share = '1/6'.






