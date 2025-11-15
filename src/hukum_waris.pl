consultFiles:-
    ['family_example.pl'].

/* RULES */
deceased_has_children(D, Result):-
    (parent_child(D, _, _) -> Result = true ; Result = false).

/* husband inherits from wife */
/* has no children */
inherit_from(Deceased, Husband, Share):-
    person(Husband, male),
    spouse(Husband, Deceased),
    deceased_has_children(Deceased, false),
    Share is 1/2.

/* has children */
inherit_from(Deceased, Husband, Share):-
    person(Husband, male),
    spouse(Husband, Deceased),
    deceased_has_children(Deceased, true),
    Share is 1/4.

/* wife inherits from husband */
/* has no children */
inherit_from(Deceased, Wife, Share):-
    person(Wife, female),
    spouse(Deceased, Wife),
    deceased_has_children(Deceased, false),
    Share is 1/4.

/* has children */
inherit_from(Deceased, Wife, Share):-
    person(Wife, female),
    spouse(Deceased, Wife),
    deceased_has_children(Deceased, true),
    Share is 1/8.