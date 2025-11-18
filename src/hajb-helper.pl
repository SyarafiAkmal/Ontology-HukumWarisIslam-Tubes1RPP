/* has_heir_type(List, Type) */
has_heir_type(HeirsList, Type) :-
    member(Type, HeirsList).

/* count_heir_type(List, Type, Count) */
count_heir_type(HeirsList, Type, Count) :-
    findall(1, member(Type, HeirsList), List),
    length(List, Count).

/* has_descendant(DeceasedID, HeirsList) */
has_descendant(HeirsList) :-
    ( member(son, HeirsList)
    ; member(daughter, HeirsList)
    ; member(grandson, HeirsList)
    ; member(granddaughter, HeirsList)
    ).

/* has_male_descendant(HeirsList) */
has_male_descendant(HeirsList) :-
    ( member(son, HeirsList)
    ; member(grandson, HeirsList)
    ).
    
/* has_female_descendant(HeirsList) */
has_female_descendant(HeirsList) :-
    ( member(daughter, HeirsList)
    ; member(granddaughter, HeirsList)
    ).