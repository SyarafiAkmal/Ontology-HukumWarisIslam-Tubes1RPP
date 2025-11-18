% --- BEGIN Ashab al-Furud (Fixed Share) Rules ---
% get_provisional_share(HeirType, HeirsList, Share)

% --- Spouses ---
get_provisional_share(wife, HeirsList, 1/8) :- 
    has_descendant(HeirsList), !.
get_provisional_share(wife, _, 1/4) :- !.

get_provisional_share(husband, HeirsList, 1/4) :- 
    has_descendant(HeirsList), !.
get_provisional_share(husband, _, 1/2) :- !.

% --- Parents ---
get_provisional_share(mother, HeirsList, 1/6) :-
    ( has_descendant(HeirsList)
    ; count_heir_type(HeirsList, full_brother, FB), FB >= 2
    ; count_heir_type(HeirsList, full_sister, FS), FS >= 2
    ; count_heir_type(HeirsList, paternal_brother, PB), PB >= 2
    ), !.
get_provisional_share(mother, _, 1/3) :- !.

get_provisional_share(father, HeirsList, 1/6) :-
    has_male_descendant(HeirsList), !.
get_provisional_share(father, HeirsList, combine) :-
    has_female_descendant(HeirsList), !.
get_provisional_share(father, _, asabah) :- !.

% --- Grandparents ---
get_provisional_share(grandfather_paternal, HeirsList, 1/6) :-
    has_male_descendant(HeirsList), !.
get_provisional_share(grandfather_paternal, HeirsList, combine) :-
    has_female_descendant(HeirsList), !.
get_provisional_share(grandfather_paternal, _, asabah) :- !.

get_provisional_share(grandmother_maternal, HeirsList, 1/12) :-
    has_heir_type(grandmother_paternal, HeirsList), !.
get_provisional_share(grandmother_maternal, _, 1/6) :- !.
get_provisional_share(grandmother_paternal, HeirsList, 1/12) :-
    has_heir_type(grandmother_maternal, HeirsList), !.
get_provisional_share(grandmother_paternal, _, 1/6) :- !.

% --- Sons and Daughters ---
get_provisional_share(son, _, asabah) :- !.

get_provisional_share(daughter, HeirsList, asabah) :-
    has_heir_type(HeirsList, son), !.
get_provisional_share(daughter, HeirsList, 1/2) :-
    count_heir_type(HeirsList, daughter, 1), !.
get_provisional_share(daughter, HeirsList, DividedShare) :-
    count_heir_type(HeirsList, daughter, N), N >= 2,
    frac_multiply(2/3, 1/N, DividedShare), !.

% --- Grandsons and Granddaughters ---
get_provisional_share(grandson, _, asabah) :- !.

get_provisional_share(granddaughter, HeirsList, asabah) :-
    has_heir_type(HeirsList, grandson), !.
get_provisional_share(granddaughter, HeirsList, 1/6) :-
    count_heir_type(HeirsList, daughter, 1),
    \+ has_heir_type(HeirsList, grandson), !.
get_provisional_share(granddaughter, HeirsList, 1/2) :-
    count_heir_type(HeirsList, granddaughter, 1),
    \+ has_heir_type(HeirsList, daughter),
    \+ has_heir_type(HeirsList, grandson), !.
get_provisional_share(granddaughter, HeirsList, DividedShare) :-
    count_heir_type(HeirsList, granddaughter, N), N >= 2,
    frac_multiply(2/3, 1/N, DividedShare),
    \+ has_heir_type(HeirsList, daughter),
    \+ has_heir_type(HeirsList, grandson), !.

% --- Full Siblings ---
get_provisional_share(full_brother, _, asabah) :- !.

get_provisional_share(full_sister, HeirsList, asabah) :-
    has_heir_type(HeirsList, full_brother), !.
get_provisional_share(full_sister, HeirsList, asabah) :-
    (has_heir_type(HeirsList, granddaughter) ; has_heir_type(HeirsList, daughter)), !.
get_provisional_share(full_sister, HeirsList, 1/2) :-
    count_heir_type(HeirsList, full_sister, 1), !.
get_provisional_share(full_sister, HeirsList, DividedShare) :-
    count_heir_type(HeirsList, full_sister, N), N >= 2,
    frac_multiply(2/3, 1/N, DividedShare), !.

% --- Maternal Siblings (Uterine) ---
get_provisional_share(maternal_brother, HeirsList, 1/6) :-
    count_heir_type(HeirsList, maternal_brother, 1),
    count_heir_type(HeirsList, maternal_sister, 0), !.
get_provisional_share(maternal_brother, _, 1/3) :- !.

get_provisional_share(maternal_sister, HeirsList, 1/6) :-
    count_heir_type(HeirsList, maternal_sister, 1),
    count_heir_type(HeirsList, maternal_brother, 0), !.
get_provisional_share(maternal_sister, _, 1/3) :- !.

% --- Paternal Siblings ---
get_provisional_share(paternal_brother, _, asabah) :- !.

get_provisional_share(paternal_sister, HeirsList, asabah) :-
    has_heir_type(HeirsList, paternal_brother), !.
get_provisional_share(paternal_sister, HeirsList, 1/2) :-
    count_heir_type(HeirsList, paternal_sister, 1), !.
get_provisional_share(paternal_sister, HeirsList, DividedShare) :-
    count_heir_type(HeirsList, paternal_sister, N), N >= 2,
    frac_multiply(2/3, 1/N, DividedShare), !.

% --- Others ---
get_provisional_share(full_brothers_son, _, asabah) :- !.
get_provisional_share(paternal_brothers_son, _, asabah) :- !.
get_provisional_share(full_uncle_paternal, _, asabah) :- !.
get_provisional_share(fathers_paternal_brother, _, asabah) :- !.
get_provisional_share(full_paternal_uncles_son, _, asabah) :- !.
get_provisional_share(fathers_paternal_brothers_son, _, asabah) :- !.
% --- END Ashab al-Furud Rules ---