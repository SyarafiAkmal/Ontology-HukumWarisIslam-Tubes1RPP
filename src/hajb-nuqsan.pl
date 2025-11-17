% --- BEGIN Ashab al-Furud (Fixed Share) Rules ---
% Note: These rules MUST be defined in order of priority,
% using cuts (!) to ensure only one rule matches.
% The main predicate will handle 'is_excluded' *before* calling this.

:- initialization(consult('hajb-helper.pl')).

% get_provisional_share(HeirType, HeirsList, Share)

:- dynamic(get_provisional_share/3).

% --- Spouses ---
get_provisional_share(wife, HeirsList, 1/8) :- 
    has_descendant(HeirsList),!.
get_provisional_share(wife, _, 1/4).

get_provisional_share(husband, HeirsList, 1/4) :- 
    has_descendant(HeirsList),!.
get_provisional_share(husband, _, 1/2).

% --- Ascendants (Mother, Father) ---
get_provisional_share(mother, HeirsList, 1/6) :-
    ( has_descendant(HeirsList)
    ; count_heir_type(HeirsList, full_sibling, FS), FS >= 2
    ; count_heir_type(HeirsList, paternal_sibling, PS), PS >= 2
    ; count_heir_type(HeirsList, maternal_sibling, MS), MS >= 2
    ; count_heir_type(HeirsList, full_sibling, FS), 
      count_heir_type(HeirsList, paternal_sibling, PS),
      count_heir_type(HeirsList, maternal_sibling, MS),
      FS + PS + MS >= 2
    ),!.
% (Special Umariyyatayn cases would go here)
get_provisional_share(mother, _, 1/3).

get_provisional_share(father, HeirsList, 1/6) :-
    has_male_descendant(HeirsList),!.
get_provisional_share(father, HeirsList, '1/6 + asabah') :-
    has_female_descendant(HeirsList),
    \+ has_male_descendant(HeirsList),!.
get_provisional_share(father, _, asabah).

% --- Descendants (Daughter, Granddaughter) ---
% Son is pure 'asabah'
get_provisional_share(son, _, asabah).

% Daughter becomes 'asabah' if a Son is present
get_provisional_share(daughter, HeirsList, asabah) :-
    has_heir_type(HeirsList, son),!.
% Daughter gets 1/2 if she is the only daughter
get_provisional_share(daughter, HeirsList, 1/2) :-
    count_heir_type(HeirsList, daughter, 1),!.
% Daughters get 2/3 (shared) if 2 or more, and no son
get_provisional_share(daughter, HeirsList, 2/3) :-
    count_heir_type(HeirsList, daughter, N), N >= 2,!.

% (Similar logic for Granddaughter, Full Sister, Paternal Sister...)
% E.g., Granddaughter (if no Son, and < 2 Daughters)
get_provisional_share(granddaughter, HeirsList, asabah) :-
    has_heir_type(HeirsList, grandson),!.
get_provisional_share(granddaughter, HeirsList, 1/6) :-
    count_heir_type(HeirsList, daughter, 1),!.
%...etc.

% --- Uterine Siblings ---
get_provisional_share(maternal_sibling, HeirsList, 1/6) :-
    count_heir_type(HeirsList, maternal_sibling, 1),!.
get_provisional_share(maternal_sibling, HeirsList, 1/3) :-
    count_heir_type(HeirsList, maternal_sibling, N), N >= 2,!.
    
% --- END Ashab al-Furud Rules ---