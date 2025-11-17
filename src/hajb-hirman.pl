% --- BEGIN Hajb al-Hirman (Full Exclusion) Rules ---
% is_excluded(HeirType, DeceasedID, HeirsList)
% Succeeds if HeirType is fully excluded by someone in HeirsList.

% Rule: Son excludes Grandson and Granddaughter 
is_excluded(grandson, _, HeirsList) :- 
    has_heir_type(HeirsList, son), !.

is_excluded(grandson, _, HeirsList) :-
    has_heir_type(HeirsList, daughter),
    \+ has_heir_type(HeirsList, son), !.

is_excluded(granddaughter, _, HeirsList) :- 
    has_heir_type(HeirsList, son), !.

% Rule: Two or more Daughters exclude Granddaughter 
is_excluded(granddaughter, _, HeirsList) :- 
    count_heir_type(HeirsList, daughter, N), 
    N >= 2, 
    \+ has_heir_type(HeirsList, grandson), !.

% Rule: Father excludes Grandfather 
is_excluded(grandfather, _, HeirsList) :- 
    has_heir_type(HeirsList, father), !.

is_excluded(paternal_grandfather, _, HeirsList) :- 
    has_heir_type(HeirsList, father), !.

% Rule: Mother excludes Grandmothers
is_excluded(paternal_grandmother, _, HeirsList) :- 
    has_heir_type(HeirsList, mother), !.

is_excluded(maternal_grandmother, _, HeirsList) :- 
    has_heir_type(HeirsList, mother), !.

% Rule: Father also excludes Paternal Grandmother
is_excluded(paternal_grandmother, _, HeirsList) :- 
    has_heir_type(HeirsList, father), !.

% Rule: Descendants or Father exclude Full Brothers and Sisters
is_excluded(full_brother, _, HeirsList) :- 
    ( has_male_descendant(HeirsList)
    ; has_heir_type(HeirsList, father)
    ), !.

is_excluded(full_sister, _, HeirsList) :- 
    ( has_male_descendant(HeirsList)
    ; has_heir_type(HeirsList, father)
    ), !.

% Rule: Paternal siblings excluded by descendants, father, or full brother
is_excluded(paternal_brother, _, HeirsList) :-
    ( has_male_descendant(HeirsList)
    ; has_heir_type(HeirsList, father)
    ; has_heir_type(HeirsList, full_brother)
    ), !.

is_excluded(paternal_sister, _, HeirsList) :-
    ( has_male_descendant(HeirsList)
    ; has_heir_type(HeirsList, father)
    ; has_heir_type(HeirsList, full_brother)
    ), !.

% Rule: Maternal siblings excluded by any descendant or father/grandfather
is_excluded(maternal_brother, _, HeirsList) :- 
    ( has_descendant(HeirsList)
    ; has_heir_type(HeirsList, father)
    ; has_heir_type(HeirsList, grandfather)
    ; has_heir_type(HeirsList, paternal_grandfather)
    ), !.

is_excluded(maternal_sister, _, HeirsList) :- 
    ( has_descendant(HeirsList)
    ; has_heir_type(HeirsList, father)
    ; has_heir_type(HeirsList, grandfather)
    ; has_heir_type(HeirsList, paternal_grandfather)
    ), !.

% Rule: Paternal uncles excluded by father, grandfather, or brothers
is_excluded(paternal_uncle, _, HeirsList) :-
    ( has_heir_type(HeirsList, father)
    ; has_heir_type(HeirsList, grandfather)
    ; has_heir_type(HeirsList, full_brother)
    ; has_heir_type(HeirsList, paternal_brother)
    ; has_male_descendant(HeirsList)
    ), !.

% --- END Hajb al-Hirman Rules ---