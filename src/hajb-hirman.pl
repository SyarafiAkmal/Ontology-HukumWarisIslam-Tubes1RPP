% --- BEGIN Hajb al-Hirman (Full Exclusion) Rules ---
% is_excluded(HeirType, DeceasedID, HeirsList)
% Succeeds if HeirType is fully excluded by someone in HeirsList.

% Rule: Father excludes Grandfather 
is_excluded(grandfather_paternal, _, HeirsList) :- 
    has_heir_type(HeirsList, father), !.

% Rule: Mother excludes Grandmothers
is_excluded(grandmother_maternal, _, HeirsList) :- 
    has_heir_type(HeirsList, mother), !.

is_excluded(grandmother_paternal, _, HeirsList) :- 
    has_heir_type(HeirsList, mother), !.

% Rule: Father also excludes Paternal Grandmother
is_excluded(grandmother_paternal, _, HeirsList) :- 
    has_heir_type(HeirsList, father), !.

% Rule: Son excludes Grandson and Granddaughter 
is_excluded(grandson, _, HeirsList) :- 
    has_heir_type(HeirsList, son), !.

is_excluded(granddaughter, _, HeirsList) :- 
    has_heir_type(HeirsList, son), !.

% Rule: Two or more Daughters exclude Granddaughter 
is_excluded(granddaughter, _, HeirsList) :- 
    count_heir_type(HeirsList, daughter, N), 
    N >= 2, 
    \+ has_heir_type(HeirsList, grandson), !.

% Rule: Descendants or Father exclude Full Brothers and Sisters
is_excluded(full_brother, _, HeirsList) :- 
    ( has_male_descendant(HeirsList)
    ; has_heir_type(HeirsList, father)
    ; has_heir_type(HeirsList, grandfather_paternal)
    ), !.

is_excluded(full_sister, _, HeirsList) :- 
    ( has_male_descendant(HeirsList)
    ; has_heir_type(HeirsList, father)
    ; has_heir_type(HeirsList, grandfather_paternal)
    ), !.

% Rule: Maternal siblings excluded by any descendant or father/grandfather
is_excluded(maternal_brother, _, HeirsList) :- 
    ( has_descendant(HeirsList)
    ; has_heir_type(HeirsList, father)
    ; has_heir_type(HeirsList, paternal_grandfather)
    ), !.

is_excluded(maternal_sister, _, HeirsList) :- 
    ( has_descendant(HeirsList)
    ; has_heir_type(HeirsList, father)
    ; has_heir_type(HeirsList, paternal_grandfather)
    ), !.

% Rule: Full brothers son excluded by full brother or full sister in some condition
is_excluded(full_brothers_son, _, HeirsList) :-
    (is_excluded(full_brother, _, HeirsList)
    ; has_heir_type(HeirsList, full_brother)
    ; is_excluded(full_sister, _, HeirsList)
    ; get_provisional_share(full_sister, HeirsList, asabah)
    ), !.

% Rule: Paternal siblings excluded by descendants, father, or full brother
is_excluded(paternal_sister, _, HeirsList) :-
    ( is_excluded(full_brother, _, HeirsList)
    ; has_heir_type(HeirsList, full_brother)
    ; is_excluded(full_sister, _, HeirsList)
    ; has_heir_type(HeirsList, full_sister)
    ), !.

is_excluded(paternal_brother, _, HeirsList) :-
    (is_excluded(full_brothers_son, _, HeirsList)
    ; has_heir_type(HeirsList, full_brothers_son)
    ; is_excluded(paternal_sister, _, HeirsList)
    ; get_provisional_share(paternal_sister, HeirsList, asabah)
    ), !.

% Rule: Paternal brothers son excluded by paternal brother and others excluding him
is_excluded(paternal_brothers_son, _, HeirsList) :-
    (is_excluded(paternal_brother, _, HeirsList)
    ; has_heir_type(HeirsList, paternal_brother)
    ), !.

% Rule: Paternal uncles excluded by paternal brothers son and others excluding him
is_excluded(full_uncle_paternal, _, HeirsList) :-
    ( is_excluded(paternal_brothers_son, _, HeirsList)
    ; has_heir_type(HeirsList, paternal_brothers_son)
    ), !.

% Rule: Fathers paternal brother excluded by paternal uncles and others excluding him
is_excluded(fathers_paternal_brother, _, HeirsList) :-
    ( is_excluded(full_uncle_paternal, _, HeirsList)
    ; has_heir_type(HeirsList, full_uncle_paternal)
    ), !.

% Rule: Full paternal uncles son excluded by fathers paternal brothers and others excluding him
is_excluded(full_paternal_uncles_son, _, HeirsList) :-
    ( is_excluded(fathers_paternal_brother, _, HeirsList)
    ; has_heir_type(HeirsList, fathers_paternal_brother)
    ), !.

% Rule: Fathers paternal brothers son excluded by full paternal uncles son and others excluding him
is_excluded(fathers_paternal_brothers_son, _, HeirsList) :-
    ( is_excluded(full_paternal_uncles_son, _, HeirsList)
    ; has_heir_type(HeirsList, full_paternal_uncles_son)
    ), !.

% --- END Hajb al-Hirman Rules ---