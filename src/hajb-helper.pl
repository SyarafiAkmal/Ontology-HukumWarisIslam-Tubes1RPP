% --- BEGIN Helper Predicates for Hajb ---

% has_heir_type(List, Type)
% Succeeds if at least one heir of Type is in the List.
has_heir_type(HeirsList, Type) :-
    member(Type, HeirsList).

% count_heir_type(List, Type, Count)
% Counts how many heirs of Type are in the List.
count_heir_type(HeirsList, Type, Count) :-
    findall(1, member(Type, HeirsList), List),
    length(List, Count).

% has_descendant(DeceasedID, HeirsList)
% Succeeds if any descendant (son, daughter, grandson, etc.) is in the list.
% Note: This requires a more complex check using the genealogical rules.
% For simplicity here, we assume 'son', 'daughter', 'grandson', 
% 'granddaughter' are the only descendant types.
has_descendant(HeirsList) :-
    ( member(son, HeirsList)
    ; member(daughter, HeirsList)
    ; member(grandson, HeirsList)
    ; member(granddaughter, HeirsList)
    ).

% has_male_descendant(HeirsList)
% Succeeds if a son or grandson (agnatic) is present.
has_male_descendant(HeirsList) :-
    ( member(son, HeirsList)
    ; member(grandson, HeirsList)
    ).
    
% has_female_descendant(HeirsList)
% Succeeds if a daughter or granddaughter is present.
has_female_descendant(HeirsList) :-
    ( member(daughter, HeirsList)
    ; member(granddaughter, HeirsList)
    ).

% --- END Helper Predicates for Hajb ---