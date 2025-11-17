% separate_shares_asabah(ProvisionalShares, FixedSharePairs, AsabahList)
% Separates shares into fixed fractions and Asabah (residuary) heirs.
separate_shares_asabah([], [], []).

separate_shares_asabah([Type-Share|Rest], Fixed, [Type|AsabahRest]) :-
    Share == asabah,
    !,
    separate_shares_asabah(Rest, Fixed, AsabahRest).

separate_shares_asabah([Type-Share|Rest], [Type-Share|FixedRest], Asabah) :-
    Share \== asabah,
    !,
    separate_shares_asabah(Rest, FixedRest, Asabah).

% handle_asabah(Remainder, FixedSharePairs, AsabahList, FinalSharesList)
handle_asabah(Remainder, FixedSharePairs, AsabahList, FinalSharesList) :-
    % 1. Find the highest priority Asabah group present
    find_highest_priority_asabah(AsabahList, HighestGroup),
    
    % 2. Count shares for this group (2 for male, 1 for female)
    count_asabah_shares(HighestGroup, TotalShares),
    
    % 3. Calculate distribution for each Asabah heir
    findall(Type-Share,
            ( member(Type, HighestGroup),
              ( is_male_asabah(Type) 
              -> MaleShares = 2
              ; MaleShares = 1
              ),
              frac_multiply(Remainder, MaleShares/TotalShares, Share)
            ),
            AsabahShares),

    % 4. Combine with the original fixed shares
    append(FixedSharePairs, AsabahShares, FinalSharesList).

% find_highest_priority_asabah(AsabahList, HighestGroup)
% Finds the highest priority Asabah group from the list.
% Priority order: son > grandson > full_brother > paternal_brother > 
%                 paternal_uncle > full_brothers_son, etc.

find_highest_priority_asabah(AsabahList, HighestGroup) :-
    % Check each priority level and return the first match
    ( filter_asabah_type(AsabahList, [son, daughter], Group), Group \= [] -> HighestGroup = Group
    ; filter_asabah_type(AsabahList, [grandson, granddaughter], Group), Group \= [] -> HighestGroup = Group
    ; filter_asabah_type(AsabahList, [full_brother, full_sister], Group), Group \= [] -> HighestGroup = Group
    ; filter_asabah_type(AsabahList, [paternal_brother, paternal_sister], Group), Group \= [] -> HighestGroup = Group
    ; filter_asabah_type(AsabahList, [paternal_uncle], Group), Group \= [] -> HighestGroup = Group
    ; HighestGroup = AsabahList % Default: return all if no specific group found
    ).

% filter_asabah_type(AsabahList, TypeList, FilteredGroup)
% Filters AsabahList to only include types in TypeList
filter_asabah_type(AsabahList, TypeList, FilteredGroup) :-
    findall(Type,
            (member(Type, AsabahList), member(Type, TypeList)),
            FilteredGroup).

% count_asabah_shares(AsabahGroup, TotalShares)
% Counts the total shares for Asabah distribution.
% Males get 2 shares, females get 1 share (2:1 ratio).

count_asabah_shares([], 0).

count_asabah_shares([Type|Rest], TotalShares) :-
    count_asabah_shares(Rest, RestShares),
    ( is_male_asabah(Type) 
    -> TotalShares is RestShares + 2
    ; TotalShares is RestShares + 1
    ).

% is_male_asabah(Type)
% Determines if an heir type is male (gets 2 shares in Asabah distribution)
is_male_asabah(son).
is_male_asabah(grandson).
is_male_asabah(full_brother).
is_male_asabah(paternal_brother).
is_male_asabah(maternal_brother).
is_male_asabah(paternal_uncle).
is_male_asabah(fathers_brother).
is_male_asabah(full_brothers_son).
is_male_asabah(paternal_brothers_son).

% Females get 1 share (implicit - anything not male)