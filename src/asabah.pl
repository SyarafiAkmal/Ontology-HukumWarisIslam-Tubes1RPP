/* separate_shares_asabah(ProvisionalShares, FixedSharePairs, AsabahList) */
/* Memisahkan warisan sementara menjadi warisan tetap dan asabah */
separate_shares_asabah([], [], []).

/* Tipe Asabah */
separate_shares_asabah([Type-Share|Rest], Fixed, [Type|AsabahRest]) :-
    Share == asabah,
    !,
    separate_shares_asabah(Rest, Fixed, AsabahRest).

/* Tipe Gabungan (combine) */
separate_shares_asabah([Type-Share|Rest], [Type-1/6|FixedRest], [Type|AsabahRest]) :-
    Share == combine,
    !,
    separate_shares_asabah(Rest, FixedRest, AsabahRest).

/* Tipe Warisan Tetap */
separate_shares_asabah([Type-Share|Rest], [Type-Share|FixedRest], Asabah) :-
    Share \== asabah,
    !,
    separate_shares_asabah(Rest, FixedRest, Asabah).

/* handle_asabah(Remainder, FixedSharePairs, AsabahList, FinalSharesList) */
handle_asabah(Remainder, FixedSharePairs, AsabahList, FinalSharesList) :-
    find_highest_priority_asabah(AsabahList, HighestGroup),
    
    count_asabah_shares(HighestGroup, TotalShares),
    
    findall(Type-AsabahShare,
            ( member(Type, HighestGroup),
              ( is_male_asabah(Type) -> Shares = 2 ; Shares = 1 ),
              frac_multiply(Remainder, Shares/TotalShares, AsabahShare)
            ),
            AsabahShares),
    
    merge_shares(FixedSharePairs, AsabahShares, FinalSharesList).

/* merge_shares(FixedList, AsabahList, MergedList) */
merge_shares([], AsabahList, AsabahList) :- !.  % No fixed shares left
merge_shares([Type-Fixed|RestFixed], AsabahList, [Type-Total|RestMerged]) :-
    ( member(Type-AsabahShare, AsabahList) ->
        frac_add(Fixed, AsabahShare, Total),
        select(Type-AsabahShare, AsabahList, RemainingAsabah)
    ;
        Total = Fixed,
        RemainingAsabah = AsabahList
    ),
    merge_shares(RestFixed, RemainingAsabah, RestMerged).

/* find_highest_priority_asabah(AsabahList, HighestGroup) */
find_highest_priority_asabah(AsabahList, HighestGroup) :-
    % Priority 1: Sons (and daughters with them)
    ( member(son, AsabahList) ->
        filter_asabah_type(AsabahList, [son, daughter], HighestGroup)
    
    % Priority 2: Grandsons (and granddaughters with them)
    ; member(grandson, AsabahList) ->
        filter_asabah_type(AsabahList, [grandson, granddaughter], HighestGroup)
    
    % Priority 3: Father (when no male descendants)
    ; member(father, AsabahList) ->
        filter_asabah_type(AsabahList, [father], HighestGroup)
    
    % Priority 4: Paternal Grandfather
    ; member(paternal_grandfather, AsabahList) ->
        filter_asabah_type(AsabahList, [paternal_grandfather], HighestGroup)
    
    % Priority 5: Full siblings
    ; (member(full_brother, AsabahList) ; member(full_sister, AsabahList)) ->
        filter_asabah_type(AsabahList, [full_brother, full_sister], HighestGroup)
    
    % Priority 6: Paternal siblings
    ; (member(paternal_brother, AsabahList) ; member(paternal_sister, AsabahList)) ->
        filter_asabah_type(AsabahList, [paternal_brother, paternal_sister], HighestGroup)
    
    % Priority 7: Paternal uncle and descendants
    ; member(paternal_uncle, AsabahList) ->
        filter_asabah_type(AsabahList, [paternal_uncle], HighestGroup)
    
    % Default: return all
    ; HighestGroup = AsabahList
    ).

/* filter_asabah_type(AsabahList, TypeList, FilteredGroup) */
filter_asabah_type(AsabahList, TypeList, FilteredGroup) :-
    findall(Type,
            (member(Type, AsabahList), member(Type, TypeList)),
            FilteredGroup).

/* count_asabah_shares(AsabahGroup, TotalShares) */
count_asabah_shares([], 0).

count_asabah_shares([Type|Rest], TotalShares) :-
    count_asabah_shares(Rest, RestShares),
    ( is_male_asabah(Type) 
    -> TotalShares is RestShares + 2
    ; TotalShares is RestShares + 1
    ).

/* is_male_asabah(Type) */
is_male_asabah(son).
is_male_asabah(grandson).
is_male_asabah(father).
is_male_asabah(paternal_grandfather).
is_male_asabah(full_brother).
is_male_asabah(paternal_brother).
is_male_asabah(paternal_uncle).
is_male_asabah(fathers_brother).
is_male_asabah(full_brothers_son).
is_male_asabah(paternal_brothers_son).
% NOTE: maternal_brother is NEVER Asabah - removed!

% Females get 1 share (implicit - anything not male)