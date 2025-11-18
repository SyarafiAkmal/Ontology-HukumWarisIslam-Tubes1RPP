% ['parser.pl'].
% parse_family_tree('../input/akmal.json').
% save_family_tree('family-tree.pl').
:- ['calculator.pl'].
:- ['hajb-helper.pl'].
:- ['hajb-hirman.pl'].
:- ['hajb-nuqsan.pl'].
:- ['asabah.pl'].
:- ['kinship.pl'].
:- ['family-tree.pl'].

/* Titik awal query */
calculate_inheritance(DeceasedName, HeirName, FinalShareString) :-
    person(DeceasedID, DeceasedName, _, false),
    person(HeirID, HeirName, _, true),
    
    findall(Type, 
            living_relative_type(DeceasedID, Type), 
            HeirsList),
    write('Heirs List: '), write(HeirsList), nl,
            
    living_relative_type(DeceasedID, HeirID, HeirType),
    
    calculate_shares(HeirType, DeceasedID, HeirsList, FinalShareFraction),
    
    format_fraction(FinalShareFraction, FinalShareString).

calculate_inheritance(_, HeirName, '0/1') :-
    write(HeirName), write(' is not an eligible heir or is fully excluded.'), nl.

/* Proses penghitungan utama */
calculate_shares(HeirType, DeceasedID, HeirsList, FinalShare) :-
    /* Periksa apakah excluded */
    is_excluded(HeirType, DeceasedID, HeirsList),!,
    FinalShare = 0/1.
    
calculate_shares(HeirType, DeceasedID, HeirsList, FinalShare) :-
    findall(Type-Share,
            ( member(Type, HeirsList),
              \+ is_excluded(Type, DeceasedID, HeirsList),
              get_provisional_share(Type, HeirsList, Share)
            ),
            ProvisionalShares),
    write('Provisional Shares: '), write(ProvisionalShares), nl,

    separate_shares_asabah(ProvisionalShares, FixedSharePairs, AsabahList),

    findall(Share, member(_-Share, FixedSharePairs), ShareValues),
    frac_sum_list(ShareValues, TotalFixedShare),
    write('Total Fixed Share: '), write(TotalFixedShare), nl,
    
    frac_subtract(1/1, TotalFixedShare, Remainder),
    write('Remainder: '), write(Remainder), nl,

    distribute_remainder(Remainder, FixedSharePairs, AsabahList, TotalFixedShare, FinalShareList),

    ( member(HeirType-FinalShare, FinalShareList)
    ; FinalShare = 0/1
    ).

/* Distribusi sisa warisan */
/* Case 1: Remainder > 0 dan terdapat asabah */
distribute_remainder(Rem, Fixed, Asabah, _, Final) :-
    frac_compare(Rem, '>', 0/1), \+ Asabah = [],!,
    handle_asabah(Rem, Fixed, Asabah, Final).
    
/* Case 2: Remainder > 0 dan tidak ada asabah */
distribute_remainder(Rem, Fixed, _, _, Final) :-
    frac_compare(Rem, '>', 0/1),!,                 
    handle_radd(Rem, Fixed, Final).
    
/* Case 3: Remainder < 0 (Defisit) */
distribute_remainder(Rem, Fixed, _, TotalFixedShare, Final) :-
    frac_compare(Rem, '<', 0/1),!,                 
    handle_aul(Fixed, TotalFixedShare, Final). 
    
/* Case 4: Remainder = 0 */
distribute_remainder(0/1, Fixed, _, _, Fixed) :-!.  

/* Utility: format_fraction(N/D, String) */
format_fraction(N/D, String) :-
    number_chars(N, NChars),
    number_chars(D, DChars),
    append(NChars, ['/'], Temp),
    append(Temp, DChars, ResultChars),
    atom_chars(String, ResultChars).

/* handle_aul(FixedSharePairs, TotalFixedShare, AdjustedSharesList) */
handle_aul(FixedSharePairs, SumN/SumD, AdjustedSharesList) :-
    NewDenominator is SumN,
    
    findall(Type-AdjShare,
            ( member(Type-N/D, FixedSharePairs),
              NumCommon is N * (SumD / D), 
              simplify_fraction(NumCommon/NewDenominator, AdjShare)
            ),
            AdjustedSharesList).

/*  Utility: partition/4 */
partition(_, [], [], []).
partition(Pred, [H|T], [H|Sat], NotSat) :-
    call(Pred, H), !,
    partition(Pred, T, Sat, NotSat).
partition(Pred, [H|T], Sat, [H|NotSat]) :-
    partition(Pred, T, Sat, NotSat).

/* handle_radd(Remainder, FixedSharePairs, AdjustedSharesList) */
handle_radd(Remainder, FixedSharePairs, AdjustedSharesList) :-
    partition(is_spouse, FixedSharePairs, SpouseShares, RaddHeirs),
    
    ( RaddHeirs = [] ->
        AdjustedSharesList = FixedSharePairs
    ;
        findall(Share, member(_-Share, RaddHeirs), RaddShareValues),
        frac_sum_list(RaddShareValues, TotalRaddShare),
        
        findall(Type-NewShare,
                ( member(Type-OldShare, RaddHeirs),
                  frac_divide(OldShare, TotalRaddShare, Proportion),
                  frac_multiply(Remainder, Proportion, RaddAmount),
                  frac_add(OldShare, RaddAmount, NewShare)
                ),
                AdjustedRaddShares),
        
        append(SpouseShares, AdjustedRaddShares, AdjustedSharesList)
    ).
is_spouse(wife-_).
is_spouse(husband-_).