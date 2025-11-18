% --- BEGIN Top-Level Predicates ---

:- initialization(load_files).

load_files :-
    consult('family-tree.pl'),
    consult('calculator.pl'),
    consult('hajb-helper.pl'),
    consult('hajb-hirman.pl'),
    consult('hajb-nuqsan.pl'),
    consult('asabah.pl'),
    consult('kinship.pl').

% Main entry point for user query
calculate_inheritance(DeceasedName, HeirName, FinalShareString) :-
    person(DeceasedID, DeceasedName, _, false), % Find deceased person
    person(HeirID, HeirName, _, true),       % Find living heir
    
    % 1. Gather all living relatives and their types
    findall(Type, 
            living_relative_type(DeceasedID, Type), 
            HeirsList),
    write('Heirs List: '), write(HeirsList), nl,
            
    % 2. Get the specific type of the heir being queried
    living_relative_type(DeceasedID, HeirID, HeirType),
    
    % 3. Run the main calculation engine
    calculate_shares(HeirType, DeceasedID, HeirsList, FinalShareFraction),
    
    % 4. Format the output string
    format_fraction(FinalShareFraction, FinalShareString).

calculate_inheritance(_, HeirName, '0/1') :-
    write(HeirName), write(' is not an eligible heir or is fully excluded.'), nl.

% The "Engine" Predicate
calculate_shares(HeirType, DeceasedID, HeirsList, FinalShare) :-
    % Check for Hajb al-Hirman first
    is_excluded(HeirType, DeceasedID, HeirsList),!,
    FinalShare = 0/1.
    
calculate_shares(HeirType, DeceasedID, HeirsList, FinalShare) :-
    % 1. Get all provisional shares for all non-excluded heirs
    findall(Type-Share,
            ( member(Type, HeirsList),
              \+ is_excluded(Type, DeceasedID, HeirsList),
              get_provisional_share(Type, HeirsList, Share)
            ),
            ProvisionalShares),
    write('Provisional Shares: '), write(ProvisionalShares), nl,

    % 2. Separate fixed shares from residuaries (Asabah)
    separate_shares_asabah(ProvisionalShares, FixedSharePairs, AsabahList),

    % 3. Sum the fixed shares
    findall(Share, member(_-Share, FixedSharePairs), ShareValues),
    frac_sum_list(ShareValues, TotalFixedShare),
    write('Total Fixed Share: '), write(TotalFixedShare), nl,
    
    % 4. Calculate Remainder
    frac_subtract(1/1, TotalFixedShare, Remainder),
    write('Remainder: '), write(Remainder), nl,

    % 5. Distribute Remainder (handles Aul, Radd, and Asabah)
    distribute_remainder(Remainder, FixedSharePairs, AsabahList, TotalFixedShare, FinalShareList),

    % 6. Look up the queried heirs final share
    ( member(HeirType-FinalShare, FinalShareList)
    ; FinalShare = 0/1 % Heir was not in the final list (e.g., fixed sharer in Asabah-only case)
    ).

% This is the 3-way logical branch that controls the calculation
distribute_remainder(Rem, Fixed, Asabah, _, Final) :-
    frac_compare(Rem, '>', 0/1), \+ Asabah = [],!,
    handle_asabah(Rem, Fixed, Asabah, Final).
    
distribute_remainder(Rem, Fixed, _, _, Final) :-
    frac_compare(Rem, '>', 0/1),!,                 % Case 2: Remainder > 0, NO Asabah
    handle_radd(Rem, Fixed, Final).
    
distribute_remainder(Rem, Fixed, _, TotalFixedShare, Final) :-
    frac_compare(Rem, '<', 0/1),!,                 % Case 3: Remainder < 0 (Deficit)
    handle_aul(Fixed, TotalFixedShare, Final). % Need TotalFixedShare here
    
distribute_remainder(0/1, Fixed, _, _, Fixed) :-!.    % Case 4: Remainder is exactly 0

% (Helper: separate_shares_asabah/3 must be implemented to parse
%  the ProvisionalShares list, splitting 'asabah' and '1/6 + asabah'
%  terms from simple fractions.)

% (Helper: living_relative_type/3 must map IDs to types, e.g.
%  living_relative_type(p1, p2, wife) :- wife(p1, p2), is_living_relative(p2).)

% (Helper: format_fraction/2 converts N/D term to "N/D" string)
format_fraction(N/D, String) :-
    number_chars(N, NChars),
    number_chars(D, DChars),
    append(NChars, ['/'], Temp),
    append(Temp, DChars, ResultChars),
    atom_chars(String, ResultChars).

% handle_aul(FixedSharePairs, TotalFixedShare, AdjustedSharesList)
% TotalFixedShare is the sum, e.g., 13/12.
handle_aul(FixedSharePairs, SumN/SumD, AdjustedSharesList) :-
    % The new denominator is the Numerator of the sum.
    NewDenominator is SumN,
    
    % We must re-calculate each share against the new base.
    % Share_adj = (Share * OldDenominator) / NewDenominator
    % (Share * OldDenominator) is just the numerator of the share
    % when expressed with the common denominator.
    % This requires a helper to find the common denominator of all shares.
    % For simplicity, we can use the denominator of the sum (SumD).
    
    findall(Type-AdjShare,
            ( member(Type-N/D, FixedSharePairs),
              % Convert N/D to common denominator SumD
              NumCommon is N * (SumD / D), 
              % New share is NumCommon / NewDenominator
              simplify_fraction(NumCommon/NewDenominator, AdjShare)
            ),
            AdjustedSharesList).

% Utility: partition/4
partition(_, [], [], []).
partition(Pred, [H|T], [H|Sat], NotSat) :-
    call(Pred, H), !,
    partition(Pred, T, Sat, NotSat).
partition(Pred, [H|T], Sat, [H|NotSat]) :-
    partition(Pred, T, Sat, NotSat).

% handle_radd(Remainder, FixedSharePairs, AdjustedSharesList)
% handle_radd(Remainder, FixedSharePairs, AdjustedSharesList)
handle_radd(Remainder, FixedSharePairs, AdjustedSharesList) :-
    % 1. Separate spouses from other Radd-eligible heirs
    partition(is_spouse, FixedSharePairs, SpouseShares, RaddHeirs),
    
    % 2. Check if there are Radd-eligible heirs
    ( RaddHeirs = [] ->
        AdjustedSharesList = FixedSharePairs
    ;
        % 3. Calculate the total share of Radd-eligible heirs
        findall(Share, member(_-Share, RaddHeirs), RaddShareValues),
        frac_sum_list(RaddShareValues, TotalRaddShare),
        
        % 4. Calculate adjusted shares for Radd-eligible heirs
        % NewShare = OldShare + (Remainder * (OldShare / TotalRaddShare))
        findall(Type-NewShare,
                ( member(Type-OldShare, RaddHeirs),
                  frac_divide(OldShare, TotalRaddShare, Proportion),
                  frac_multiply(Remainder, Proportion, RaddAmount),
                  frac_add(OldShare, RaddAmount, NewShare)
                ),
                AdjustedRaddShares),
        
        % 5. Re-combine lists
        append(SpouseShares, AdjustedRaddShares, AdjustedSharesList)
    ).
is_spouse(wife-_).
is_spouse(husband-_).

% --- END Top-Level Predicates ---