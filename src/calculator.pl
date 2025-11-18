/* gcd(A, B, GCD) */
gcd(A, 0, A) :-!.
gcd(A, B, G) :-
    R is A mod B,
    gcd(B, R, G).

lcm(A, B, LCM) :-
    gcd(A, B, G),
    LCM is (A * B) // G.

/* simplify_fraction(N/D, NumSimplified/DenSimplified) */
simplify_fraction(N/D, NS/DS) :-
    gcd(N, D, G),
    NS is N // G,
    DS is D // G.

/* frac_add(Frac1, Frac2, Sum) */
frac_add(N1/D, N2/D, NS/D) :-!, % Penyebut sama
    NS is N1 + N2.
frac_add(N1/D1, N2/D2, NS/DS) :- % Penyebut beda
    DS is lcm(D1, D2),
    NewN1 is N1 * (DS / D1),
    NewN2 is N2 * (DS / D2),
    NS is NewN1 + NewN2.

/* frac_subtract(Frac1, Frac2, Difference) */
frac_subtract(N1/D, N2/D, NS/D_Simp) :-!, % Penyebut sama
    N_Diff is N1 - N2,
    simplify_fraction(N_Diff/D, NS/D_Simp).
frac_subtract(N1/D1, N2/D2, NS/DS) :- % Penyebut beda
    NewN1 is N1 * D2,
    NewN2 is N2 * D1,
    NewD is D1 * D2,
    NewN is NewN1 - NewN2,
    simplify_fraction(NewN/NewD, NS/DS).
    
/* frac_multiply(Frac1, Frac2, Product) */
frac_multiply(N1/D1, N2/D2, NS/DS) :-
    NewN is N1 * N2,
    NewD is D1 * D2,
    simplify_fraction(NewN/NewD, NS/DS).

/* frac_sum_list(ListOfFractions, TotalSum) */
frac_sum_list([], 0/1). % Sum of empty list is 0
frac_sum_list([H|T], TotalSum) :-
    frac_sum_list(T, RestSum),
    frac_add(H, RestSum, TotalSum).

/* frac_compare(Frac1, Op, Frac2) */
frac_compare(N1/D1, Op, N2/D2) :-
    % Convert to common denominator to compare numerators
    Num1 is N1 * D2,
    Num2 is N2 * D1,
    % Construct the goal to call (e.g., 6 > 4)
    Goal =.. [Op, Num1, Num2],
    call(Goal).

/* frac_divide(Frac1, Frac2, Result) */
frac_divide(N1/D1, N2/D2, Result) :-
    % Division: (N1/D1) / (N2/D2) = (N1/D1) * (D2/N2)
    frac_multiply(N1/D1, D2/N2, Result).