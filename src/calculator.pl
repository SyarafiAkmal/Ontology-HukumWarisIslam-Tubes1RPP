% --- BEGIN Rational Arithmetic Library (fractions.pl) ---
% Required for GNU-Prolog.

% gcd(A, B, GCD)
% Calculates the Greatest Common Divisor of A and B.
gcd(A, 0, A) :-!.
gcd(A, B, G) :-
    R is A mod B,
    gcd(B, R, G).

% simplify_fraction(N/D, NumSimplified/DenSimplified)
% Simplifies a fraction to its lowest terms.
simplify_fraction(N/D, NS/DS) :-
    gcd(N, D, G),
    NS is N // G,
    DS is D // G.

% frac_add(Frac1, Frac2, Sum)
% Adds two fractions and simplifies the result.
frac_add(N1/D, N2/D, NS/D_Simp) :-!, % Same denominator
    N_Sum is N1 + N2,
    simplify_fraction(N_Sum/D, NS/D_Simp).
frac_add(N1/D1, N2/D2, NS/DS) :- % Different denominators
    NewN1 is N1 * D2,
    NewN2 is N2 * D1,
    NewD is D1 * D2,
    NewN is NewN1 + NewN2,
    simplify_fraction(NewN/NewD, NS/DS).

% frac_subtract(Frac1, Frac2, Difference)
% Subtracts Frac2 from Frac1.
frac_subtract(N1/D, N2/D, NS/D_Simp) :-!, % Same denominator
    N_Diff is N1 - N2,
    simplify_fraction(N_Diff/D, NS/D_Simp).
frac_subtract(N1/D1, N2/D2, NS/DS) :- % Different denominators
    NewN1 is N1 * D2,
    NewN2 is N2 * D1,
    NewD is D1 * D2,
    NewN is NewN1 - NewN2,
    simplify_fraction(NewN/NewD, NS/DS).
    
% frac_multiply(Frac1, Frac2, Product)
% Multiplies two fractions.
frac_multiply(N1/D1, N2/D2, NS/DS) :-
    NewN is N1 * N2,
    NewD is D1 * D2,
    simplify_fraction(NewN/NewD, NS/DS).

% frac_sum_list(ListOfFractions, TotalSum)
% Recursively sums a list of fractions.
frac_sum_list([], 0/1). % Sum of empty list is 0
frac_sum_list([H|T], TotalSum) :-
    frac_sum_list(T, RestSum),
    frac_add(H, RestSum, TotalSum).

% frac_compare(Frac1, Op, Frac2)
% Compares two fractions (Op is '>', '<', '>=', '=<', '=:=', '=\=')
frac_compare(N1/D1, Op, N2/D2) :-
    % Convert to common denominator to compare numerators
    Num1 is N1 * D2,
    Num2 is N2 * D1,
    % Construct the goal to call (e.g., 6 > 4)
    Goal =.. [Op, Num1, Num2],
    call(Goal).

% frac_divide(Frac1, Frac2, Result)
% Divides Frac1 by Frac2 (Frac1 / Frac2)
frac_divide(N1/D1, N2/D2, Result) :-
    % Division: (N1/D1) / (N2/D2) = (N1/D1) * (D2/N2)
    frac_multiply(N1/D1, D2/N2, Result).

% --- END Rational Arithmetic Library ---