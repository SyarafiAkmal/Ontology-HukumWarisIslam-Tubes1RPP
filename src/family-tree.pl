:- dynamic(person/4).
:- dynamic(parent_of/2).
:- dynamic(married/2).
:- discontiguous(person/4).
:- discontiguous(parent_of/2).
:- discontiguous(married/2).

% --- GENERATION 1 (The Ancestors) ---

% Paternal Grandparents
person(p1, ali, male, false).
person(p2, amina, female, false).
married(p1, p2).

% Maternal Grandparents
person(p3, bakr, male, false).
person(p4, basma, female, false).
married(p3, p4).

% --- GENERATION 2 (Parents, Aunts, Uncles) ---

% --- Paternal Side ---
% Father of the Deceased
person(p5, umar, male, false).
parent_of(p5, p1). % Son of Ali
parent_of(p5, p2). % Son of Amina

% Paternal Uncle (Fathers Full Brother)
person(p6, usman, male, false).
parent_of(p6, p1). % Son of Ali
parent_of(p6, p2). % Son of Amina

% --- Maternal Side ---
% Mother of the Deceased
person(p7, khadija, female, true).
parent_of(p7, p3). % Daughter of Bakr
parent_of(p7, p4). % Daughter of Basma

% Maternal Uncle (Mothers Full Brother)
person(p8, karim, male, false).
parent_of(p8, p3). % Son of Bakr
parent_of(p8, p4). % Son of Basma

% --- Marriages of Generation 2 ---
% Marriage of Father and Mother
married(p5, p7).

% Fathers Second Wife (for Paternal Siblings)
person(p11, safiya, female, false).
married(p5, p11).

% Mothers Second Husband (for Maternal Siblings)
person(p13, bilal, male, false).
married(p13, p7).


% --- GENERATION 3 (Deceased, Spouse, Siblings) ---

% --- The Deceased Person (Primary Test Case) ---
% Zayd died, leaving parents, a wife, children, and a grandchild.
person(p9, zayd, male, false). % Husband

% --- Zayds Spouse ---
person(p15, ayesha, female, false). % DECEASED
parent_of(p15, p5).  % Daughter of Umar
parent_of(p15, p7).  % Daughter of Khadija
married(p9, p15).

% --- Zayds Siblings ---

% Full Sister (Shares Father and Mother)
person(p10, fatima, female, false).
parent_of(p10, p5). % Daughter of Umar
parent_of(p10, p7). % Daughter of Khadija

person(p22, puan, female, false).
parent_of(p22, p5). % Daughter of Umar
parent_of(p22, p7). % Daughter of Khadija

% Paternal Brother (Consanguine - Shares Father only)
person(p12, hamza, male, false).
parent_of(p12, p5).  % Son of Umar
parent_of(p12, p11). % Son of Safiya (Fathers 2nd wife)

% Maternal Sister (Uterine - Shares Mother only)
person(p14, sumayya, female, false).
parent_of(p14, p13). % Daughter of Bilal (Mothers 2nd husband)
parent_of(p14, p7).  % Daughter of Khadija


% --- GENERATION 4 (Children of the Deceased) ---

% Deceased Son (to allow Grandson to inherit)
person(p16, ibrahim, male, false).
parent_of(p16, p9).  % Son of Zayd
parent_of(p16, p15). % Son of Ayesha

% Deceased Daughter
person(p17, maryam, female, true).
parent_of(p17, p9).  % Daughter of Zayd
parent_of(p17, p15). % Daughter of Ayesha

person(p21, mega, female, false).
parent_of(p21, p9).  % Daughter of Zayd
parent_of(p21, p15). % Daughter of Ayesha

% Another Deceased Son (for testing multiple children)
person(p20, musab, male, false). % DEAD
parent_of(p20, p9).  % Son of Zayd
parent_of(p20, p15). % Son of Ayesha

% Wife of the Deceased Son (Ibrahim)
person(p18, zaynab, female, false).
married(p16, p18).

% --- GENERATION 5 (Grandchildren of the Deceased) ---

% Sons Son (Grandson)
person(p19, yusuf, male, false).
parent_of(p19, p20). % Son of Musab
parent_of(p19, p18). % Son of Zaynab