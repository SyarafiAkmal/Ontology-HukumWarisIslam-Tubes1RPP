/* ====== HELPER ====== */
sibling(Origin, Target, Gender, Lineage):-
    person(Target, Gender, _),
    parent_child(Father, Origin, Lineage),
    parent_child(Father, Target, Lineage),
    parent_child(Mother, Origin, Lineage),
    parent_child(Mother, Target, Lineage).

paternal_uncle(Uncle, Niece, Lineage):-
    person(Uncle, male, _),
    parent_child(Father, Niece, _),
    sibling(Father, Uncle, male, Lineage).

full_brother(Origin, Target):-
    person(Target, male, alive),
    sibling(Origin, Target, male, both).

% calculate_full_sister_share:-

% calculate_full_sister_share:-

allow_collaterals(Deceased):-
    \+(
        parent_child(Deceased, Son),
        person(Son, male, alive)  
    ),
    \+(
        parent_child(Deceased, Son),
        parent_child(Son, Grandson),
        person(Grandson, male, alive) 
    ),
    \+(
        parent_child(Father, Deceased),
        person(Father, male, alive)  
    ),
    \+(
        parent_child(Father, Deceased),
        parent_child(Grandfather, Father),
        person(Grandfather, male, alive) 
    ).

/* ====== FOOTNOTE RULES ====== */

%% FULL BROTHER FAMILY

%% Full Brother
inherit_from(Deceased, FullBrother, Share):-
    allow_collaterals(Deceased),
    full_brother(Deceased, FullBrother),
    Share = 'T',
    !.

%% Full Brother's Son
inherit_from(Deceased, FullBrotherSon, Share):-
    allow_collaterals(Deceased),
    person(FullBrotherSon, male, alive),
    parent_child(Blocker, FullBrotherSon, _),
    full_brother(Deceased, Blocker),
    !.

%% Paternal Brother
inherit_from(Deceased, PaternalBrother, Share):-
    allow_collaterals(Deceased),
    person(PaternalBrother, male, alive),
    sibling(Deceased, PaternalBrother, male, paternal),
    Share = 'T',
    !.

%% Paternal Brother's Son
inherit_from(Deceased, PaternalBrotherSon, Share):-
    allow_collaterals(Deceased),
    person(PaternalBrotherSon, male, alive),
    parent_child(Blocker, PaternalBrotherSon, _),
    sibling(Deceased, Blocker, male, paternal),
    Share='T',
    !.

%% Full Uncle Paternal
inherit_from(Deceased, FullUnclePaternal, Share):-
    allow_collaterals(Deceased),
    person(FullUnclePaternal, male, alive),
    paternal_uncle(FullUnclePaternal, Deceased, both),
    Share='T',
    !.

%% Father's Paternal Brother
inherit_from(Deceased, FatherPaternalBrother, Share):-
    allow_collaterals(Deceased),
    person(FatherPaternalBrother, male, alive),
    paternal_uncle(FatherPaternalBrother, Deceased, paternal),
    Share='T',
    !.

%% Full Paternal Uncle's Son
inherit_from(Deceased, FullPaternalUncleSon, Share):-
    allow_collaterals(Deceased),
    person(FullPaternalUncleSon, male, alive),
    parent_child(Blocker, FullPaternalUncleSon, _),
    paternal_uncle(Blocker, Deceased, both),
    Share='T',
    !.

%% Father's Paternal Brother's Son
inherit_from(Deceased, FullPaternalUncleSon, Share):-
    allow_collaterals(Deceased),
    person(FullPaternalUncleSon, male, alive),
    parent_child(Blocker, FullPaternalUncleSon, _),
    paternal_uncle(Blocker, Deceased, paternal),
    Share='T',
    !.

%% FULL SISTER FAMILY
%% Full Sister
inherit_from(Deceased, FullSister, Share):-
    allow_collaterals(Deceased),
    person(FullSister, female, alive),
    sibling(Deceased, FullSister, female, both),
    Share = 'T',
    !.

%% Paternal Sister

%% MATERNAL SIBLINGS