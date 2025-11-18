/* Relasi dasar */
father(Child, Father) :-
    parent_of(Child, Father),
    person(Father, _, male, _).

mother(Child, Mother) :-
    parent_of(Child, Mother),
    person(Mother, _, female, _).

son(Parent, Son) :-
    parent_of(Son, Parent),
    person(Son, _, male, _).

daughter(Parent, Daughter) :-
    parent_of(Daughter, Parent),
    person(Daughter, _, female, _).

husband(Wife, Husband) :-
    married(Husband, Wife).

wife(Husband, Wife) :-
    married(Husband, Wife).

/* Periksa apakah kerabat masih hidup */
is_living_relative(Person) :-
    person(Person, _, _, true).

full_sibling_of(P1, P2) :-
    father(P1, F), father(P2, F),
    mother(P1, M), mother(P2, M),
    P1 \== P2.

paternal_sibling_of(P1, P2) :-
    father(P1, F), father(P2, F),
    mother(P1, M1), mother(P2, M2),
    M1 \== M2,
    P1 \== P2.

maternal_sibling_of(P1, P2) :-
    mother(P1, M), mother(P2, M),
    father(P1, F1), father(P2, F2),
    F1 \== F2,
    P1 \== P2.

full_uncle_paternal(Uncle, NieceNephew) :-
    (full_sibling_of(Uncle, Parent),
    person(Parent, _, male, _),
    parent_of(Parent, NieceNephew)
    ;
    husband(Aunt, Uncle),
    full_sibling_of(Parent, Aunt),
    parent_of(Parent, NieceNephew)
    ).

ancestor_of(Anc, Desc) :-
    parent_of(Desc, Anc).

ancestor_of(Anc, Desc) :-
    parent_of(Desc, Parent),
    ancestor_of(Anc, Parent).

descendant_of(Desc, Anc) :-
    ancestor_of(Anc, Desc).
    
son_descendant(Desc, Anc) :-
    son(Anc, Desc).
son_descendant(Desc, Anc) :-
    son(Anc, S),
    son_descendant(Desc, S).

/* living_relative_type/3 - identifikasi tipe relasi kerabat yang masih hidup */
living_relative_type(DeceasedID, HeirID, wife) :-
    person(HeirID, _, female, true),
    (married(DeceasedID, HeirID) ; married(HeirID, DeceasedID)).

living_relative_type(DeceasedID, HeirID, husband) :-
    person(HeirID, _, male, true),
    (married(DeceasedID, HeirID) ; married(HeirID, DeceasedID)).

living_relative_type(DeceasedID, HeirID, father) :-
    person(HeirID, _, male, true),
    parent_of(DeceasedID, HeirID).

living_relative_type(DeceasedID, HeirID, mother) :-
    person(HeirID, _, female, true),
    parent_of(DeceasedID, HeirID).

living_relative_type(DeceasedID, HeirID, son) :-
    person(HeirID, _, male, true),
    parent_of(HeirID, DeceasedID).

living_relative_type(DeceasedID, HeirID, daughter) :-
    person(HeirID, _, female, true),
    parent_of(HeirID, DeceasedID).

living_relative_type(DeceasedID, HeirID, grandfather_paternal) :-
    person(HeirID, _, male, true),
    parent_of(DeceasedID, ParentID),
    person(ParentID, _, male, false), 
    parent_of(ParentID, HeirID).

living_relative_type(DeceasedID, HeirID, grandmother_maternal) :-
    person(HeirID, _, female, true),
    parent_of(DeceasedID, ParentID),
    person(ParentID, _, female, false),
    parent_of(ParentID, HeirID).

living_relative_type(DeceasedID, HeirID, grandmother_paternal) :-
    person(HeirID, _, female, true),
    parent_of(DeceasedID, ParentID),
    person(ParentID, _, male, false), 
    parent_of(ParentID, HeirID).

living_relative_type(DeceasedID, HeirID, grandson_son) :-
    person(HeirID, _, male, true),
    parent_of(HeirID, ParentID),
    person(ParentID, _, male, false),
    parent_of(ParentID, DeceasedID),
    person(ParentID, _, male, false).

living_relative_type(DeceasedID, HeirID, granddaughter_son) :-
    person(HeirID, _, female, true),
    parent_of(HeirID, ParentID),
    person(ParentID, _, male, false),
    parent_of(ParentID, DeceasedID),
    person(ParentID, _, male, false).

living_relative_type(DeceasedID, HeirID, full_brother) :-
    person(HeirID, _, male, true),
    full_sibling(DeceasedID, HeirID).

living_relative_type(DeceasedID, HeirID, full_sister) :-
    person(HeirID, _, female, true),
    full_sibling(DeceasedID, HeirID).

living_relative_type(DeceasedID, HeirID, maternal_sister) :-
    person(HeirID, _, female, true),
    maternal_sibling(DeceasedID, HeirID).

living_relative_type(DeceasedID, HeirID, maternal_brother) :-
    person(HeirID, _, male, true),
    maternal_sibling(DeceasedID, HeirID).

living_relative_type(DeceasedID, HeirID, full_brothers_son) :-
    person(HeirID, _, male, true),
    full_sibling(DeceasedID, SiblingID),
    parent_of(HeirID, SiblingID).

living_relative_type(DeceasedID, HeirID, paternal_sister) :-
    person(HeirID, _, female, true),
    paternal_sibling(DeceasedID, HeirID).

living_relative_type(DeceasedID, HeirID, paternal_brother) :-
    person(HeirID, _, male, true),
    paternal_sibling(DeceasedID, HeirID).

living_relative_type(DeceasedID, HeirID, paternal_brothers_son) :-
    person(HeirID, _, male, true),
    paternal_sibling(DeceasedID, SiblingID),
    parent_of(HeirID, SiblingID).

living_relative_type(DeceasedID, HeirID, full_uncle_paternal) :-
    person(HeirID, _, male, true),
    full_uncle_paternal(HeirID, DeceasedID).

living_relative_type(DeceasedID, HeirID, fathers_paternal_brother) :-
    person(HeirID, _, male, true),
    parent_of(DeceasedID, FatherID),
    paternal_sibling(FatherID, HeirID).

living_relative_type(DeceasedID, HeirID, full_paternal_uncles_son) :-
    person(HeirID, _, male, true),
    full_uncle_paternal(UncleID, DeceasedID),
    parent_of(HeirID, UncleID).

living_relative_type(DeceasedID, HeirID, fathers_paternal_brothers_son) :-
    person(HeirID, _, male, true),
    parent_of(DeceasedID, FatherID),
    paternal_sibling(FatherID, UncleID),
    parent_of(HeirID, UncleID).

full_sibling(Person1, Person2) :-
    Person1 \= Person2,
    parent_of(Person1, Father),
    parent_of(Person2, Father),
    person(Father, _, male, _),
    parent_of(Person1, Mother),
    parent_of(Person2, Mother),
    person(Mother, _, female, _).

paternal_sibling(Person1, Person2) :-
    Person1 \= Person2,
    parent_of(Person1, Father),
    parent_of(Person2, Father),
    person(Father, _, male, _),
    \+ full_sibling(Person1, Person2).

maternal_sibling(Person1, Person2) :-
    Person1 \= Person2,
    parent_of(Person1, Mother),
    parent_of(Person2, Mother),
    person(Mother, _, female, _),
    \+ full_sibling(Person1, Person2).

/* living_relative_type/2 - mencari seluruh kerabat yang masih hidup dan tipe mereka */
/* Digunakan dengan findall */
living_relative_type(DeceasedID, Type) :-
    person(HeirID, _, _, true),
    living_relative_type(DeceasedID, HeirID, Type).