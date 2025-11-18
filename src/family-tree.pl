:- dynamic(person/4).
:- dynamic(parent_of/2).
:- dynamic(married/2).
:- discontiguous(person/4).
:- discontiguous(parent_of/2).
:- discontiguous(married/2).

% --- Persons ---
person(p1, jupri, male, false).
person(p2, siti, female, true).
person(p3, paijo, male, false).
person(p4, sri, female, true).
person(p5, sukirman, male, true).
person(p6, lasmi, female, true).
person(p7, sumantri, male, false).
person(p8, paimin, male, true).
person(p9, sulastri, female, true).
person(p10, sumarni, female, true).

% --- Marriages ---
married(p1, p2).
married(p5, p6).
married(p3, p4).

% --- Parent-Child Relationships ---
parent_of(p3, p1).
parent_of(p4, p5).
parent_of(p4, p6).
parent_of(p7, p3).
parent_of(p8, p7).
parent_of(p9, p3).
parent_of(p10, p3).
parent_of(p3, p2).
parent_of(p7, p4).
parent_of(p9, p4).
parent_of(p10, p4).
