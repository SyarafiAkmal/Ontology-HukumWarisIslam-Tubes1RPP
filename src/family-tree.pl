:- dynamic(person/4).
:- dynamic(parent_of/2).
:- dynamic(married/2).
:- discontiguous(person/4).
:- discontiguous(parent_of/2).
:- discontiguous(married/2).

% --- Persons ---
person(p1, betta, male, false).
person(p2, rina, female, true).
person(p3, aldi, male, false).
person(p4, wati, female, false).
person(p5, devi, female, true).
person(p6, lia, female, true).

% --- Marriages ---
married(p1, p2).
married(p3, p4).

% --- Parent-Child Relationships ---
parent_of(p3, p1).
parent_of(p5, p3).
parent_of(p6, p3).
parent_of(p3, p2).
parent_of(p5, p4).
parent_of(p6, p4).
