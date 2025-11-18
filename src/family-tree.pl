:- dynamic(person/4).
:- dynamic(parent_of/2).
:- dynamic(married/2).
:- discontiguous(person/4).
:- discontiguous(parent_of/2).
:- discontiguous(married/2).

% --- Persons ---
person(p1, ali, male, false).
person(p2, amina, female, false).
person(p3, umar, male, false).
person(p4, khadija, female, true).
person(p5, zayd, male, false).
person(p6, ayesha, female, true).
person(p7, ibrahim, male, false).
person(p8, zaynab, female, false).
person(p9, bagas, male, false).
person(p10, maryam, female, true).
person(p11, mega, female, false).
person(p12, musab, male, false).
person(p13, aisha, female, true).
person(p14, yusuf, male, false).
person(p15, fatima, female, false).
person(p16, puan, female, false).
person(p17, usman, male, false).

% --- Marriages ---
married(p1, p2).
married(p3, p4).
married(p5, p6).
married(p7, p8).
married(p12, p13).

% --- Parent-Child Relationships ---
parent_of(p3, p1).
parent_of(p5, p3).
parent_of(p7, p5).
parent_of(p9, p7).
parent_of(p10, p5).
parent_of(p11, p5).
parent_of(p12, p5).
parent_of(p14, p12).
parent_of(p15, p3).
parent_of(p16, p3).
parent_of(p17, p1).
parent_of(p3, p2).
parent_of(p5, p4).
parent_of(p7, p6).
parent_of(p9, p8).
parent_of(p10, p6).
parent_of(p11, p6).
parent_of(p12, p6).
parent_of(p14, p13).
parent_of(p15, p4).
parent_of(p16, p4).
parent_of(p17, p2).
