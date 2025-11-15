:- use_module(library(http/json)).

:- dynamic person/3.
:- dynamic spouse/2.
:- dynamic parent_child/3.


%% parse_family_tree(+File)
parse_family_tree(File) :-
    retractall(person(_, _)),
    retractall(spouse(_, _)),
    retractall(parent_child(_, _, _)),
    open(File, read, In),
    json_read_dict(In, Root),
    close(In),
    process_family_member(Root, none),
    complete_parent_child().

%% process_family_member(+Dict, +ParentNameOrNone)
process_family_member(Node, Parent) :-
    get_name(Node, Name),
    get_gender(Node, Gender),
    get_status(Node, Status),

    % assert person(Name, Gender) once
    ( person(Name, _, _) -> true ; assertz(person(Name, Gender, Status)) ),

    % assert parent-child if called with a parent
    ( Parent \= none,
      get_dict(lineage, Node, RawLineage)
    -> normalize_lineage(RawLineage, Lin),
       assertz(parent_child(Parent, Name, Lin))
    ;  true
    ),

    % spouse
    ( get_dict(spouse, Node, SNode)
    -> get_name(SNode, SName),
       get_gender(SNode, SGender),
       get_status(SNode, SStatus),
       ( person(SName, _, _) -> true ; assertz(person(SName, SGender, SStatus)) ),
       ( spouse(Name, SName) -> true ; assertz(spouse(Name, SName)) )
    ; true
    ),

    % recursively process descendents
    ( get_dict(descendents, Node, DescList)
    -> forall(member(D, DescList), process_family_member(D, Name))
    ; true
    ).

%% create_married
married(X, Y) :- spouse(X, Y).
married(X, Y) :- spouse(Y, X).

%% complete_parent_child()
complete_parent_child() :- 
    parent_child(Parent, Child, Lineage),
    married(Parent, ParentSpouse),
    assertz(parent_child(ParentSpouse, Child, Lineage)). 


%% Helpers
get_name(Dict, Name) :-
    ( get_dict(name, Dict, Name) -> true
    ; throw(error(missing_name(Dict), _))
    ).

get_gender(Dict, Gender) :-
    ( get_dict(gender, Dict, Gender) -> true
    ; Gender = unknown ).

get_status(Dict, Status) :-
    ( get_dict(status, Dict, Status) -> true
    ; Status = unknown ).

normalize_lineage(Raw, Atom) :-
    ( atom(Raw) -> atom_string(Raw, S) ; S = Raw ),
    string_lower(S, L),
    ( L = "Paternal" -> Atom = paternal
    ; L = "Maternal" -> Atom = maternal
    ; L = "Both"     -> Atom = both
    ; atom_string(Atom, L)  % fallback
    ).
