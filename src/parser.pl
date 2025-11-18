:- use_module(library(http/json)).

:- dynamic person/4.
:- dynamic parent_of/2.
:- dynamic married/2.
:- dynamic person_counter/1.
:- dynamic name_to_id/2.

:- discontiguous(person/4).
:- discontiguous(parent_of/2).
:- discontiguous(married/2).

%% parse_family_tree(+File)
parse_family_tree(File) :-
    % Clear existing data
    retractall(person(_, _, _, _)),
    retractall(parent_of(_, _)),
    retractall(married(_, _)),
    retractall(person_counter(_)),
    retractall(name_to_id(_, _)),
    
    % Initialize counter
    assertz(person_counter(1)),
    
    % Read JSON
    open(File, read, In),
    json_read_dict(In, Root),
    close(In),
    
    % Process the family tree
    process_family_member(Root, none),
    
    % Complete parent relationships (add spouse as parent)
    complete_parent_relationships.

%% process_family_member(+Dict, +ParentID)
process_family_member(Node, ParentID) :-
    get_name(Node, Name),
    get_gender(Node, RawGender),
    get_status(Node, RawStatus),
    
    normalize_gender(RawGender, Gender),
    normalize_status(RawStatus, IsAlive),
    
    get_or_create_id(Name, PersonID),
    
    ( person(PersonID, _, _, _) 
    -> true 
    ; assertz(person(PersonID, Name, Gender, IsAlive))
    ),
    
    % Handle parent-child relationship
    ( ParentID \= none
    -> assertz(parent_of(PersonID, ParentID))
    ; true
    ),
    
    % NEW: Handle explicit parents from JSON
    ( get_dict(parents, Node, ParentsDict)
    -> process_parents(PersonID, ParentsDict)
    ; true
    ),
    
    % Handle spouse
    ( get_dict(spouse, Node, SpouseNode)
    -> process_spouse(PersonID, SpouseNode)
    ; true
    ),
    
    % Recursively process descendants
    ( get_dict(descendents, Node, DescList)
    -> forall(member(Desc, DescList), process_family_member(Desc, PersonID))
    ; true
    ).

% process_parents/2
process_parents(ChildID, ParentsDict) :-
    % Process father
    ( get_dict(father, ParentsDict, FatherNode)
    -> process_family_member(FatherNode, none),
       get_name(FatherNode, FatherName),
       name_to_id(FatherName, FatherID),
       ( parent_of(ChildID, FatherID) -> true ; assertz(parent_of(ChildID, FatherID)) )
    ; true
    ),
    
    % Process mother
    ( get_dict(mother, ParentsDict, MotherNode)
    -> process_family_member(MotherNode, none),
       get_name(MotherNode, MotherName),
       name_to_id(MotherName, MotherID),
       ( parent_of(ChildID, MotherID) -> true ; assertz(parent_of(ChildID, MotherID)) )
    ; true
    ),
    
    % Assert marriage between parents
    ( get_dict(father, ParentsDict, _),
      get_dict(mother, ParentsDict, _)
    -> get_name(FatherNode, FName),
       get_name(MotherNode, MName),
       name_to_id(FName, FID),
       name_to_id(MName, MID),
       ( married(FID, MID) -> true
       ; married(MID, FID) -> true
       ; assertz(married(FID, MID))
       )
    ; true
    ).

%% process_spouse(+PersonID, +SpouseDict)
process_spouse(PersonID, SpouseNode) :-
    get_name(SpouseNode, SpouseName),
    get_gender(SpouseNode, RawGender),
    get_status(SpouseNode, RawStatus),
    
    normalize_gender(RawGender, Gender),
    normalize_status(RawStatus, IsAlive),
    
    % Get or create ID for spouse
    get_or_create_id(SpouseName, SpouseID),
    
    % Assert spouse person if not exists
    ( person(SpouseID, _, _, _)
    -> true
    ; assertz(person(SpouseID, SpouseName, Gender, IsAlive))
    ),

    % Handle spouses parents if present
    ( get_dict(parents, SpouseNode, ParentsDict)
    -> process_parents(SpouseID, ParentsDict)
    ; true
    ),
    
    % Assert marriage (avoid duplicates)
    ( married(PersonID, SpouseID) -> true
    ; married(SpouseID, PersonID) -> true
    ; assertz(married(PersonID, SpouseID))
    ).

%% complete_parent_relationships()
% For each parent-child relationship, add the spouse as a parent too
complete_parent_relationships :-
    forall(
        (parent_of(ChildID, ParentID),
         married(ParentID, SpouseID),
         \+ parent_of(ChildID, SpouseID)),
        assertz(parent_of(ChildID, SpouseID))
    ).

%% get_or_create_id(+Name, -ID)
% Gets existing ID or creates a new one
get_or_create_id(Name, ID) :-
    ( name_to_id(Name, ID)
    -> true
    ; person_counter(N),
      atom_concat(p, N, ID),
      assertz(name_to_id(Name, ID)),
      N1 is N + 1,
      retract(person_counter(N)),
      assertz(person_counter(N1))
    ).

%% Normalization helpers
normalize_gender(Raw, male) :-
    atom_string(Raw, S),
    string_lower(S, L),
    (L = "male" ; L = "m"), !.
normalize_gender(Raw, female) :-
    atom_string(Raw, S),
    string_lower(S, L),
    (L = "female" ; L = "f"), !.
normalize_gender(_, unknown).

normalize_status(Raw, true) :-
    atom_string(Raw, S),
    string_lower(S, L),
    (L = "alive" ; L = "living" ; L = "true"), !.
normalize_status(Raw, false) :-
    atom_string(Raw, S),
    string_lower(S, L),
    (L = "dead" ; L = "deceased" ; L = "false"), !.
normalize_status(_, true).  % Default to alive

normalize_name_lowercase(Name, Lower) :-
    atom_string(Name, S),
    string_lower(S, LowerStr),
    atom_string(Lower, LowerStr).

%% Helpers
get_name(Dict, NormName) :-
    ( get_dict(name, Dict, RawName) 
    -> normalize_name_lowercase(RawName, NormName)
    ; throw(error(missing_name(Dict), _))
    ).

get_gender(Dict, Gender) :-
    ( get_dict(gender, Dict, Gender) -> true
    ; Gender = 'Unknown'
    ).

get_status(Dict, Status) :-
    ( get_dict(status, Dict, Status) -> true
    ; Status = 'Alive'
    ).

%% Utility: Print the family tree
print_family_tree :-
    writeln('=== Persons ==='),
    forall(person(ID, Name, Gender, IsAlive),
           format('person(~w, ~q, ~w, ~w).~n', [ID, Name, Gender, IsAlive])),
    nl,
    writeln('=== Marriages ==='),
    forall(married(ID1, ID2),
           format('married(~w, ~w).~n', [ID1, ID2])),
    nl,
    writeln('=== Parent-Child ==='),
    forall(parent_of(ChildID, ParentID),
           format('parent_of(~w, ~w).~n', [ChildID, ParentID])).

%% Utility: Save to file
save_family_tree(File) :-
    open(File, write, Out),
    
    % Write directives
    write(Out, ':- dynamic(person/4).\n'),
    write(Out, ':- dynamic(parent_of/2).\n'),
    write(Out, ':- dynamic(married/2).\n'),
    write(Out, ':- discontiguous(person/4).\n'),
    write(Out, ':- discontiguous(parent_of/2).\n'),
    write(Out, ':- discontiguous(married/2).\n\n'),
    
    % Write persons
    write(Out, '% --- Persons ---\n'),
    forall(person(ID, Name, Gender, IsAlive),
           format(Out, 'person(~w, ~q, ~w, ~w).~n', [ID, Name, Gender, IsAlive])),
    nl(Out),
    
    % Write marriages
    write(Out, '% --- Marriages ---\n'),
    forall(married(ID1, ID2),
           format(Out, 'married(~w, ~w).~n', [ID1, ID2])),
    nl(Out),
    
    % Write parent-child relationships
    write(Out, '% --- Parent-Child Relationships ---\n'),
    forall(parent_of(ChildID, ParentID),
           format(Out, 'parent_of(~w, ~w).~n', [ChildID, ParentID])),
    
    close(Out),
    format('Family tree saved to ~w~n', [File]).