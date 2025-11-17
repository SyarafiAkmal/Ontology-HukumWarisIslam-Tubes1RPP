/* family tree */
person(ahmed, male, alive).
person(fatima, female, deceased).
person(hamzah, male, alive).
person(khadijah, female, alive).
person(khalid, male, deceased).
person(amina, female, deceased).

spouse(ahmed, fatima).
spouse(hamzah, khadijah).

parent_child(ahmed, khalid, both).
parent_child(ahmed, amina, both).
parent_child(hamzah, ahmed, both).