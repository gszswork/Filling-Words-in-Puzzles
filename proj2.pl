:- ensure_loaded(library(clpfd))

list_tree([], empty).
list_tree([E|List], node(Left,Elt,Right)) :-
    length(List, Len),
    Len2 is Len // 2,
    length(Front, Len2),
    append(Front, [Elt|Back], [E|List]),
    list_tree(Front, Left),
    list_tree(Back, Right).
