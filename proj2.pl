%%  File Name: proj2.pl
%%  Author: SHUZHI GONG <shuzhig@student.unimelb.edu.au> ID: 1047975

/* 
----Problem Description:
    A fill-in puzzle (sometimes called a fill-it-in) is like a crossword 
    puzzle, except that instead of being given obscure clues telling us which 
    words go where, you are given a list of all the words to place in the 
    puzzle, but not told where they go.
    The puzzle consists of a grid of squares, most of which are empty, into 
    which letters or digits are to be written, but some of which are filled in 
    solid, and are not to be written in. You are also given a list of words to 
    place in the puzzle.
    You must place each word in the word list exactly once in the puzzle, 
    either left-to-right or top-to-bottom, filling a maximal sequence of empty 
    squares. Also, every maximal sequence of non-solid squares that is more 
    than one square long must have one word from the word list written in it. 
    Many words cross one another, so many of the letters in a horizontal word 
    will also be a letter in a vertical word. For a properly constructed 
    fill-in puzzle, there will be only one way to fill in the words (in some 
    cases, the puzzle is symmetrical around a diagonal axis, in which case 
    there will be two symmetrical solutions).
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%************************puzzle_solution************************************%
/*
puzzle_solution(?Puzzle, +WordList).
    To solve the puzzle problem:
Note:   1. convert the Puzzle matrix to rows of slots
        2. fill words in those slots
    More description will be contained in detailed function comments.
*/
:- ensure_loaded(library(clpfd)).
puzzle_solution(Puzzle, WordList):-
    convert_to_slots(Puzzle, Slots),                      % [Note 1] 
    filling_words(Slots, WordList).                       % [Note 2]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%********************convert_to_slots part**********************************%
/*
convert_to_slots(+Puzzle, -Slots).
    Convert Puzzle matrix to slots. A slot is a maximal horizontal or vertical 
    sequence of fill-able and pre-filled squres.
    To convert the matrix to slots (get all pre-filled/fill-able) squares:
Note:   1. get rows of slots from horizontal aspect.
        2. transpose the matrix and get rows of slots from vertical aspect.
        3. only include valid slots whose length is larger than 1.
        4. append the horizontal and vertical slots to variable Slots.
*/
convert_to_slots(Puzzle, Slots):- 
    get_slots(Puzzle, HSlots),                             % [Note 1]
    transpose(Puzzle, PuzzleT),
    get_slots(PuzzleT, VSlots),                            % [Note 2]
    include(valid,HSlots,NewHSlots),
    include(valid,VSlots,NewVSlots),                       % [Note 3]   
    append(NewHSlots, NewVSlots, Slots).                   % [Note 4]

/*
valid(+Slot).
    Auxiliary function used in convert_to_slots/2, the predicates hold only 
    when Slot's length is larger than 1.
*/
valid(Slot):-
    length(Slot,Length), 
    Length #> 1.

/*
get_slots(+Rows, -Slots).
    Get all horizontal slots row by row with predicate get_row_slot which can 
    extract slots in one row. 
    To get all slots: 
Note:   1. use predicates get_row_slot to extract slots in one row to 
        CurrentRowSlots.
        2. append CurrentRowSlots and Accumulated Slots to Slots.
        3. Tail Recurision.
*/
get_slots([], []).
get_slots([R|Rows], Slots):-                              
    get_row_slot(R, [], CurrentRowSlots),                  % [Note 1]
    append(CurrentRowSlots, AccRowSlots, Slots),           % [Note 2]
    get_slots(Rows, AccRowSlots).                          % [Note 3]

/*
get_row_slot(+List, -Acc, -Slots).
    Slots in one row. The 2nd parameter Acc in get_row_slot/3 is accumulartor
    which recursively adds squares until meets char '#'. When meets '#', the 
    get_row_slot/3 will add the accumulator to Slots and reset accumulator.
    To get slots in one row:
Note:   1. if no elements in the current list, add Acc to Slots when Acc not 
        empty.
        2. if meets '#', add the Acc to Slots and reset Acc as [] (empty).
        3. else (doesn't meet '#'), keep adding squre elements to Acc.
*/
get_row_slot([],[],[]).
get_row_slot([], Acc, [Acc]).                               % [Note 1]
get_row_slot([H|Rest], Acc, Slots):-
    ( H == '#' ->
        Slots = [Acc|Slots1],
        get_row_slot(Rest,[], Slots1)                       % [Note 2]
    ;   append(Acc,[H],Acc1),
        get_row_slot(Rest,Acc1,Slots)                       % [Note 3]
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%**************************filling_words part*******************************%
/*
filling_words(?Slot, +WordList).
    Fill words from WordList into Slots, unify when all match. The strategy is:
    Each time a word is to be placed, you should count the number of words that 
    match each slot, and select (one of) the slot(s) with the fewest matching
    words to fill. When finish a filling process, remove matching word and slot
    from their sets.
    To fill into words:
Note:   1. select best slot with fewest matching words and filling responding 
        word.
        2. get the matching word.
        3. remove the best slot and the filled word
        4. Tail recurision until Slots is empty. 
*/
filling_words([],[]).
filling_words(Slots, Wordlist):-
    best_slot(Slots, Wordlist, BestSlot),                   % [Note 1]
    exclude(\=(BestSlot), Wordlist, MatchWord),
    member(Word, MatchWord),
    BestSlot = Word,                                        % [Note 2]
    exclude(==(Word), Wordlist, NewWords),
    exclude(==(BestSlot), Slots, NewSlots),
    filling_words(NewSlots, NewWords).                      % [Note 3]
  
/*
best_slot(+Slots, +WordList, -BestSlot).
    It will return the best slot with least amount of 
    matching words. (This can minimuse the search space based on Hint7.)
    The selection is assessed in 2 steps:
Note:   1. get MatchNumber by match_word_num/3.
        2. pass the current slot and matching number to select_best_slot/5
*/
best_slot([S|Slots], Wordlist, BestSlot):-
    match_word_num(S, Wordlist, 0, MatchNumber),                   % [Note 1]
    select_best_slot(Slots, Wordlist, MatchNumber, S, BestSlot).   % [Note 2]

/*
match_word_num(?Slot, +WordList, -Acc, -MatchNumber).
    It can traverse the WordList and count how many words match Slot.
    To get the matching word number:
Note:   1. When the WordList is empty, return the Acc as matching number.
        2. Unify and add Acc if match, then tail recurision.
*/
match_word_num(_,[], Acc, Acc).                               % [Note 1]
match_word_num(Slot, [W|Wordlist], Acc, MatchNumber):-
    (Slot \= W->                                              % [Note 2]
        match_word_num(Slot, Wordlist, Acc, MatchNumber)
    ;   % Note the Slot (part of Puzzle) will be unified here. 
        match_word_num(Slot, Wordlist, Acc+1, MatchNumber)
    ).
    
/*
select_best_slot(+Slots, +WordList, +MatchNumber, -CurrentBest, -BestSlot).
    Traverse the slot list (Slots) to check whether the slot passed from
    best_slot (CurrentBest) is really the best.
Note:   1. If Slots is empty, then the current best is the final best.
        2. If one slot's match number is not smaller, just go next.
        3. If one slot's match number is smaller, change the current best and 
        match number, then go next.
*/
select_best_slot([], _, _, BestSlot, BestSlot).                % [Note 1]
select_best_slot([S|Slots], Wordlist, MatchNumber, CurrentBest, BestSlot):-
    match_word_num(S, Wordlist, 0, CandidateNum),
    (CandidateNum >= MatchNumber->                             % [Note 2]
        select_best_slot(Slots, Wordlist, MatchNumber, CurrentBest, BestSlot)
                                                               % [Note 3]
    ;   select_best_slot(Slots, Wordlist, CandidateNum, S, BestSlot)
    ).


