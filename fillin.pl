%  File     : fillin.pl
%  Author   : YuKun
%  Student ID   : 776991
%  Purpose  : Proj2 project Fillin puzzles

% Given an empty (or almost empty) grid of squares and 
% a list of words( or other conponents like digital numbers). 
% The problem is to place these elements (each word can only be used once)
% into the squares.
% The solid is some places where the letters can not be filled in.
% The particular Fillin puzzle is wrote in a text file, including
% lists of the words (one word per line) and the shape of the squares. 
% In this squares specification, an empty character
% location is represented by '_', while the solid is pointed as '#'
% Words are strings (character lists) of at least two characters. 
% A horizontal or vertical sequence of character places in the 
% Fillin squares is called a slot. Our problem is to find a 
% compatible way of placing words onto slots.
% Many words may cross one another.

:- ensure_loaded(library(clpfd)).

% main is the entrance of the program

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

% this	part is to read the file 
	
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

% this part is to print the puzzle into a file	
	
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

put_puzzle_char(Stream, Char) :-
    (   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).

samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).

% This is the main part for solving the puzzle problem.	
% To solve the problem, we will have two steps.
% Firstly, we will find the words which only have an unique length and fill 
% in the squares. These words obviously have one possible position to be filled.
% Secondly, we will fill the rest words in the squares.
% The 'transpose' predicate is been used to fill the vertical sequences.
% When we try to fill the other words,
% we will change the squares into list of slots which replace the '_' with
% the predicate 'fill/1' and the free variable. For example, the puzzle 
% [['#','_','#'],['a','b','a'],['_','#','_']] which has been filled with unique length words
% will be traslated to a list of slots [['a','b','a'],['a',fill(A)],[fill(B),'b'],['a',fill(C)]]
% After filling the slots, the puzzle will be solved as well.
	
solve_puzzle(Puzzle, Wordlist, Solved):-
	find_uniWord(Wordlist,Wordlist,[],Uniwordlist),
	(Uniwordlist=[]
	->	solve_otherpuzzle(Puzzle, Wordlist, Solved)
	;	solve_unipuzzle(Puzzle, Uniwordlist,[],Restword, Puzzle1),
		(	Restword=[]
		->	find_otherWord(Wordlist,Uniwordlist,Otherwordlist),			
			solve_otherpuzzle(Puzzle1, Otherwordlist, Solved)
		;	transpose(Puzzle1,TPuzzle1),
			solve_unipuzzle(TPuzzle1, Restword,[],[], TPuzzle2),
			transpose(TPuzzle2,Puzzle2),
			find_otherWord(Wordlist,Uniwordlist,Otherwordlist),
			solve_otherpuzzle(Puzzle2, Otherwordlist, Solved)
		)
	).

% compare and record the number which is the same length word's appeared times in the word list
	
compareword(_,[],Count,Count).
compareword(Word1,[Word2|Wordlist],Count1,Count):-
    (samelength(Word1,Word2)
    -> 	Count2 is Count1+1,
		compareword(Word1,Wordlist,Count2,Count)
	; 	compareword(Word1,Wordlist,Count1,Count)
	).

% find the unique length words
	
find_uniWord(_,[],Uniwordlist,Uniwordlist).
find_uniWord(Wordlist,[Word|Wordlist1],Uniwordlist1,Uniwordlist):-
	(compareword(Word,Wordlist,0,1)
	->	append(Uniwordlist1,[Word],Uniwordlist2),
		find_uniWord(Wordlist,Wordlist1,Uniwordlist2,Uniwordlist)
	;	find_uniWord(Wordlist,Wordlist1,Uniwordlist1,Uniwordlist)
	).

% return the words except the unique length words	
	
find_otherWord(Otherwordlist,[],Otherwordlist).
find_otherWord(Wordlist,[Word|Wordlist1],Otherwordlist):-
	delete(Wordlist,Word,Otherwordlist1),
	find_otherWord(Otherwordlist1,Wordlist1,Otherwordlist).

% fill the unique length words into the squares, if the word can
% not be filled in a horizontal sequence,we will keep it and fill it in 
% a vertical sequence later. We can ensure that the word can be filled in
% the puzzle once, so it will appear in the either a horizontal sequence 
% or a vertical sequence. 
	
solve_unipuzzle(Puzzle,[],Restword,Restword,Puzzle).
solve_unipuzzle(Puzzle,[Word|Wordlist],Restword1,Restword,Solved):-
	solve_slots(Puzzle,Word,[],Solved1),
	(Solved1=Puzzle
	-> 	solve_unipuzzle(Solved1, Wordlist,[Word|Restword1],Restword,Solved)
	; 	solve_unipuzzle(Solved1, Wordlist,Restword1,Restword,Solved)
	).

% fill the unique length words into the slots	
	
solve_slots([],_,Solved,Solved).
solve_slots([Slot|Puzzle],Word,Solved1,Solved):-
	solve_slot(Slot,Word,[],FSlot),
	(FSlot=Slot
	->	append(Solved1,[FSlot],Solved2),
		solve_slots(Puzzle,Word,Solved2,Solved)
	;	append(Solved1,[FSlot|Puzzle],Solved)
	).

% fill the unique length words into one slot	
	
solve_slot([],_,FSlot,FSlot).
solve_slot(Slot,Word,FSlot1,FSlot):-
	scan_slot(Slot,0,Count,[],PrSlot,PoSlot),
	(length(Word,Count),check_slot(PrSlot,Word)
	->	write_slot(PrSlot,Word,[],FPrSlot),
		append(FPrSlot,PoSlot,Temp),
		append(FSlot1,Temp,FSlot)
	;	append(FSlot1,PrSlot,FSlot2),
		solve_slot(PoSlot,Word,FSlot2,FSlot)
	).

% check if the slot can be filled with current word
	
check_slot(_,[]).
check_slot([S|PrSlot],[W|Word]):-
	(S='#'
	->	check_slot(PrSlot,[W|Word])
	;S='_'
	->	check_slot(PrSlot,Word)
	;S=W
	->	check_slot(PrSlot,Word)
	;	false
	).

% scan the slot and return the possible length that can be filled
	
scan_slot([],Count,Count,PrSlot,PrSlot,[]).
scan_slot([S|Slot],Count1,Count,PrSlot1,PrSlot,PoSlot):-
	(S='#',Count1=0
	->	append(PrSlot1,[S],PrSlot2),
		scan_slot(Slot,Count1,Count,PrSlot2,PrSlot,PoSlot)
	;S\='#'
	->	Count2 is Count1+1,
		append(PrSlot1,[S],PrSlot2),
		scan_slot(Slot,Count2,Count,PrSlot2,PrSlot,PoSlot)
	;	append(PrSlot1,[S],PrSlot2),
		Count=Count1,
		PrSlot=PrSlot2,
		PoSlot=Slot
	).

% fill the slot	
	
write_slot(PrSlot,[],FPrSlot2,FPrSlot):-
	append(FPrSlot2,PrSlot,FPrSlot).
write_slot([S|PrSlot],[W|Word],FPrSlot1,FPrSlot):-
	(S='#'
	->	append(FPrSlot1,[S],FPrSlot2),
		write_slot(PrSlot,[W|Word],FPrSlot2,FPrSlot)
	;	append(FPrSlot1,[W],FPrSlot2),
		write_slot(PrSlot,Word,FPrSlot2,FPrSlot)
	).

% fill rest words in the squares.
% We will firstly change the puzzle by replace the '_'
% with predicate 'fill/1' and get the list of the slots from 
% horizontal or vertical sequences.
% After filling the slots, the puzzle will also been solved.
% For example, the puzzle [['#','_','#'],['a','b','a'],['_','#','_']]
% which has been filled with unique length words
% will be traslated to a list of slots [['a','b','a'],['a',fill(A)],[fill(B),'b'],['a',fill(C)]]
% Then after we fill the free variable 'A' with letter 'a', the slots will become 
% [['a','b','a'],['a',fill('a')],[fill(B),'b'],['a',fill(C)]], and at the same time the puzzle
% will become [['#',fill(B),'#'],['a','b','a'],[fill('a'),'#',fill(C)]]
% Finally, we just need to change the puzzle with predicate 'fill/1' back to 
% the value of the predicate 'fill/1', and the puzzle will become
% [['#','b','#'],['a','b','a'],['a','#','c']].
	
solve_otherpuzzle(Puzzle,[],Puzzle).
solve_otherpuzzle(Puzzle,Wordlist,Solved):-
	change_slots(Puzzle,[],Puzzle1),
	get_slots(Puzzle1,[],Slots1),
	transpose(Puzzle1,TPuzzle1),
	get_slots(TPuzzle1,[],Slots2),
	append(Slots1,Slots2,Slots3),
	sort_slots_by_value(Slots3, Slots),
	fill_slots(Wordlist,Slots),
	change_backs(Puzzle1,[],Solved).

% sort the slots by its have-been-filled positions' number which
% means that the more squares the slot have been filled,the earlier
% we will choose to handle it. In this way, we can dramatically reduce the
% search space. 

sort_slots_by_value(Slots, ByDValue):-
    map_list_to_pairs(getFilledSlotsC, Slots, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, ByValue),
	reverse(ByValue,ByDValue).

% get the number of the slot's been-filled point	
	
getFilledSlotsC(Slot,Count):-
	getFilledSlotsC2(Slot,0,Count).

getFilledSlotsC2([],Count,Count).
getFilledSlotsC2([S|Slot],Count1,Count):-
	arg(_,S,L),
	(var(L)
	->	getFilledSlotsC2(Slot,Count1,Count)
	;	Count2 is Count1+1,
		getFilledSlotsC2(Slot,Count2,Count)
	).

% try to fill the slots with the words' letter	
	
fill_slots([],[]).
fill_slots(Wordlist,[S|Slots]):-
	find_possibleSlot(S,Wordlist,[],PossibleList),
	(PossibleList\=[]
	->	select(Word,PossibleList,_),
		matchvalue(Word,S),
		delete(Wordlist,Word,Wordlist1),
		sort_slots_by_value(Slots, Slots2),
		fill_slots(Wordlist1,Slots2)
	;	sort_slots_by_value(Slots, Slots2),
		fill_slots(Wordlist,Slots2)
	).
	
% check if the word fixes the slot	
	
matchvalue([],[]).
matchvalue([L|Ls],[S|Ss]):-
	arg(_,S,L),
	matchvalue(Ls,Ss).
	
% find possible word list for a slot
	
find_possibleSlot(_,[],PossibleList,PossibleList).
find_possibleSlot(Slot,[W|Wordlist],PossibleList1,PossibleList):-
	(same_length(W,Slot)
	->	append(PossibleList1,[W],PossibleList2),
		find_possibleSlot(Slot,Wordlist,PossibleList2,PossibleList)
	;	find_possibleSlot(Slot,Wordlist,PossibleList1,PossibleList)
	).

% change the squares into list of slots which replace the '_' with
% the functor 'fill' and the free variable. For example, the puzzle 
% [['#','_','#'],['a','b','a'],['_','#','_']] which has been filled with unique length words
% will be traslated to a list of slots [['a','b','a'],['a',fill(A)],[fill(B),'b'],['a',fill(C)]]
	
change_slots([],Solved,Solved).
change_slots([Slot|Puzzle],Solved1,Solved):-
	change_slot(Slot,[],FSlot),
	append(Solved1,[FSlot],Solved2),
	change_slots(Puzzle,Solved2,Solved).

change_slot([],FSlot,FSlot).
change_slot([S|Slot],FSlot1,FSlot):-
	(S='#'
	->	append(FSlot1,[S],FSlot2),
		change_slot(Slot,FSlot2,FSlot)
	;S='_'
	->	functor(T,fill,1),
		append(FSlot1,[T],FSlot2),
		change_slot(Slot,FSlot2,FSlot)
	;	functor(T,fill,1),
		arg(_,T,S),
		append(FSlot1,[T],FSlot2),
		change_slot(Slot,FSlot2,FSlot)
	).
	
% get the slots' list from the puzzle	
	
get_slots([],Solved,Solved).
get_slots([Slot|Puzzle],Solved1,Solved):-
	get_slot(Slot,0,[],[],Slots),
	append(Solved1,Slots,Solved2),
	get_slots(Puzzle,Solved2,Solved).

get_slot([],Count,PoSlot1,Slots1,Slots):-
	(PoSlot1\=[],Count>1
	->	Slots=[PoSlot1|Slots1]
	;	Slots=Slots1
	).
	
get_slot([S|Slot],Count1,PoSlot1,Slots1,Slots):-
	(S='#',Count1=0
	->	get_slot(Slot,Count1,PoSlot1,Slots1,Slots)
	;S\='#'
	->	Count2 is Count1+1,
		append(PoSlot1,[S],PoSlot2),
		get_slot(Slot,Count2,PoSlot2,Slots1,Slots)
	;Count1>1
	->	append(Slots1,[PoSlot1],Slots2),
		get_slot(Slot,0,[],Slots2,Slots)
	;	get_slot(Slot,0,[],Slots1,Slots)
	).

% change the functor back to value and return the solved puzzle
	
change_backs([],Solved,Solved).
change_backs([Slot|Puzzle],Solved1,Solved):-
	change_back(Slot,[],FSlot),
	append(Solved1,[FSlot],Solved2),
	change_backs(Puzzle,Solved2,Solved).

change_back([],FSlot,FSlot).
change_back([S|Slot],FSlot1,FSlot):-
	(S='#'
	->	append(FSlot1,[S],FSlot2),
		change_back(Slot,FSlot2,FSlot)
	;	functor(S,fill,1),
		arg(_,S,A),
		append(FSlot1,[A],FSlot2),
		change_back(Slot,FSlot2,FSlot)
	).