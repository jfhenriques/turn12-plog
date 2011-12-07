
:- use_module(library(clpfd)).


get_elements_pos( _,  _, -1,-1,-1,-1,   A,B,C,D,   A,B,C,D   ):-!.
get_elements_pos( [X|T],  Pos,  Pos,P2,P3,P4,   _,B,C,D,   E,F,G,H   ) :- !,
	Pos1 is Pos - 1,
	get_elements_pos( T,  Pos1,  -1,P2,P3,P4,   X,B,C,D,   E,F,G,H   ).
get_elements_pos( [X|T],  Pos,  P1,Pos,P3,P4,   A,_,C,D,   E,F,G,H   ) :- !,
	Pos1 is Pos - 1,
	get_elements_pos( T,  Pos1,  P1,-1,P3,P4,   A,X,C,D,   E,F,G,H   ).
get_elements_pos( [X|T],  Pos,  P1,P2,Pos,P4,   A,B,_,D,   E,F,G,H   ) :- !,
	Pos1 is Pos - 1,
	get_elements_pos( T,  Pos1,  P1,P2,-1,P4,   A,B,X,D,   E,F,G,H   ).
get_elements_pos( [X|T],  Pos,  P1,P2,P3,Pos,   A,B,C,_,   E,F,G,H   ) :- !,
	Pos1 is Pos - 1,
	get_elements_pos( T,  Pos1,  P1,P2,P3,-1,   A,B,C,X,   E,F,G,H   ).
get_elements_pos( [_|T],  Pos,  P1,P2,P3,P4,   A,B,C,D,   E,F,G,H   ) :- !,
	Pos1 is Pos - 1,
	get_elements_pos( T,  Pos1,  P1,P2,P3,P4,   A,B,C,D,   E,F,G,H   ).

	
print_face_cross(FaceN,C1,C2,C3,C4):-
	write('['),
	write(FaceN),
	write('] '),
	write(C1), write(','),
	write(C2), write(','),
	write(C3), write(','),
	write(C4), nl.


shifted_face( Face, Shift, TotalElements, Distance, C1, C2, C3, C4 ) :- !,
	S_F1 is ( Shift mod TotalElements ),
	S_F2 is ( ( Shift + Distance ) mod TotalElements ),
	S_F3 is ( ( Shift + ( 2 * Distance ) ) mod TotalElements ),
	S_F4 is ( ( Shift + ( 3 * Distance ) ) mod TotalElements ),
	TotalElementsIn is TotalElements - 1,
	get_elements_pos( Face,  TotalElementsIn,   S_F1,S_F2,S_F3,S_F4,  0,0,0,0,  C1,C2,C3,C4 ).

	
print_rots(R1,R2,R3,R4,R5,R6) :-
	write('R_TT: '), write( R1 ), nl,
	write('R_BA: '), write( R2 ), nl,
	write('R_RR: '), write( R3 ), nl,
	write('R_LL: '), write( R4 ), nl,
	write('R_FF: '), write( R5 ), nl,
	write('R_BO: '), write( R6 ), nl.

	
string_to_list(String,ListOut):-!,
	string_to_list(String,[],ListOut).
string_to_list([H|T],ListIn,ListOut):-!,
	Val is H - 48, % o número zero tem o código ascii 48
	string_to_list(T,[Val|ListIn],ListOut).
string_to_list(_,List,List):-!.


w_top_line:- write(' _________').
w_top_line_f:- write('_________').
w_side_space:- write('          ').
w_top_bot_num(N):-write('|    '),write(N),write('    ').
w_bot_num_line(N):- write('|____'),write(N),write('____').
w_empty_line:- write('|         ').
w_middle_nums(TREE_L_DESC,N1,N2):- write('|'),write(N1),write('  ') , write(TREE_L_DESC), write('  '),write(N2).
w_cube_end:-write('|').


project_cube(  TT_C1,TT_C2,TT_C3,TT_C4,   BA_C1,BA_C2,BA_C3,BA_C4,   RR_C1,RR_C2,RR_C3,RR_C4,
               LL_C1,LL_C2,LL_C3,LL_C4,   FF_C1,FF_C2,FF_C3,FF_C4,   BO_C1,BO_C2,BO_C3,BO_C4 ) :-
	
	% BACK
	w_side_space,                       w_top_line,                         nl,
	w_side_space,                       w_top_bot_num(BA_C1),               w_cube_end, nl,
	w_side_space,                       w_empty_line,                       w_cube_end, nl,
	w_side_space,                       w_middle_nums('BAC', BA_C4, BA_C2), w_cube_end, nl,
	w_side_space,                       w_empty_line,                       w_cube_end, nl,
	w_top_line,                         w_bot_num_line(BA_C3),w_cube_end,   w_top_line_f,                      w_top_line, nl,
	
	% LEFT                              TOP                                 RIGHT                               BOTTOM
	w_top_bot_num(LL_C1),               w_top_bot_num(TT_C1),               w_top_bot_num(RR_C1),               w_top_bot_num(BO_C1),               w_cube_end, nl,
	w_empty_line,                       w_empty_line,                       w_empty_line,                       w_empty_line,                       w_cube_end, nl,
	w_middle_nums('LEF', LL_C4, LL_C2), w_middle_nums('TOP', TT_C4, TT_C2), w_middle_nums('RIG', RR_C4, RR_C2), w_middle_nums('BOT', BO_C4, BO_C2), w_cube_end, nl,
	w_empty_line,                       w_empty_line,                       w_empty_line,                       w_empty_line,                       w_cube_end, nl,
	w_bot_num_line(LL_C3),              w_bot_num_line(TT_C3),              w_bot_num_line(RR_C3),              w_bot_num_line(BO_C3),              w_cube_end, nl,
	
	% FRONT
	w_side_space,                       w_top_bot_num(FF_C1),               w_cube_end, nl,
	w_side_space,                       w_empty_line,                       w_cube_end, nl,
	w_side_space,                       w_middle_nums('FRO', FF_C4, FF_C2), w_cube_end, nl,
	w_side_space,                       w_empty_line,                       w_cube_end, nl,
	w_side_space,                       w_bot_num_line(FF_C3),              w_cube_end, nl.


	

processStreamLine(Stream, ListIn, ListOut) :-
	at_end_of_stream( Stream ), !,
	ListOut = ListIn.
processStreamLine(Stream, ListIn, ListOut) :-
	at_end_of_line(Stream), !,
	skip_line(Stream),
	ListOut = ListIn.
processStreamLine(Stream, ListIn, ListOut) :- !,
	get_code(Stream, Code),
	Number is Code - 48,
	processStreamLine(Stream, [Number|ListIn], ListOut).
	
	
parse_file(Filename, Top, Bottom, Front, Back, Left, Right) :-
	open(Filename, read, Stream),
	processStreamLine(Stream, [], Top),
	processStreamLine(Stream, [], Bottom),
	processStreamLine(Stream, [], Front),
	processStreamLine(Stream, [], Back),
	processStreamLine(Stream, [], Left),
	processStreamLine(Stream, [], Right),
	close(Stream).
	
verifyLinesLength(Top, Bottom, Front, Back, Left, Right, ToElements):-
	length(Top,ToElements),
	length(Bottom,L2),
	length(Front,L3),
	length(Back,L4),
	length(Left,L5),
	length(Right,L6),
	ToElements = L2,
	ToElements = L3,
	ToElements = L4,
	ToElements = L5,
	ToElements = L6, !.
	
verifyLinesLength(_,_,_,_,_,_,_):-!,
	write('Nem todas as linhas têm o mesmo comprimento. Abortando.'), nl,
	abort.
	
verifyLineDistance(Size, Distance):-
	Size mod 4 =:= 0, !,
	Distance is floor(Size / 4).
	
verifyLineDistance(_, _):-!,
	write('O comprimento das linhas não é múltiplo de 4. Abortando'), nl,
	abort.

turn12:-
	parse_file('C:/Users/João Henriques/Desktop/Eng. Informática/FEUP/PLOG/turn12/src/cubo.txt',
						Top, Bottom, Front, Back, Left, Right),
						
	verifyLinesLength(Top, Bottom, Front, Back, Left, Right, TotElems),
	verifyLineDistance( TotElems, Distance ),	  

	/*string_to_list( "356735869637537564374649", Top    ),
	string_to_list( "343574954363596738547975", Bottom ),
	string_to_list( "353748635975458485676439", Front  ),
	string_to_list( "349576573795398457964379", Back   ),
	string_to_list( "357863653739359498735895", Left   ),
	string_to_list( "348765738453874583769785", Right  ),*/

	
	Rotations = [ R1, R2, R3, R4, R5, R6 ],

	domain(Rotations, 0, 23),

	labeling([], [R1]),
	shifted_face( Top   , R1, TotElems, Distance, TT_C1, TT_C2, TT_C3, TT_C4 ),
	labeling([], [R2]),
	shifted_face( Back  , R2, TotElems, Distance, BA_C1, BA_C2, BA_C3, BA_C4 ),
	
	TT_C1 + BA_C3 #= 12,

	labeling([], [R3]),
	shifted_face( Right , R3, TotElems, Distance, RR_C1, RR_C2, RR_C3, RR_C4 ),
	TT_C2 + RR_C4 #= 12,
	RR_C1 + BA_C2 #= 12,
	
	labeling([], [R4]),
	shifted_face( Left  , R4, TotElems, Distance, LL_C1, LL_C2, LL_C3, LL_C4 ),
	TT_C4 + LL_C2 #= 12,
	LL_C1 + BA_C4 #= 12,
	
	labeling([], [R5]),
	shifted_face( Front , R5, TotElems, Distance, FF_C1, FF_C2, FF_C3, FF_C4 ),
	TT_C3 + FF_C1 #= 12,
	RR_C3 + FF_C2 #= 12,
	LL_C3 + FF_C4 #= 12,

	labeling([], [R6]),
	shifted_face( Bottom, R6, TotElems, Distance, BO_C1, BO_C2, BO_C3, BO_C4 ),
	BO_C3 + FF_C3 #= 12,
	BO_C2 + LL_C4 #= 12,
	BO_C4 + RR_C2 #= 12,
	BO_C1 + BA_C1 #= 12,
	
	
	print_rots( R1,R2,R3,R4,R5,R6 ),
	nl,
	
	project_cube( TT_C1,TT_C2,TT_C3,TT_C4,   BA_C1,BA_C2,BA_C3,BA_C4,   RR_C1,RR_C2,RR_C3,RR_C4,
                  LL_C1,LL_C2,LL_C3,LL_C4,   FF_C1,FF_C2,FF_C3,FF_C4,   BO_C1,BO_C2,BO_C3,BO_C4 ),
	
	nl,nl,
	write('More possibilities ?'),
	1 = 2. % falha para ver se existem mais possibilidades
