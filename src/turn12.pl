
:- use_module(library(clpfd)).
%:- use_module(library(system)).


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
		%write(P1), write(','), write(P2), write(','), write(P3), write(','), write(P4), nl,
		get_elements_pos( T,  Pos1,  P1,P2,P3,P4,   A,B,C,D,   E,F,G,H   ).
		
		
/*print_time:-
	datime(T),
	write(T).*/

print_face_cross(FaceN,C1,C2,C3,C4):-
	write('Face['),
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
	/*write('['),
	write(L),
	write('] '),
	write(R1), write(','),
	write(R2), write(','),
	write(R3), write(','),
	write(R4), write(','),
	write(R5), write(','),
	write(R6), nl.*/
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
		
turn12:-
	
	string_to_list( "356735869637537564374649", Top    ),
	string_to_list( "343574954363596738547975", Bottom ),
	string_to_list( "353748635975458485676439", Front  ),
	string_to_list( "349576573795398457964379", Back   ),
	string_to_list( "357863653739359498735895", Left   ),
	string_to_list( "348765738453874583769785", Right  ),
	
	TotElems is 24,
	Distance is 6,
	
	Rotations = [ R1, R2, R3, R4, R5, R6 ],
	domain(Rotations, 1, 24),
	
	/*now(StartRun),
	print_time, nl,*/
		
	labeling([], [R1]),
	shifted_face( Top   , R1, TotElems, Distance, TT_C1, TT_C2, TT_C3, TT_C4 ),
	labeling([], [R2]),
	shifted_face( Back  , R2, TotElems, Distance, BA_C1, BA_C2, BA_C3, BA_C4 ),
	%print_rots(1, R1,R2,0,0,0,0),
	TT_C1 + BA_C3 #= 12,
	
	labeling([], [R3]),
	shifted_face( Right , R3, TotElems, Distance, RR_C1, RR_C2, RR_C3, RR_C4 ),
	%print_rots(2, R1,R2,R3,0,0,0),
	TT_C2 + RR_C4 #= 12,
	RR_C1 + BA_C2 #= 12,
	
	labeling([], [R4]),
	shifted_face( Left  , R4, TotElems, Distance, LL_C1, LL_C2, LL_C3, LL_C4 ),
	%print_rots(3, R1,R2,R3,R4,0,0),
	TT_C4 + LL_C2 #= 12,
	LL_C1 + BA_C4 #= 12,
	
	labeling([], [R5]),
	shifted_face( Front , R5, TotElems, Distance, FF_C1, FF_C2, FF_C3, FF_C4 ),
	%print_rots(4, R1,R2,R3,R4,R5,0),
	TT_C3 + FF_C1 #= 12,
	RR_C3 + FF_C2 #= 12,
	LL_C3 + FF_C4 #= 12,
	
	labeling([], [R6]),
	shifted_face( Bottom, R6, TotElems, Distance, BO_C1, BO_C2, BO_C3, BO_C4 ),
	%print_rots(5, R1,R2,R3,R4,R5,R6),
	
	BO_C3 + FF_C3 #= 12,
	BO_C2 + LL_C4 #= 12,
	BO_C4 + RR_C2 #= 12,
	BO_C1 + BA_C1 #= 12,
	
	/*nl, print_time, nl,
	now(EndRun),
	TotalTime is (EndRun - StartRun),
	write('Total time is: '),
	write(TotalTime), nl,*/
	
	print_rots( R1,R2,R3,R4,R5,R6 ),
	
	print_face_cross('Top   ', TT_C1, TT_C2, TT_C3, TT_C4 ),
	print_face_cross('Back  ', BA_C1, BA_C2, BA_C3, BA_C4 ),
	print_face_cross('Right ', RR_C1, RR_C2, RR_C3, RR_C4 ),
	print_face_cross('Left  ', LL_C1, LL_C2, LL_C3, LL_C4 ),
	print_face_cross('Front ', FF_C1, FF_C2, FF_C3, FF_C4 ),
	print_face_cross('Bottom', BO_C1, BO_C2, BO_C3, BO_C4 ),
	nl,
	write('More possibilities ?'),
	1 = 2. % falha para ver se existem mais possibilidades
	
