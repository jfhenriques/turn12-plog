
:- use_module(library(clpfd)).


element_pos(0, [X|_], X).
element_pos(Pos, [_|T], X) :-
		Pos > 0,
		Pos1 is Pos - 1,
		element_pos(Pos1, T, X).


shifted_face(Face, Shift, TotalElements, Distance, C1, C2, C3, C4 ) :- !,
		S_F1 is ( Shift mod TotalElements ),
		S_F2 is ( ( Shift + Distance ) mod TotalElements ),
		S_F3 is ( ( Shift + ( 2 * Distance ) ) mod TotalElements ),
		S_F4 is ( ( Shift + ( 3 * Distance ) ) mod TotalElements ),
		element_pos( S_F1, Face, C1 ),
		element_pos( S_F2, Face, C2 ),
		element_pos( S_F3, Face, C3 ),
		element_pos( S_F4, Face, C4 ).

turn12:-

	Top    = [3,5,6,7,3,5,8,6,9,6,3,7,5,3,7,5,6,4,3,7,4,6,4,9],
	Bottom = [3,4,3,5,7,4,9,5,4,3,6,3,5,9,6,7,3,8,5,4,7,9,7,5],
	Front  = [3,5,3,7,4,8,6,3,5,9,7,5,4,5,8,4,8,5,6,7,6,4,3,9],
	Back   = [3,4,9,5,7,6,5,7,3,7,9,5,3,9,8,4,5,7,9,6,4,3,7,9],
	Left   = [3,5,7,8,6,3,6,5,3,7,3,9,3,5,9,4,9,8,7,3,5,8,9,5],
	Right  = [3,4,8,7,6,5,7,3,8,4,5,3,8,7,4,5,8,3,7,6,9,7,8,5],
	
	TotElems is 24,
	Distance is 6,
	
	Rotations = [ R1, R2, R3, R4, R5, R6 ],
	domain(Rotations, 0, 23),
	
	%labeling([], Rotations),
	
	labeling([], [R1]),
	shifted_face( Top   , R1, TotElems, Distance, TT_C1, TT_C2, TT_C3, TT_C4 ),
	write('*'),
	
	labeling([], [R2]),
	shifted_face( Bottom, R2, TotElems, Distance, BO_C1, BO_C2, BO_C3, BO_C4 ),
	write('|'),
	
	labeling([], [R3]),
	shifted_face( Front , R3, TotElems, Distance, FF_C1, FF_C2, FF_C3, FF_C4 ),
	write('.'),
	
	labeling([], [R4]),
	shifted_face( Back  , R4, TotElems, Distance, BA_C1, BA_C2, BA_C3, BA_C4 ),
	
	labeling([], [R5]),
	shifted_face( Left  , R5, TotElems, Distance, LL_C1, LL_C2, LL_C3, LL_C4 ),

	labeling([], [R6]),
	shifted_face( Right , R6, TotElems, Distance, RR_C1, RR_C2, RR_C3, RR_C4 ),

	
	TT_C1 + BA_C3 #= 12,
	TT_C2 + RR_C4 #= 12,
	TT_C3 + FF_C1 #= 12,
	TT_C4 + LL_C2 #= 12,

	BO_C1 + BA_C1 #= 12,
	BO_C2 + LL_C4 #= 12,
	BO_C3 + FF_C3 #= 12,
	BO_C4 + RR_C2 #= 12,

	LL_C1 + BA_C4 #= 12,
	LL_C3 + FF_C4 #= 12,
	
	RR_C1 + BA_C2 #= 12,
	RR_C3 + FF_C2 #= 12,
	

	write('R_TT: '), write( R1 ), nl,
	write('R_BO: '), write( R2 ), nl,
	write('R_FF: '), write( R3 ), nl,
	write('R_BA: '), write( R4 ), nl,
	write('R_FF: '), write( R5 ), nl,
	write('R_BA: '), write( R6 ), nl.
	
	
