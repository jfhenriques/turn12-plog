
:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(lists)).


/******************************************************************
 * Gets and Sets the elements of the list, in pre marked positions,
 * as the new contact points
 ******************************************************************/
 
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

	
/******************************************************************
 * Calculates the positions of new contact points based in
 * input shift.
 ******************************************************************/

shifted_face( Face, Shift, TotalElements, Distance, C1, C2, C3, C4 ) :- !,
	S_F1 is ( Shift mod TotalElements ),
	S_F2 is ( ( Shift + Distance ) mod TotalElements ),
	S_F3 is ( ( Shift + ( 2 * Distance ) ) mod TotalElements ),
	S_F4 is ( ( Shift + ( 3 * Distance ) ) mod TotalElements ),
	TotalElementsIn is TotalElements - 1,
	get_elements_pos( Face,  TotalElementsIn,   S_F1,S_F2,S_F3,S_F4,  0,0,0,0,  C1,C2,C3,C4 ).

	
/******************************************************************
 * Prints rotations
 ******************************************************************/
	
print_rots(R1,R2,R3,R4,R5,R6) :-

	write('[ Rotations ]'), nl,
	write('Top   : '), write( R1 ), nl,
	write('Bottom: '), write( R2 ), nl,
	write('Front : '), write( R3 ), nl,
	write('Back  : '), write( R4 ), nl,
	write('Left  : '), write( R5 ), nl,
	write('Right : '), write( R6 ), nl.

	
/******************************************************************
 * Converts a list of numbered characters, defined
 * with "", to its numeric value
 ******************************************************************/
 
string_to_list(String,ListOut):-!,
	string_to_list(String,[],ListOut).
string_to_list([H|T],ListIn,ListOut):-!,
	Val is H - 48, % o n�mero zero tem o c�digo ascii 48
	string_to_list(T,[Val|ListIn],ListOut).
string_to_list(_,List,List):-!.


/******************************************************************
 * Prints a 2D projection of the cube
 ******************************************************************/

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
	
	%                                   BACK
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
	
	%                                   FRONT
	w_side_space,                       w_top_bot_num(FF_C1),               w_cube_end, nl,
	w_side_space,                       w_empty_line,                       w_cube_end, nl,
	w_side_space,                       w_middle_nums('FRO', FF_C4, FF_C2), w_cube_end, nl,
	w_side_space,                       w_empty_line,                       w_cube_end, nl,
	w_side_space,                       w_bot_num_line(FF_C3),              w_cube_end, nl.
	

/******************************************************************
 * Processes an input file
 ******************************************************************/

 processStreamLine(Stream, ListIn, ListOut) :-
	at_end_of_line(Stream), !,
	skip_line(Stream),
	ListOut = ListIn.
processStreamLine(Stream, ListIn, ListOut) :-
	peek_code(Stream, Code),
	Code = -1, !,
	skip_line(Stream),
	ListOut = ListIn. 
processStreamLine(Stream, ListIn, ListOut) :-
	at_end_of_stream(Stream), !,
	ListOut = ListIn.
processStreamLine(Stream, ListIn, ListOut) :-
	get_code(Stream, Code),
	%write(Code), write(','),
	Number is Code - 48,
	Number >= 3,
	Number =< 9, !,
	processStreamLine(Stream, [Number|ListIn], ListOut).
processStreamLine(Stream,_,_):-!,
	write('Os valores aceites para as faces t�m de estar 3 e 9, inclusive.'), nl,
	close(Stream),
	abort.
	
parse_file(Filename, Top, Bottom, Front, Back, Left, Right) :-
	open(Filename, read, Stream),
	processStreamLine(Stream, [], Top),
	processStreamLine(Stream, [], Bottom),
	processStreamLine(Stream, [], Front),
	processStreamLine(Stream, [], Back),
	processStreamLine(Stream, [], Left),
	processStreamLine(Stream, [], Right),
	close(Stream).
	
	
/******************************************************************
 * Verifies if all list faces, have the same length
 ******************************************************************/
 
verifyLinesLength(Top, Bottom, Front, Back, Left, Right, ToElements):-
	length(Top   , ToElements),
	length(Bottom, L2),
	length(Front , L3),
	length(Back  , L4),
	length(Left  , L5),
	length(Right , L6),
	ToElements = L2,
	ToElements = L3,
	ToElements = L4,
	ToElements = L5,
	ToElements = L6, !.
verifyLinesLength(_,_,_,_,_,_,_):-!,
	write('Not all lines have the same with. Aborting.'), nl,
	abort.

	
/******************************************************************
 * Verifies if the length is a multiple of 4
 ******************************************************************/
 
verifyLineDistance(Size, Distance):-
	Size mod 4 =:= 0, !,
	Distance is floor(Size / 4).
verifyLineDistance(_, _):-!,
	write('The lenght of the lines is not a multiple of 4. Aborting.'), nl,
	abort.

	
/******************************************************************
 * turn12 processing
 ******************************************************************/
 
turn12p:-
	turn12('C:/Users/Jo�o Henriques/Desktop/Eng. Inform�tica/FEUP/PLOG/turn12/src/cubo_a.txt').
 
turn12(Filename):-

	write('Reading cube file...'),nl,
	parse_file(Filename, Top, Bottom, Front, Back, Left, Right), !,
	
	write('Attempting to solve cube...'),nl,
	turn12( Top,Bottom,Front,Back,Left,Right,    R1, R2, R3, R4, R5, R6,
			TT_C1,TT_C2,TT_C3,TT_C4,   BO_C1,BO_C2,BO_C3,BO_C4,    FF_C1,FF_C2,FF_C3,FF_C4,
			BA_C1,BA_C2,BA_C3,BA_C4,   LL_C1,LL_C2,LL_C3,LL_C4,    RR_C1,RR_C2,RR_C3,RR_C4   ),

	nl,	print_rots( R1,R2,R3,R4,R5,R6 ), nl,
	
	project_cube( TT_C1,TT_C2,TT_C3,TT_C4,   BA_C1,BA_C2,BA_C3,BA_C4,   RR_C1,RR_C2,RR_C3,RR_C4,
                  LL_C1,LL_C2,LL_C3,LL_C4,   FF_C1,FF_C2,FF_C3,FF_C4,   BO_C1,BO_C2,BO_C3,BO_C4 ),
	nl,nl,
	write('More possibilities ?'),
	1 = 2. % falha para ver se existem mais possibilidades

turn12( Top, Bottom, Front, Back, Left, Right,    R1, R2, R3, R4, R5, R6,
		TT_C1,TT_C2,TT_C3,TT_C4,   BO_C1,BO_C2,BO_C3,BO_C4,    FF_C1,FF_C2,FF_C3,FF_C4,
		BA_C1,BA_C2,BA_C3,BA_C4,   LL_C1,LL_C2,LL_C3,LL_C4,    RR_C1,RR_C2,RR_C3,RR_C4  ) :-
	
	verifyLinesLength(Top, Bottom, Front, Back, Left, Right, TotElems),
	verifyLineDistance( TotElems, Distance ),

	Rotations = [ R1, R2, R3, R4, R5, R6 ],

	domain(Rotations, 1, TotElems),

	
	% os labelings foram separados de forma a evitar que o predicado shifted_face
	% seja corrido sem ser necess�rio. Desta forma sempre que um shifted_face falha,
	% volta ao labeling anterior, e gera a rota��o da face, s� voltando aos labelings
	% anteriores assim que o mesmo esgota todas as possibilidades do dom�nio.
	
	% Face Topo com a face de Tr�s
	TT_C1 + BA_C3 #= 12,
	
	labeling([], [R1,R4]),
	shifted_face( Top   , R1, TotElems, Distance, TT_C1, TT_C2, TT_C3, TT_C4 ),
	shifted_face( Back  , R4, TotElems, Distance, BA_C1, BA_C2, BA_C3, BA_C4 ),
	
	
	% Face Topo, com Direira e Tr�s 
	TT_C2 + RR_C4 #= 12,
	RR_C1 + BA_C2 #= 12,
	
	labeling([], [R6]),
	shifted_face( Right , R6, TotElems, Distance, RR_C1, RR_C2, RR_C3, RR_C4 ),
	

	% Face Topo, com Esquerda e Tr�s
	TT_C4 + LL_C2 #= 12,
	LL_C1 + BA_C4 #= 12,
	
	labeling([], [R5]),
	shifted_face( Left  , R5, TotElems, Distance, LL_C1, LL_C2, LL_C3, LL_C4 ),

	
	% Face Topo, com Frente, Direita, Esquerda e Tr�s
	TT_C3 + FF_C1 #= 12,
	RR_C3 + FF_C2 #= 12,
	LL_C3 + FF_C4 #= 12,
	
	labeling([], [R3]),
	shifted_face( Front , R3, TotElems, Distance, FF_C1, FF_C2, FF_C3, FF_C4 ),


	% Face de Baixo com o pontos de contacto das faces adjecentes
	BO_C3 + FF_C3 #= 12,
	BO_C2 + LL_C4 #= 12,
	BO_C4 + RR_C2 #= 12,
	BO_C1 + BA_C1 #= 12,
	
	labeling([], [R2]),
	shifted_face( Bottom, R2, TotElems, Distance, BO_C1, BO_C2, BO_C3, BO_C4 ).

	

	
	
	
/******************************************************************
 ******************************************************************
 * Generating problems
 ******************************************************************
 ******************************************************************/
 
 
 %
/******************************************************************
 * Generate random number in the domain of the problem
 * [3, 10-1]
 ******************************************************************/
 
random_turn12_n(Number):-
	random(3, 10, Number).
	
	
/******************************************************************
 * Guarantees there is no adjecent repeated
 * numbers in each face
 ******************************************************************/
 
no_equal_number(N,N, Out) :-!,
	Out is 3 + ( ( ( N + 1 ) - 3 ) mod 7 ).
no_equal_number(_,N,N):-!.	


/******************************************************************
 * Guarantees there is not another not another pattern equal to
 * the original
 ******************************************************************/
 
no_equal_pattern( C1,C2,C3,C4, C1,C2,C3,C4, O4 ) :-!,
	no_equal_number(C4, C4, O4).
no_equal_pattern( _,_,_,_,  _,_,_,T4,  T4 ).


/******************************************************************
 * Fills the cube with random numbers in the domain of the problem
 ******************************************************************/

fill_cube_face_int(  _,_,_,_,  DistBetweenElems,DistBetweenElems,   A,B,C,D,   FaceOut ) :- !,
	append([], D, L1),
	append(L1, C, L2),
	append(L2, B, L3),
	append(L3, A, FaceOut).
fill_cube_face_int( C1,C2,C3,C4,  Pos,DistBetweenElems,   [HA|A],[HB|B],[HC|C],[HD|D],   FaceOut ) :-
	random_turn12_n( V1_temp ),
	no_equal_number( HA, V1_temp, V1 ),
		
	random_turn12_n( V2_temp ),
	no_equal_number( HB, V2_temp, V2 ),
	
	random_turn12_n( V3_temp ),
	no_equal_number( HC, V3_temp, V3 ),
	
	random_turn12_n( V4_temp ),
	no_equal_number( HD, V4_temp, V4_tt2 ),

	no_equal_pattern( C1,C2,C3,C4,  V1,V2,V3,V4_tt2, V4 ),
	
	NextPos is Pos + 1,
	fill_cube_face_int(  C1,C2,C3,C4,  NextPos,DistBetweenElems,   [V1|[HA|A]],[V2|[HB|B]],[V3|[HC|C]],[V4|[HD|D]],   FaceOut ).
	
fill_cube_face( C1,C2,C3,C4,  DistBetweenElems,  FaceOut ) :-
	fill_cube_face_int( C1,C2,C3,C4,  1,   DistBetweenElems,   [C1],[C2],[C3],[C4],   FaceOut ).	


/******************************************************************
 * Writes the cube face to the file Stream
 ******************************************************************/
 
write_cube_line([], _):-!.
write_cube_line([H|T], Stream) :-
	Char is H + 48, % ascii character 0 is 48
	put_code(Stream, Char), !,
	write_cube_line(T, Stream).
write_cube_line(_, Stream) :- !,
	write('Error writing in file. Aborting.'), nl,
	close(Stream),
	abort.
	
	
/******************************************************************
 * Writes the cube faces to file (one per line) in this order
 * Top, Bottom, Front, Back, Left, Right, 
 ******************************************************************/
 
write_cube_file(Filename, Top, Bottom, Front, Back, Left, Right) :-
	open(Filename, write, Stream), !,
	reverse(Top   ,Top_r   ),
	reverse(Bottom,Bottom_r),
	reverse(Front ,Front_r ),
	reverse(Back  ,Back_r  ),
	reverse(Left  ,Left_r  ),
	reverse(Right ,Right_r ),
	write_cube_line(Top_r   , Stream), nl(Stream),
	write_cube_line(Bottom_r, Stream), nl(Stream),
	write_cube_line(Front_r , Stream), nl(Stream),
	write_cube_line(Back_r  , Stream), nl(Stream),
	write_cube_line(Left_r  , Stream), nl(Stream),
	write_cube_line(Right_r , Stream),
	close(Stream).
	
	
/******************************************************************
 * Predicate that only accepts a cube with a unique solution
 * This is possible because the algorithm that solves the cube
 * starts with a shift of one, if no other solution is found,
 * the solution will have all six rotations with the length of
 * the face.
 ******************************************************************/
 
turn12_unique_gen( Top, Bottom, Front, Back, Left, Right ) :- !,
	turn12( Top, Bottom, Front, Back, Left, Right,    R1, R2, R3, R4, R5, R6,
		_,_,_,_,  _,_,_,_,  _,_,_,_,  _,_,_,_,  _,_,_,_,  _,_,_,_  ),
	!,	
	%print_rots(R1,R2,R3,R4,R5,R6),
	length( Top, TotalElem ),	
	R1 = TotalElem,
	R2 = TotalElem,
	R3 = TotalElem,
	R4 = TotalElem,
	R5 = TotalElem,
	R6 = TotalElem.


/******************************************************************
 * Rotates the face list, many times specified.
 ******************************************************************/
 
shuffle_cube_face( Face, ShufflePos, FaceOut ) :-
	shuffle_cube_face( Face, 0, ShufflePos, [], FaceOut ).
shuffle_cube_face( [H|T], Pos, ShufflePos, ListIn, FaceOut ) :-
	Pos < ShufflePos,
	append( ListIn, [H], NewList ),
	NewPos is Pos + 1,
	shuffle_cube_face( T, NewPos, ShufflePos, NewList, FaceOut ).
shuffle_cube_face( Tail, _, _, ListIn, FaceOut ) :-
	append( Tail, ListIn, FaceOut ).
	
	
/******************************************************************
 * Rotates the face with a random number
 ******************************************************************/
 
randomly_suffle_face( Face, FaceSize, FaceOut ) :-
	random(0, FaceSize, Rotation),
	shuffle_cube_face( Face, Rotation, FaceOut ).
	
	
/******************************************************************
 * Analytics predicates. Just check the sum of each line.
 * Not needed to solve the problem
 ******************************************************************/
 
sum_face([], Sum, Sum).
sum_face([H|T], SumIn, SumOut) :-
	NewSum is SumIn + H,
	sum_face(T, NewSum, SumOut).
	
print_face_stats(F,Sum,FSize):-
	Div is Sum / FSize,
	write('    '), write(F), write(' sum is: '), write( Sum ), write(' ('), write(Div), write(')'), nl.
	
print_cube_stats( Top, Bottom, Front, Back, Left, Right, FaceSize ) :-
	sum_face(Top, 0, SumTop),
	sum_face(Back, 0, SumBack),
	sum_face(Right, 0, SumRight),
	sum_face(Left, 0, SumLeft),
	sum_face(Front, 0, SumFront),
	sum_face(Bottom, 0, SumBottom),
	print_face_stats('Top', SumTop, FaceSize),
	print_face_stats('Back', SumBack, FaceSize),
	print_face_stats('Right', SumRight, FaceSize),
	print_face_stats('Left', SumLeft, FaceSize),
	print_face_stats('Front', SumFront, FaceSize),
	print_face_stats('Bottom', SumBottom, FaceSize),
	TotalSum is SumTop + SumBack + SumRight + SumLeft + SumFront + SumBottom,
	print_face_stats('Total', TotalSum, FaceSize).
	
	
/******************************************************************
 * Generates a new problem base on the arguments
 ******************************************************************/
 
turn12genp:-
	turn12gen( 'C:/Users/Jo�o Henriques/Desktop/Eng. Inform�tica/FEUP/PLOG/turn12/src/cubo_a.txt', 10 ).
	
turn12genp(Dist):-
	turn12gen( 'C:/Users/Jo�o Henriques/Desktop/Eng. Inform�tica/FEUP/PLOG/turn12/src/cubo_a.txt', Dist ).

turn12gen(Filename, Distance):-
	Distance > 0,
	
	repeat,
	write('Making an attempt to find if current generated random numbers can make a solution...'), nl,
	
	random_turn12_n( TT_C1 ),
	random_turn12_n( TT_C2 ),

	random_turn12_n( BO_C1 ),
	random_turn12_n( BO_C3 ),

	random_turn12_n( BA_C2 ),
	random_turn12_n( BA_C4 ),
	
	random_turn12_n( FF_C2 ),
	random_turn12_n( FF_C4 ),
	
	random_turn12_n( LL_C2 ),
	random_turn12_n( LL_C4 ),
	
	random_turn12_n( RR_C2 ),
	random_turn12_n( RR_C4 ),

	ContactPoints = [  TT_C2,TT_C4,    FF_C1,FF_C3,    LL_C2,LL_C4,
                       BO_C2,BO_C4,    BA_C1,BA_C3,    RR_C2,RR_C4   ],

	domain(ContactPoints, 3, 9),
	
	% t�m de ser diferentes, se n�o, ao girar a face 180�,
	% e se os cantos opostos forem os mesmos, originaria uma nova solu��o
	% se os quatro forem iguais, originariam 4 novas solu��es.
	TT_C1 #\= TT_C3   #\/   TT_C2 #\= TT_C4,
	BO_C1 #\= BO_C3   #\/   BO_C2 #\= BO_C4,
	
	FF_C1 #\= FF_C3   #\/   FF_C2 #\= FF_C4,
	BA_C1 #\= BA_C3   #\/   BA_C2 #\= BA_C4,
	RR_C1 #\= RR_C3   #\/   RR_C2 #\= RR_C4,
	LL_C1 #\= LL_C3   #\/   LL_C2 #\= LL_C4,

	TT_C1 + BA_C3 #= 12,
		
	TT_C2 + RR_C4 #= 12,
	RR_C1 + BA_C2 #= 12,
	
	TT_C4 + LL_C2 #= 12,
	LL_C1 + BA_C4 #= 12,

	TT_C3 + FF_C1 #= 12,
	RR_C3 + FF_C2 #= 12,
	LL_C3 + FF_C4 #= 12,

	BO_C3 + FF_C3 #= 12,
	BO_C2 + LL_C4 #= 12,
	BO_C4 + RR_C2 #= 12,
	BO_C1 + BA_C1 #= 12,
	
	labeling([], ContactPoints),
	
	repeat,
	
	write('Attempting to fill cube with a unique solution...'), nl,
	fill_cube_face( TT_C1,TT_C2,TT_C3,TT_C4, Distance, Top    ),
	fill_cube_face( BA_C1,BA_C2,BA_C3,BA_C4, Distance, Back   ),
	fill_cube_face( RR_C1,RR_C2,RR_C3,RR_C4, Distance, Right  ),
	fill_cube_face( LL_C1,LL_C2,LL_C3,LL_C4, Distance, Left   ),
	fill_cube_face( FF_C1,FF_C2,FF_C3,FF_C4, Distance, Front  ),
	fill_cube_face( BO_C1,BO_C2,BO_C3,BO_C4, Distance, Bottom ),
	
	FaceSize is Distance * 4,
	
	%print_cube_stats( Top, Bottom, Front, Back, Left, Right, FaceSize ),

	/*write(Top),nl,
	write(Bottom),nl,
	write(Front),nl,
	write(Back),nl,
	write(Left),nl,
	write(Right),nl,*/
	
	turn12_unique_gen( Top, Bottom, Front, Back, Left, Right ), !,
	write('Cube has a unique solution.'),nl,
	

	randomly_suffle_face( Top   , FaceSize, ShuffTop    ),
	randomly_suffle_face( Back  , FaceSize, ShuffBack   ),
	randomly_suffle_face( Right , FaceSize, ShuffRight  ),
	randomly_suffle_face( Left  , FaceSize, ShuffLeft   ),
	randomly_suffle_face( Front , FaceSize, ShuffFront  ),
	randomly_suffle_face( Bottom, FaceSize, ShuffBottom ),
	
	project_cube( TT_C1,TT_C2,TT_C3,TT_C4,   BA_C1,BA_C2,BA_C3,BA_C4,   RR_C1,RR_C2,RR_C3,RR_C4,
                  LL_C1,LL_C2,LL_C3,LL_C4,   FF_C1,FF_C2,FF_C3,FF_C4,   BO_C1,BO_C2,BO_C3,BO_C4 ),
				  
	write('Writing cube to file.'), nl,
	write_cube_file(Filename, ShuffTop, ShuffBottom, ShuffFront, ShuffBack, ShuffLeft, ShuffRight ).

turn12gen(_,_):-
	write('Distance between elements must be grater than 0. Aborting.').