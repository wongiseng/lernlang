-module(prob).
-compile(export_all).

% 1. Last element
myLast([X])   -> X;
myLast([_|T]) -> myLast(T).

% 2. Second last element
mySecondLast([X,_]) -> X;
mySecondLast([_|T]) -> mySecondLast(T).

% 3. Element at (I made it 1 based not 0 based, for later use
element_at(L,1)       -> hd(L);
element_at([_|T],Pos) -> element_at(T,Pos-1).

% 4. a. Length list
len([])    -> 0;
len([_|T]) -> 1+len(T).

% 4. b. Length list with fold
lenf(L) -> lists:foldl(fun(_,Len)->1+Len end, 0, L).

% 5. a. Reverse list

rev([])    -> [];
rev([H|T]) -> rev(T)++[H].

% 5 b. Reverse with tail rec

revtail(L) -> revt(L,[]).

revt([],L) -> L;
revt([H|T], L) -> revt(T, [H|L]).


% 6. Is palindrome

ispalin(L) -> L =:= revtail(L).

% 7. Flatten list 

flatten([])    -> [];
flatten([H|T]) -> flatten(H) ++ flatten(T);
flatten(X)     -> [X]. % handling elements

% 8. Eliminate consecutive duplicates

compress([])      ->[];
compress([H,H|T]) -> compress([H|T]);
compress([H|T])   -> [H|compress(T)].

% 9. a. Pack consecutive duplicates into separate list
% If a list contains repeated element they should be placed in separate sublists

pack([])    -> [];
pack([H|T]) -> pack_dup(H, [H], T).

pack_dup(Dup, CurDup, [Dup|T]) -> pack_dup(Dup, [Dup|CurDup], T);
pack_dup(_,   CurDup,       T) -> [CurDup | pack(T)].

% 9. b. Same case using splitwith

pack_with([])          -> [];
pack_with([H|T]) -> {Dup, Rest} = lists:splitwith(fun(X)->X=:=H end, [H|T]),
                    [Dup |  pack_with(Rest)].

% 10. Run length encoding, similar to previous but now encode the length

encode([])      -> [];
encode([H|T])   -> {Dup, Rest} = lists:splitwith(fun(X)->X=:=H end, [H|T]),
                   [[length(Dup), H] |  encode(Rest)].

% With list comprehension
encode_lc(L)        -> [ [length(X), hd(X)] || X <- pack(L)].

% 11. Run length encoding without encoding unique elements, jeez i was
% making this for no 10 and had deleted it 

encode_mod([])           -> [];
encode_mod([H,H|T])      -> {Dup, Rest} = lists:splitwith(fun(X)->X=:=H end, [H,H|T]),
                              [[length(Dup), H] |  encode_mod(Rest)];
encode_mod([H|T])        -> [H | encode_mod(T)].


% With list comprehension and previous pack function
encode_mod_lc(L)        -> [ if length(X) > 1 -> [length(X), hd(X)]; true -> hd(X) end || X <- pack(L)].

% 12. Decode run length encoding from last

decode([])    -> [];
decode([H|T]) when is_list(H) 
              -> [Len, X] = H, rep(Len, X)++decode(T);
decode([H|T]) -> [H | decode(T)].

% Replicate helper, we'll use it also in other task
rep(1,X)    -> [X];
rep(Len, X) -> [X|rep(Len-1, X)].

% 13. Direct solution of run length encoding as no 11 without creating
% sub lists ? What da ya mean.


enc_direct([])      -> [];
enc_direct([H,H|T]) -> enc_dup(H, 2, T);
enc_direct([H|T])   -> [H|enc_direct(T)].

enc_dup(H, Len, [H|T]) -> enc_dup(H, Len+1, T);
enc_dup(H, Len, T)     -> [[Len,H]]++enc_direct(T).

% 14. Duplicate elements of list

dup_basic([])    -> [];
dup_basic([H|T]) -> [H,H]++dup_basic(T).

% with list comprehension and our flatten
dup_flat(L) -> flatten([ [X,X] || X<-L]).

% with foldl
dup_foldl(L) -> lists:foldl(fun(X,Acc) -> Acc++[X,X] end, [], L).

% with foldr note the fun is different, we're going from right

dup_foldr(L) -> lists:foldr(fun(X,Acc) -> [X,X]++Acc end, [], L).

% with flatmap

dup_flatmap(L) -> lists:flatmap(fun(X) -> [X,X] end, L).

% 15. Replicate a number of times

rep_flatmap(L, Rep) -> lists:flatmap(fun(X) -> rep(Rep, X) end, L).

% meh, basically just replace all the fun in no 14 with rep.


% 16. Drop every n'th element

drop(L, Nth) -> drop_nth(L, 1, Nth, []).

drop_nth([],_,_,Res) -> Res;
drop_nth([_|T], Nth, Nth, Res) % I can use remainder but this is cleaner, when Idx = Nth drop it
                    -> drop_nth(T, 1, Nth, Res);
drop_nth([H|T], Idx, Nth, Res)  
                    -> drop_nth(T, Idx+1, Nth, Res++[H]).

% 17. Split into two parts, length of first part is given

split(L, Len) -> split(L, [],  Len).

split(L, Left,  0)      -> [Left, L];
split([H|T], Left, Len) -> split(T, Left++[H], Len-1).

% 18. Given two indices, i and k, the slice is the list containing the
% elements between the i'th and k'th element of the original list (both
% limits included). Start counting the elements with 1. 

% I am lazy, i will just use the split that I made :P
slice_sp(L, Start, End) -> [_, Slice] = split(L, Start-1),
                           [Result,_] = split(Slice, End-Start+1),
                           Result.

% Slice in a standard recursive way

slice(L, Start, End)  -> slice(L, Start, End, []).

slice(_, 1, 0 , Res)          -> lists:reverse(Res);
slice([H|T], 1, End, Res)     -> slice(T, 1, End-1, [H|Res]);
slice([_|T], Start, End, Res) -> slice(T, Start-1, End-1, Res).


% 19. Rotate a list N places to the left

rotate_sp(L, N) when N > 0 -> [Left, Right] = split(L, N),
                                 Right ++ Left;
rotate_sp(L, N)            -> [Left, Right] = split(L, length(L)+N),
                                 Right ++ Left.

% 20. Remove at

% I know this is not tail recursive but I get the idea, and I am sleepy
remove_at([_|T], 1)        -> T;
remove_at([H|T], N)        -> [H|remove_at(T,N-1)].

% 21. Insert at

insert_at(X, L,  1)        -> [X|L];
insert_at(X, [H|T],  P)        -> [H|insert_at(X, T,  P-1)].

% 22. List containing integer at given range

% Just use lists:seq
range( End,  End) -> [End];
range(Start, End) -> [Start|range(Start+1, End)].

% Tail rec, with accum. Look ma, no reverse
range_acc(Start,End)              -> range_a(Start, End, []).
range_a(Start, Start, Accum) -> [Start|Accum];
range_a(Start, End, Accum)   -> range_a(Start, End-1, [End|Accum]).

% 23. Select N elements from lists randomly
random_select(_, 0) -> [];
random_select(List, N) -> Index = random:uniform(length(List)),
                          [element_at(List, Index) | random_select(remove_at(List, Index), N-1)].

% 24. Draw N different random number from the set 1..M

draw_random(N, M) -> random_select(range(1,M), N).

% 25. Generate random permutation of N elements

random_permutation(N) -> draw_random(N,N).

% 26. Generate combinations of K distinct objects from N elements of lists 

combination(_, [])    -> [];
combination(1, List)  -> [ [X]   || X <- List];
combination(N, [H|T]) -> [ [H|X] || X <- combination(N-1, T)] ++ combination(N, T).

% wrong attempt, this one produce combination noticing order
% combination(1, List ) -> [ [X]   || X <- List ];
% combination(N, List ) -> [ [X|Y] || X <- List, Y <- combination(N-1, List -- [X])].

% 27. a. Grouping 9 people in 3 disjoint subgroup of 2,3,4 persons

group3_234(L) -> group3(L, [], [], []).
group3([H|T], Two, Three, Four) ->  if length(Two)   < 2 -> group3(T, [H|Two], Three, Four);
                                                  true   -> []
                                    end ++
                                    if length(Three) < 3 -> group3(T, Two, [H|Three], Four);
                                                      true   -> []
                                    end ++
                                    if length(Four)  < 4 -> group3(T, Two, Three, [H|Four]);
                                                      true   -> []
                                    end;
group3([], Two, Three, Four)         -> [[Two, Three, Four]].

% 27. b. An generic version, group list L according to list of lengths in Lengths
%
% Using rep we initialize results accumulator as [[],[] ...] as many as length(Lengths).
group(L, Lengths) -> SumLen = lists:sum(Lengths), 
                     % some check assuring sum of length is equal to list length
                     if SumLen =:= length(L) -> group(L, rep(length(Lengths), []), Lengths);
                             true                      -> error 
                     end.

group(   [], Results, _)       -> [Results];

group([H|T], Results, Lengths) -> lists:flatmap(fun(Next) -> group(T, Next, Lengths) end,  place(H, Results, Lengths)).

% Place H on all possible places in Results, whenever it does not exceed lengths
place(H, Results, Lengths)  -> expand(H, [], Results, Lengths).

expand(H, Head, [T|Tail], [L|Lens]) when length(T) < L ->
        % To keep corresponding element in head with lengths, Head need to be reversed (keeping its order)
        [lists:reverse(Head)++[[H|T]|Tail] | expand(H, [T|Head], Tail, Lens)];

expand(H, Head, [T|Tail], [_|Lens]) ->
        expand(H, [T|Head], Tail, Lens);

expand(_, _, [], _) -> [].

% 28. a. Sort list according to length of sublist

% Rather cryptic one liner 
lsort(L) -> lists:map(fun(LTuple)->element(2,LTuple) end, lists:keysort(1,lists:map(fun(X)->{length(X),X} end, L))).

% The same solution in more readable fashion 
l_sort(L) -> % Convert into {Len, L} Len, List tuple 
             LTupleList      = lists:map(fun(X)->{length(X),X} end, L),

             % Sort tuple according to first key
             SortedTupleList = lists:keysort(1, LTupleList),

             % Extract second element of tuple
             lists:map(fun(LTuple) -> element(2, LTuple) end, SortedTupleList). 



% 28. b. Sort list according to frequency of sublist length
fsort(L) -> % Get lenghts of sublists and sorted it 
            LengthsSorted = lists:sort(lists:map(fun(X)->length(X) end, L)),

            % Using previous assignments, getting [Freq, Len]
            FreqLen       = encode(LengthsSorted), 

            % Reverse to make length as key, and freq as value
            LenFreq          = lists:map(fun([Freq, Len]) -> {Len, Freq} end, FreqLen),

            % Now map again original list to its frequency
            FreqTuple          = lists:map(fun(Elem) -> Len = length(Elem),
                                                       {value, {Len, Freq}} = lists:keysearch(Len, 1, LenFreq),
                                                   {Freq, Elem} end, L),
            % Sort the freq tuple
            Sorted          = lists:keysort(1, FreqTuple),

            % Extract second element of tuple
            lists:map(fun(LTuple) -> element(2, LTuple) end, Sorted). 

% 31. Is prime
is_prime(X) when X < 2 -> false;
is_prime(2) -> true;
is_prime(X) -> Divisors = lists:seq(2, round(math:sqrt(X))),
               lists:foldl(fun(Div, PrevCheck) -> (X rem Div =/= 0) and PrevCheck end,  true, Divisors).

% 32. GCD ?
gcd(A, 0) -> A;
gcd(A, B) when A < B -> gcd(B,A);
gcd(A, B) -> gcd(B, (A rem B)).


% 33. Coprime

is_coprime(A,B) -> gcd(A,B) =:= 1.

% 34. Euler's totient function, primitive version, manually counting
% how many X among 1 .. M is coprime with M

totient_phi(M)  -> length([X || X<-lists:seq(1,M), is_coprime(X,M)]).

% 35. Prime factors

prime_factors(X) when X <  3 -> [X];
prime_factors(X) -> Divisors = lists:seq(2, round(math:sqrt(X))),
                    Factors  = [ D || D<-Divisors, X rem D =:= 0],
                    if Factors =:= [] -> [X];
                       true              -> [hd(Factors) | prime_factors(X div hd(Factors))]
                    end.

% 36. Prime factor with multiplicity, Lazy guy reuse pack
prime_factors_mult(X) -> Factors = prime_factors(X),
                         [{hd(F), length(F)} || F <- pack(Factors)]. 

% 37. Back to totient using 36 with more efficient implementation
totient_phi_power(X) ->  Factors = prime_factors_mult(X),
                         lists:foldl(fun({P,M}, Acc) -> (P-1)*round(math:pow(P,M-1))*Acc end, 1, Factors).

% 38. Compare the two totient functions we implemented using timer:tc
% with increasing X

compare_phi_functions() ->  io:format("~10s ~15s ~15s ~n", ["X", "phi(X)", "phi_power(X)"]),
                            io:format("~s ~n", ["  ========================================"]),
                          [ io:format("~10w ~15w ~15w ~n", [X, element(1,timer:tc(prob, totient_phi, [X])), 
                                                                 element(1,timer:tc(prob, totient_phi_power, [X]))]) 
                              || X <- [ round(math:pow(2, Y)) || Y<- lists:seq(1,20)]], ok.

% Sample result of comparison :
%         X          phi(X)    phi_power(X) 
%  ======================================== 
%         2               3               4 
%         4               4               5 
%         8               5               5 
%        16               7               6 
%        32              15               7 
%        64              30               8 
%       128              59               9 
%       256             120              11 
%       512             263              13 
%      1024             562              17 
%      2048            1187              21 
%      4096            2516              33 
%      8192            5317              45 
%     16384           11162              56 
%     32768           23343              71 
%     65536           47926              92 
%    131072          100166             134 
%    262144          221902             192 
%    524288          454542             266 
%   1048576          968516             321 
%ok                            


%39. Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

%example :
%prob:prime_range(10,20).
%[11,13,17,19]

prime_range(Lo, Hi) -> [X ||  X<-lists:seq(Lo, Hi), is_prime(X) ].

% Generate list of primes first with sieve until Hi, and then filtered
% out the one less than Lo

sieve(X) when integer(X) -> sieve(lists:seq(2, X));
sieve([H|T]) 		 -> [H|sieve( lists:filter(fun(X) -> X rem H /= 0 end, T))];
sieve([])    		 -> [].

prime_range_sieve(Lo, Hi) -> lists:filter(fun(X) -> X>=Lo end, sieve(Hi)).

% Sieve version turns out to be slower after X > 8192. Before that its faster
%[ {X, element(1,timer:tc(prob, prime_range, [1,X])), element(1,timer:tc(prob, prime_range_sieve, [1,X]))} || X<-[ round(math:pow(2, Y)) || Y<- lists:seq(1,16)] ].
%[{2,4,4},
% {4,7,4},
% {8,9,7},
% {16,20,12},
% {32,47,29},
% {64,121,71},
% {128,343,173},
% {256,863,453},
% {512,2381,1497},
% {1024,6718,4141},
% {2048,17755,13974},
% {4096,42781,31949},
% {8192,113940,102521},
% {16384,297081,367245},
% {32768,817112,1166379},
% {65536,2250948,3942746}]


%40. (**) Goldbach's conjecture. Goldbach's conjecture says that every
%positive even number greater than 2 is the sum of two prime numbers.
%Example: 28 = 5 + 23. It is one of the most famous facts in number
%theory that has not been proved to be correct in the general case. It
%has been numerically confirmed up to very large numbers (much larger
%than we can go with our Prolog system). Write a predicate to find the
%two prime numbers that sum up to a given even integer. 


goldbach(4) -> {2,2};	      % special case i don't want to deal with
goldbach(X) when X < 3 -> []; % less than equal 2 ignore, outside conjecture
goldbach(X) -> OddTillHalfX = lists:seq(3, (X div 2) + 1, 2),
               AllPairs     = [ {Y, X-Y} || Y <- OddTillHalfX, is_prime(Y), is_prime(X-Y)],
	       hd(AllPairs). 
        
%41. (**) Given a range of integers by its lower and upper limit, print a
%list of all even numbers and their Goldbach composition.
%
%In most cases, if an even number is written as the sum of two prime
%numbers, one of them is very small. Very rarely, the primes are both
%bigger than say 50. Try to find out how many such cases there are in the
%range 2..3000. 
%
%Example:
%prob:goldbach_list(9, 20)
%10 = 3 + 7
%12 = 5 + 7
%14 = 3 + 11
%16 = 3 + 13
%18 = 5 + 13
%20 = 3 + 17

goldbach_list(Lo, Hi) ->  goldbach_list(Lo, Hi, 1).

% Find cases where primes are both bigger than Min 

goldbach_list(Lo, Hi, Min) -> Start = if Lo rem 2 =:= 1 -> Lo+1; true-> Lo end,
			 Results = [goldbach(X) || X <- lists:seq(Start, Hi, 2)],
			 [ io:format("~w = ~w + ~w ~n", [X+Y,X,Y] ) || {X, Y} <- Results, X> Min, Y> Min],
			 ok.


% 49. Gray codes :
% An n-bit Gray code is a sequence of n-bit strings constructed
% according to certain rules. For example,
%
% n = 1: C(1) = ['0','1'].
% n = 2: C(2) = ['00','01','11','10'].
% n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
%
% Find out the construction rules and write a predicate with the
% following specification:
%
% % gray(N,C) :- C is the N-bit Gray code
%
% Can you apply the method of "result caching" in order to make the
% predicate more efficient, when it is to be used repeatedly?
%
% Example in Haskell:
% gray 3
% ["000","001","011","010","110","111","101","100"]

% Text book gray code prepending 0 to gray(n-1) and 1 to reverse of gray(n-1)
gray(0)   ->   [[]];
gray(N)	  ->   Prev = gray(N-1),
	       [[$0|X] || X <- Prev] ++ [[$1|X] || X <- lists:reverse(Prev)].

% My bad_gray code, i see different pattern, what I do was
% duplicating gray(n-1) and convolute/padding with 0,1,1,0

bad_gray(1) -> ["0","1"];
bad_gray(N) -> Dup = dup_basic(bad_gray(N-1)),
	       convolute(Dup, ["0","1","1","0"]).

convolute(A, B) -> convolute(A,B,[],B).

convolute([],_,Res,_) -> lists:reverse(Res);
convolute([HA|TA],[HB|TB],Res,B) -> convolute(TA, TB, [HA++HB|Res], B);
convolute(A, [], Res, B)	 -> convolute(A, B, Res, B).
