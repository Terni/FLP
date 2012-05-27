MODULE Fibonacci.
IMPORT Integers.
 
PREDICATE Fib : Integer * Integer.
 
Fib(0,0).
Fib(1,1).
Fib(k,n) <-
	k > 1 &
	FibIt(k-2,1,1,n).
 
PREDICATE FibIt : Integer * Integer * Integer * Integer.
 
FibIt(0,_,g,g).
FibIt(k,f,g,n) <-
	k > 0 &
	g < n &
	FibIt(k-1,g,f+g,n).

##########################################################

MODULE PRIME.
IMPORT Integers.
 
PREDICATE Prime : Integer.
 
Prime(prime) <- ~ SOME [x] (x > 1 & x < prime & prime Mod x = 0).


##########################################################

MODULE      GCD.
IMPORT      Integers.
 
PREDICATE   Gcd : Integer * Integer * Integer.
Gcd(i,j,d) <- 
           CommonDivisor(i,j,d) &
           ~ SOME [e] (CommonDivisor(i,j,e) & e > d).
 
PREDICATE   CommonDivisor : Integer * Integer * Integer.
CommonDivisor(i,j,d) <-
           IF (i = 0 \/ j = 0)
           THEN
             d = Max(Abs(i),Abs(j))
           ELSE
             1 =< d =< Min(Abs(i),Abs(j)) &
             i Mod d = 0 &
             j Mod d = 0.
