F11JDF Example Program Data
  7                   N
 16                   NNZ
 'CG'                 METHOD
 'P' 'N' 'I' 1        PRECON, SIGCMP, NORM, ITERM
 1.0D-6 100           TOL, MAXITN
 0.0D0 0.0D0          ANORM, SIGMAX
 0.0D0 10             SIGTOL, MAXITS
 1.0D0                OMEGA
  4.   1    1
  1.   2    1
  5.   2    2
  2.   3    3
  2.   4    2
  3.   4    4
 -1.   5    1
  1.   5    4
  4.   5    5
  1.   6    2
 -2.   6    5
  3.   6    6
  2.   7    1
 -1.   7    2
 -2.   7    3
  5.   7    7         A(I), IROW(I), ICOL(I), I=1,...,NNZ
 15.  18.  -8.   21.
 11.  10.   29.       B(I), I=1,...,N
  0.   0.   0.    0.
  0.   0.   0.        X(I), I=1,...,N
