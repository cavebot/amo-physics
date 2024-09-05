*     G10BAF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          N, NS
      PARAMETER        (N=1000,NS=100)
*     .. Local Scalars ..
      DOUBLE PRECISION SHI, SLO, WINDOW
      INTEGER          IFAIL, NSTEPX, NSTEPY
      LOGICAL          USEFFT
*     .. Local Arrays ..
      DOUBLE PRECISION FFT(NS), S(NS), SMOOTH(NS), X(N)
      INTEGER          ISORT(NS)
*     .. External Subroutines ..
      EXTERNAL         G01AGF, G05FDF, G10BAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G10BAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) WINDOW
      READ (NIN,*) SLO, SHI
*
*     Generate Normal (0,1) Distribution
*
      CALL G05FDF(0.0D0,1.0D0,N,X)
*
*     Perform kernel density estimation
*
      USEFFT = .FALSE.
      IFAIL = 0
*
      CALL G10BAF(N,X,WINDOW,SLO,SHI,NS,SMOOTH,S,USEFFT,FFT,IFAIL)
*
*     Display smoothed data
*
      WRITE (NOUT,*)
      NSTEPX = 40
      NSTEPY = 20
      IFAIL = 0
*
      CALL G01AGF(S,SMOOTH,NS,ISORT,NSTEPX,NSTEPY,IFAIL)
      STOP
*
      END
