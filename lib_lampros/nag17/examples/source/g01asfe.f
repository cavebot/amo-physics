*     G01ASF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDX
      PARAMETER        (NMAX=5,LDX=10)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, LDP, M, NSTEPX, NSTEPY
*     .. Local Arrays ..
      DOUBLE PRECISION WORK(5*NMAX), X(LDX,NMAX)
      INTEGER          IWORK(LDX), N(NMAX)
      CHARACTER        PLOT(60,132)
*     .. External Subroutines ..
      EXTERNAL         G01ASF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01ASF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Set advisory message unit for plot output to NOUT
      CALL X04ABF(1,NOUT)
      READ (NIN,*) M, (N(I),I=1,M)
      READ (NIN,*) NSTEPX, NSTEPY
      DO 20 J = 1, M
         READ (NIN,*) (X(I,J),I=1,N(J))
   20 CONTINUE
      LDP = NSTEPY
      IFAIL = 0
      WRITE (NOUT,*)
*
      CALL G01ASF('Print',M,N,X,LDX,NSTEPX,NSTEPY,PLOT,LDP,WORK,IWORK,
     +            IFAIL)
*
      STOP
      END
