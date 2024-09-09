*     G01AJF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N
      PARAMETER        (N=50)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION XMAX, XMIN, XSTEP
      INTEGER          I, IFAIL, ISPACE, ITYPE, MULTY, N1, NSTEPX,
     +                 NSTEPY
*     .. Local Arrays ..
      DOUBLE PRECISION X(N)
*     .. External Subroutines ..
      EXTERNAL         G01AJF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01AJF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) (X(I),I=1,N)
      WRITE (NOUT,*)
      NSTEPX = 10
      NSTEPY = 10
      ITYPE = 0
      ISPACE = 10
      XMIN = 0.0D0
      XMAX = 0.0D0
      CALL X04ABF(1,NOUT)
      IFAIL = 0
*
      CALL G01AJF(X,N,NSTEPX,NSTEPY,ITYPE,ISPACE,XMIN,XMAX,XSTEP,N1,
     +            MULTY,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      ITYPE = 1
      ISPACE = 0
      XMIN = -10.0D0
      XMAX = 5.0D0
      IFAIL = 0
*
      CALL G01AJF(X,N,NSTEPX,NSTEPY,ITYPE,ISPACE,XMIN,XMAX,XSTEP,N1,
     +            MULTY,IFAIL)
*
      STOP
      END
