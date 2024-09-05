*     G08CBF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MAXP
      PARAMETER        (NMAX=30,MAXP=2)
*     .. Local Arrays ..
      DOUBLE PRECISION PAR(MAXP), SX(NMAX), X(NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION D, P, Z
      INTEGER          I, IFAIL, N, NP, NTYPE
*     .. External Subroutines ..
      EXTERNAL         G08CBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08CBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LE.NMAX) THEN
         READ (NIN,*) (X(I),I=1,N)
         READ (NIN,*) NP, (PAR(I),I=1,NP), NTYPE
         IFAIL = 0
*
         CALL G08CBF(N,X,'Uniform',PAR,'Supplied',NTYPE,D,Z,P,SX,IFAIL)
*
         WRITE (NOUT,*) 'Test against uniform distribution on (0,2)'
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Test statistic D = ', D
         WRITE (NOUT,99999) 'Z statistic      = ', Z
         WRITE (NOUT,99999) 'Tail probability = ', P
         WRITE (NOUT,*)
*
         READ (NIN,*) NP, (PAR(I),I=1,NP), NTYPE
         IFAIL = 0
*
         CALL G08CBF(N,X,'Normal',PAR,'Estimate',NTYPE,D,Z,P,SX,IFAIL)
*
         WRITE (NOUT,*)
     +'Test against Normal distribution with parameters estimated from t
     +he data'
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Mean = ', PAR(1), '  and variance = ',
     +     PAR(2)
         WRITE (NOUT,99999) 'Test statistic D = ', D
         WRITE (NOUT,99999) 'Z statistic      = ', Z
         WRITE (NOUT,99999) 'Tail probability = ', P
      ELSE
         WRITE (NOUT,99997) 'N is out of range: N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,F8.4)
99998 FORMAT (1X,A,F6.4,A,F6.4)
99997 FORMAT (1X,A,I7)
      END
