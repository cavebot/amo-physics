*     G08CCF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=30)
*     .. Local Arrays ..
      DOUBLE PRECISION SX(NMAX), X(NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION D, P, Z
      INTEGER          I, IFAIL, N, NTYPE
*     .. External Functions ..
      DOUBLE PRECISION CDF1, CDF2
      EXTERNAL         CDF1, CDF2
*     .. External Subroutines ..
      EXTERNAL         G08CCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08CCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LE.NMAX) THEN
         READ (NIN,*) (X(I),I=1,N)
         READ (NIN,*) NTYPE
         IFAIL = 0
*
         CALL G08CCF(N,X,CDF1,NTYPE,D,Z,P,SX,IFAIL)
*
         WRITE (NOUT,*) 'Test against uniform distribution on (0,2)'
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Test statistic D = ', D
         WRITE (NOUT,99999) 'Z statistic      = ', Z
         WRITE (NOUT,99999) 'Tail probability = ', P
*
         CALL G08CCF(N,X,CDF2,NTYPE,D,Z,P,SX,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'Test against normal distribution with mean = 0.75'
         WRITE (NOUT,*) 'and standard deviation = 0.5.'
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Test statistic D = ', D
         WRITE (NOUT,99999) 'Z statistic      = ', Z
         WRITE (NOUT,99999) 'Tail probability = ', P
      ELSE
         WRITE (NOUT,99998) 'N is out of range: N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,F8.4)
99998 FORMAT (1X,A,I7)
      END
*
      DOUBLE PRECISION FUNCTION CDF1(X)
*     .. Parameters ..
      DOUBLE PRECISION               A, B
      PARAMETER                      (A=0.0D0,B=2.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION               X
*     .. Executable Statements ..
      IF (X.LT.A) THEN
         CDF1 = 0.0D0
      ELSE IF (X.GT.B) THEN
         CDF1 = 1.0D0
      ELSE
         CDF1 = (X-A)/(B-A)
      END IF
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION CDF2(X)
*     .. Parameters ..
      DOUBLE PRECISION               XMEAN, STD
      PARAMETER                      (XMEAN=0.75D0,STD=0.5D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION               X
*     .. Local Scalars ..
      DOUBLE PRECISION               Z
      INTEGER                        IFAIL
*     .. External Functions ..
      DOUBLE PRECISION               S15ABF
      EXTERNAL                       S15ABF
*     .. Executable Statements ..
      Z = (X-XMEAN)/STD
      CDF2 = S15ABF(Z,IFAIL)
      RETURN
      END
