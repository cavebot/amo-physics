*     G08CDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=100,MMAX=50)
*     .. Local Arrays ..
      DOUBLE PRECISION SX(NMAX), SY(MMAX), X(NMAX), Y(MMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION D, P, Z
      INTEGER          IFAIL, M, N, NTYPE
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05FAF, G08CDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08CDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M
      WRITE (NOUT,*)
      IF (N.LE.NMAX .AND. M.LE.MMAX) THEN
         CALL G05CBF(0)
         CALL G05FAF(0.0D0,2.0D0,N,X)
         CALL G05FAF(0.25D0,2.25D0,M,Y)
         READ (NIN,*) NTYPE
         IFAIL = -1
*
         CALL G08CDF(N,X,M,Y,NTYPE,D,Z,P,SX,SY,IFAIL)
*
         IF (IFAIL.NE.0) WRITE (NOUT,99999) '** IFAIL = ', IFAIL
         WRITE (NOUT,99998) 'Test statistic D = ', D
         WRITE (NOUT,99998) 'Z statistic      = ', Z
         WRITE (NOUT,99998) 'Tail probability = ', P
      ELSE
         WRITE (NOUT,99997) 'N or M is out of range: N = ', N,
     +     ' and M = ', M
      END IF
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,A,F8.4)
99997 FORMAT (1X,A,I7,A,I7)
      END
