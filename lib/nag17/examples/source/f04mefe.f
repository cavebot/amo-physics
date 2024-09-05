*     F04MEF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=100)
*     .. Local Scalars ..
      DOUBLE PRECISION V
      INTEGER          I, IFAIL, K, N
*     .. Local Arrays ..
      DOUBLE PRECISION T(0:NMAX), WORK(NMAX-1), X(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F04MEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04MEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF ((N.LT.0) .OR. (N.GT.NMAX)) THEN
         WRITE (NOUT,99999) 'N is out of range. N = ', N
      ELSE
         READ (NIN,*) (T(I),I=0,N)
*
         DO 20 K = 1, N
*
            IFAIL = 0
*
            CALL F04MEF(K,T,X,V,WORK,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Solution for system of order', K
            WRITE (NOUT,99998) (X(I),I=1,K)
            WRITE (NOUT,*) 'Mean square prediction error'
            WRITE (NOUT,99998) V
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,5F9.4)
      END
