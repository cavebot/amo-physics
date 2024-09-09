*     F04MFF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=100)
*     .. Local Scalars ..
      DOUBLE PRECISION P
      INTEGER          I, IFAIL, K, N
*     .. Local Arrays ..
      DOUBLE PRECISION B(NMAX), T(0:NMAX-1), WORK(2*NMAX-1), X(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F04MFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04MFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF ((N.LT.0) .OR. (N.GT.NMAX)) THEN
         WRITE (NOUT,99999) 'N is out of range. N = ', N
      ELSE
         READ (NIN,*) (T(I),I=0,N-1)
         READ (NIN,*) (B(I),I=1,N)
*
         DO 20 K = 1, N
*
            IFAIL = 0
*
            CALL F04MFF(K,T,B,X,P,WORK,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Solution for system of order', K
            WRITE (NOUT,99998) (X(I),I=1,K)
            IF (K.GT.1) THEN
               WRITE (NOUT,*) 'Reflection coefficient'
               WRITE (NOUT,99998) P
            END IF
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,5F9.4)
      END
