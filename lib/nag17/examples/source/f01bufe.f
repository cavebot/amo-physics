*     F01BUF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, M1MAX, IA
      PARAMETER        (NMAX=20,M1MAX=8,IA=M1MAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, M, M1, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), W(M1MAX)
*     .. External Subroutines ..
      EXTERNAL         F01BUF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01BUF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M1
      IF (N.GT.0 .AND. N.LE.NMAX .AND. M1.GT.0 .AND. M1.LE.M1MAX) THEN
         READ (NIN,*) ((A(J,I),J=MAX(1,M1+1-I),M1),I=1,N)
         M = M1 - 1
         K = ((N+M)/(2*M))*M
         IFAIL = 0
*
         CALL F01BUF(N,M1,K,A,IA,W,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Computed upper triangular matrix'
         WRITE (NOUT,*)
         DO 20 I = 1, N
            WRITE (NOUT,99999) (A(J,I),J=MAX(1,M1+1-I),M1)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
      END
