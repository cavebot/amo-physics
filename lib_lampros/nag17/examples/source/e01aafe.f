*     E01AAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, N1MAX, N2MAX
      PARAMETER        (NMAX=9,N1MAX=NMAX+1,N2MAX=NMAX*N1MAX/2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION X
      INTEGER          I, J, K, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(N1MAX), B(N1MAX), C(N2MAX)
*     .. External Subroutines ..
      EXTERNAL         E01AAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E01AAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, X
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) (A(I),I=1,N+1)
         READ (NIN,*) (B(I),I=1,N+1)
*
         CALL E01AAF(A,B,C,N+1,N*(N+1)/2,N,X)
*
         K = 1
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Interpolated values'
         DO 20 I = 1, N - 1
            WRITE (NOUT,99999) (C(J),J=K,K+N-I)
            K = K + N - I + 1
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Interpolation point = ', X
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Function value at interpolation point = ',
     +     C(N*(N+1)/2)
      END IF
      STOP
*
99999 FORMAT (1X,6F12.5)
99998 FORMAT (1X,A,F12.5)
      END
