*     E01ABF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, N1MAX, N2MAX
      PARAMETER        (NMAX=10,N1MAX=2*NMAX,N2MAX=2*NMAX+1)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION P
      INTEGER          I, IFAIL, N, R
*     .. Local Arrays ..
      DOUBLE PRECISION A(N1MAX), G(N2MAX)
*     .. External Subroutines ..
      EXTERNAL         E01ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E01ABF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, P
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) (A(I),I=1,2*N)
         IFAIL = 0
*
         CALL E01ABF(N,P,A,G,2*N,2*N+1,IFAIL)
*
         WRITE (NOUT,*)
         DO 20 R = 0, N - 1
            WRITE (NOUT,99999) 'Central differences order ', R,
     +        ' of Y0 =', G(2*R+1)
            WRITE (NOUT,99998) '                               Y1 =',
     +        G(2*R+2)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Function value at interpolation point =',
     +     G(2*N+1)
      END IF
      STOP
*
99999 FORMAT (1X,A,I1,A,F12.5)
99998 FORMAT (1X,A,F12.5)
      END
