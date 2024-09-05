*     C06FAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=20)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          IFAIL, J, N, N2, NJ
*     .. Local Arrays ..
      DOUBLE PRECISION A(0:NMAX-1), B(0:NMAX-1), WORK(NMAX),
     +                 X(0:NMAX-1), XX(0:NMAX-1)
*     .. External Subroutines ..
      EXTERNAL         C06FAF, C06FBF, C06GBF
*     .. Intrinsic Functions ..
      INTRINSIC        MOD
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C06FAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=120) N
      IF (N.GT.1 .AND. N.LE.NMAX) THEN
         DO 40 J = 0, N - 1
            READ (NIN,*) X(J)
            XX(J) = X(J)
   40    CONTINUE
         IFAIL = 0
*
         CALL C06FAF(X,N,WORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Components of discrete Fourier transform'
         WRITE (NOUT,*)
         WRITE (NOUT,*) '          Real      Imag'
         WRITE (NOUT,*)
         A(0) = X(0)
         B(0) = 0.0D0
         N2 = (N-1)/2
         DO 60 J = 1, N2
            NJ = N - J
            A(J) = X(J)
            A(NJ) = X(J)
            B(J) = X(NJ)
            B(NJ) = -X(NJ)
   60    CONTINUE
         IF (MOD(N,2).EQ.0) THEN
            A(N2+1) = X(N2+1)
            B(N2+1) = 0.0D0
         END IF
         DO 80 J = 0, N - 1
            WRITE (NOUT,99999) J, A(J), B(J)
   80    CONTINUE
*
         CALL C06GBF(X,N,IFAIL)
         CALL C06FBF(X,N,WORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'Original sequence as restored by inverse transform'
         WRITE (NOUT,*)
         WRITE (NOUT,*) '        Original  Restored'
         WRITE (NOUT,*)
         DO 100 J = 0, N - 1
            WRITE (NOUT,99999) J, XX(J), X(J)
  100    CONTINUE
         GO TO 20
      ELSE
         WRITE (NOUT,*) 'Invalid value of N'
      END IF
  120 STOP
*
99999 FORMAT (1X,I5,2F10.5)
      END
