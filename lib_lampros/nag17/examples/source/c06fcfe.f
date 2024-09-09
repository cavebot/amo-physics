*     C06FCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=20)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION WORK(NMAX), X(0:NMAX-1), XX(0:NMAX-1),
     +                 Y(0:NMAX-1), YY(0:NMAX-1)
*     .. External Subroutines ..
      EXTERNAL         C06FCF, C06GCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C06FCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=100) N
      IF (N.GT.1 .AND. N.LE.NMAX) THEN
         DO 40 J = 0, N - 1
            READ (NIN,*) X(J), Y(J)
            XX(J) = X(J)
            YY(J) = Y(J)
   40    CONTINUE
         IFAIL = 0
*
         CALL C06FCF(X,Y,N,WORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Components of discrete Fourier transform'
         WRITE (NOUT,*)
         WRITE (NOUT,*) '          Real      Imag'
         WRITE (NOUT,*)
         DO 60 J = 0, N - 1
            WRITE (NOUT,99999) J, X(J), Y(J)
   60    CONTINUE
*
         CALL C06GCF(Y,N,IFAIL)
         CALL C06FCF(X,Y,N,WORK,IFAIL)
         CALL C06GCF(Y,N,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'Original sequence as restored by inverse transform'
         WRITE (NOUT,*)
         WRITE (NOUT,*) '             Original                 Restored'
         WRITE (NOUT,*)
     +     '          Real      Imag           Real      Imag'
         WRITE (NOUT,*)
         DO 80 J = 0, N - 1
            WRITE (NOUT,99999) J, XX(J), YY(J), X(J), Y(J)
   80    CONTINUE
         GO TO 20
      ELSE
         WRITE (NOUT,*) 'Invalid value of N'
      END IF
  100 STOP
*
99999 FORMAT (1X,I5,2F10.5,5X,2F10.5)
      END
