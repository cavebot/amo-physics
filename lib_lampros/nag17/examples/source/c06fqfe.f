*     C06FQF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=5,NMAX=20)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION TRIG(2*NMAX), U(MMAX*NMAX), V(MMAX*NMAX),
     +                 WORK(2*NMAX*MMAX), X(MMAX*NMAX)
*     .. External Subroutines ..
      EXTERNAL         C06FPF, C06FQF, C06GQF, C06GSF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C06FQF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=140) M, N
      IF (M.LE.MMAX .AND. N.LE.NMAX) THEN
         DO 40 J = 1, M
            READ (NIN,*) (X(I*M+J),I=0,N-1)
   40    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Original data values'
         WRITE (NOUT,*)
         DO 60 J = 1, M
            WRITE (NOUT,99999) '     ', (X(I*M+J),I=0,N-1)
   60    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Original data written in full complex form'
         IFAIL = 0
*
         CALL C06GSF(M,N,X,U,V,IFAIL)
*
         DO 80 J = 1, M
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Real ', (U(I*M+J),I=0,N-1)
            WRITE (NOUT,99999) 'Imag ', (V(I*M+J),I=0,N-1)
   80    CONTINUE
*
         CALL C06FQF(M,N,X,'Initial',TRIG,WORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Discrete Fourier transforms (real values)'
         WRITE (NOUT,*)
         DO 100 J = 1, M
            WRITE (NOUT,99999) '     ', (X(I*M+J),I=0,N-1)
  100    CONTINUE
*
         CALL C06FPF(M,N,X,'Subsequent',TRIG,WORK,IFAIL)
         CALL C06GQF(M,N,X,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Original data as restored by inverse transform'
         WRITE (NOUT,*)
         DO 120 J = 1, M
            WRITE (NOUT,99999) '     ', (X(I*M+J),I=0,N-1)
  120    CONTINUE
         GO TO 20
      ELSE
         WRITE (NOUT,*) 'Invalid value of M or N'
      END IF
  140 STOP
*
99999 FORMAT (1X,A,6F10.4)
      END
