*     C06FRF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=5,NMAX=20)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION TRIG(2*NMAX), WORK(2*MMAX*NMAX), X(MMAX*NMAX),
     +                 Y(MMAX*NMAX)
*     .. External Subroutines ..
      EXTERNAL         C06FRF, C06GCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C06FRF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=120) M, N
      IF (M.LE.MMAX .AND. N.LE.NMAX) THEN
         DO 40 J = 1, M
            READ (NIN,*) (X(I*M+J),I=0,N-1)
            READ (NIN,*) (Y(I*M+J),I=0,N-1)
   40    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Original data values'
         DO 60 J = 1, M
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Real ', (X(I*M+J),I=0,N-1)
            WRITE (NOUT,99999) 'Imag ', (Y(I*M+J),I=0,N-1)
   60    CONTINUE
         IFAIL = 0
*
         CALL C06FRF(M,N,X,Y,'Initial',TRIG,WORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Discrete Fourier transforms'
         DO 80 J = 1, M
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Real ', (X(I*M+J),I=0,N-1)
            WRITE (NOUT,99999) 'Imag ', (Y(I*M+J),I=0,N-1)
   80    CONTINUE
*
         CALL C06GCF(Y,M*N,IFAIL)
         CALL C06FRF(M,N,X,Y,'Subsequent',TRIG,WORK,IFAIL)
         CALL C06GCF(Y,M*N,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Original data as restored by inverse transform'
         DO 100 J = 1, M
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Real ', (X(I*M+J),I=0,N-1)
            WRITE (NOUT,99999) 'Imag ', (Y(I*M+J),I=0,N-1)
  100    CONTINUE
         GO TO 20
      ELSE
         WRITE (NOUT,*) 'Invalid value of M or N'
      END IF
  120 STOP
*
99999 FORMAT (1X,A,6F10.4)
      END
