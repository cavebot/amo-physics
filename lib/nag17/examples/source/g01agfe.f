*     G01AGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOBS
      PARAMETER        (NOBS=48)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, NSTEPX, NSTEPY
*     .. Local Arrays ..
      DOUBLE PRECISION X(NOBS), Y(NOBS)
      INTEGER          ISORT(NOBS)
*     .. External Subroutines ..
      EXTERNAL         G01AGF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01AGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  '            Plot of wheat (X) and potato (Y) yields'
      WRITE (NOUT,*) '            in 48 counties in England in 1936.'
      WRITE (NOUT,*)
      READ (NIN,*) (X(I),Y(I),I=1,NOBS)
      CALL X04ABF(1,NOUT)
      IFAIL = 0
      NSTEPX = 40
      NSTEPY = 32
*
      CALL G01AGF(X,Y,NOBS,ISORT,NSTEPX,NSTEPY,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'G01AGF fails unexpectedly with IFAIL =',
     +     IFAIL
      END IF
      STOP
*
99999 FORMAT (1X,A,I2)
      END
