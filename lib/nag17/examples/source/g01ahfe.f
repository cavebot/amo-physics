*     G01AHF Example Program Text
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NOBS, LWORK
      PARAMETER        (NOBS=25,LWORK=5*NOBS/2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION XBAR, XSTD
      INTEGER          I, IFAIL, ISTAND, NSTEPX, NSTEPY
*     .. Local Arrays ..
      DOUBLE PRECISION WORK(LWORK), X(NOBS), XSORT(NOBS)
      INTEGER          IWORK(NOBS)
*     .. External Subroutines ..
      EXTERNAL         G01AHF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01AHF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) (X(I),I=1,NOBS)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '25 data values to be plotted'
      WRITE (NOUT,99997) (X(I),I=1,NOBS)
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  'Plot of normal scores (Y) against standardised residuals (X)'
      WRITE (NOUT,*)
      CALL X04ABF(1,NOUT)
      NSTEPX = 50
      NSTEPY = 40
      ISTAND = 1
      IFAIL = 1
*
      CALL G01AHF(X,NOBS,NSTEPX,NSTEPY,ISTAND,IWORK,WORK,LWORK,XSORT,
     +            XBAR,XSTD,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Failed in G01AHF. IFAIL = ', IFAIL
      ELSE
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Mean of data values = ', XBAR
         WRITE (NOUT,99998) 'Standard deviation  = ', XSTD
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Sorted standardised data values'
         WRITE (NOUT,99997) (XSORT(I),I=1,NOBS)
      END IF
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,A,F5.2)
99997 FORMAT (5X,5F7.2)
      END
