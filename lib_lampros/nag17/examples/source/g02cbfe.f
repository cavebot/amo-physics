*     G02CBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N
      PARAMETER        (N=8)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION RESULT(20), X(N), Y(N)
*     .. External Subroutines ..
      EXTERNAL         G02CBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02CBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) (X(I),Y(I),I=1,N)
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' Case     Independent     Dependent'
      WRITE (NOUT,*) 'number     variable       variable'
      WRITE (NOUT,*)
      WRITE (NOUT,99999) (I,X(I),Y(I),I=1,N)
      WRITE (NOUT,*)
      IFAIL = 1
*
      CALL G02CBF(N,X,Y,RESULT,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99998) 'Routine fails, IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,99997)
     +     'Mean of independent variable               = ', RESULT(1)
         WRITE (NOUT,99997)
     +     'Mean of   dependent variable               = ', RESULT(2)
         WRITE (NOUT,99997)
     +     'Standard deviation of independent variable = ', RESULT(3)
         WRITE (NOUT,99997)
     +     'Standard deviation of   dependent variable = ', RESULT(4)
         WRITE (NOUT,99997)
     +     'Correlation coefficient                    = ', RESULT(5)
         WRITE (NOUT,*)
         WRITE (NOUT,99997)
     +     'Regression coefficient                     = ', RESULT(6)
         WRITE (NOUT,99997)
     +     'Standard error of coefficient              = ', RESULT(8)
         WRITE (NOUT,99997)
     +     't-value for coefficient                    = ', RESULT(10)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Analysis of regression table :-'
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +'      Source        Sum of squares  D.F.    Mean square     F-val
     +ue'
         WRITE (NOUT,*)
         WRITE (NOUT,99996) 'Due to regression', (RESULT(I),I=12,15)
         WRITE (NOUT,99996) 'About  regression', (RESULT(I),I=16,18)
         WRITE (NOUT,99996) 'Total            ', (RESULT(I),I=19,20)
      END IF
      STOP
*
99999 FORMAT (1X,I4,2F15.4)
99998 FORMAT (1X,A,I2)
99997 FORMAT (1X,A,F8.4)
99996 FORMAT (1X,A,F14.4,F8.0,2F14.4)
      END
