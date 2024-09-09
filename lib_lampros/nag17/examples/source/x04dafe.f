*     X04DAF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NMAX, LDA
      PARAMETER        (NMAX=4,LDA=NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION AA
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX)
*     .. External Subroutines ..
      EXTERNAL         X04DAF
*     .. Intrinsic Functions ..
      INTRINSIC        DCMPLX
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04DAF Example Program Results'
      WRITE (NOUT,*)
*     Generate an array of data
      DO 40 J = 1, NMAX
         DO 20 I = 1, LDA
            AA = 10*I + J
            A(I,J) = DCMPLX(AA,-AA)
   20    CONTINUE
   40 CONTINUE
*
      IFAIL = 0
*
*     Print 4 by 3 rectangular matrix
      CALL X04DAF('General',' ',4,3,A,LDA,'Example 1:',IFAIL)
*
      WRITE (NOUT,*)
*
*     Print 4 by 4 lower triangular matrix
      CALL X04DAF('Lower','Non-unit',4,4,A,LDA,'Example 2:',IFAIL)
*
      STOP
      END
