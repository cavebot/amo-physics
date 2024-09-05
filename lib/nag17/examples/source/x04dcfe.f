*     X04DCF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NMAX, LA
      PARAMETER        (NMAX=4,LA=(NMAX*(NMAX+1))/2)
*     .. Local Scalars ..
      DOUBLE PRECISION AA
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      COMPLEX*16       A(LA)
*     .. External Subroutines ..
      EXTERNAL         X04DCF
*     .. Intrinsic Functions ..
      INTRINSIC        DCMPLX
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04DCF Example Program Results'
      WRITE (NOUT,*)
*
*     Generate an array of data
      DO 20 I = 1, LA
         AA = I
         A(I) = DCMPLX(AA,-AA)
   20 CONTINUE
*
      IFAIL = 0
*
*     Print order 3 lower triangular matrix
      CALL X04DCF('Lower','Unit',3,A,'Example 1:',IFAIL)
*
      WRITE (NOUT,*)
*
*     Print order 4 upper triangular matrix
      CALL X04DCF('Upper','Non-unit',4,A,'Example 2:',IFAIL)
*
      STOP
      END
