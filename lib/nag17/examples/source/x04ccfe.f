*     X04CCF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NMAX, LA
      PARAMETER        (NMAX=5,LA=(NMAX*(NMAX+1))/2)
*     .. Local Scalars ..
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LA)
*     .. External Subroutines ..
      EXTERNAL         X04CCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04CCF Example Program Results'
      WRITE (NOUT,*)
*
*     Generate an array of data
      DO 20 I = 1, LA
         A(I) = I
   20 CONTINUE
*
      IFAIL = 0
*
*     Print order 4 lower triangular matrix
      CALL X04CCF('Lower','Unit',4,A,'Example 1:',IFAIL)
*
      WRITE (NOUT,*)
*
*     Print order 5 upper triangular matrix
      CALL X04CCF('Upper','Non-unit',5,A,'Example 2:',IFAIL)
*
      STOP
      END
