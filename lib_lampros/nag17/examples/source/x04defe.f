*     X04DEF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NMAX, LDA
      PARAMETER        (NMAX=5,LDA=NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION AA
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX)
*     .. External Subroutines ..
      EXTERNAL         X04DEF
*     .. Intrinsic Functions ..
      INTRINSIC        DCMPLX
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04DEF Example Program Results'
      WRITE (NOUT,*)
*
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
*     Print 5 by 5 band matrix with 1 sub-diagonal and 1 super-diagonal
      CALL X04DEF(5,5,1,1,A,LDA,'Band Matrix:',IFAIL)
*
      STOP
      END
