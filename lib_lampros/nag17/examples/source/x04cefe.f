*     X04CEF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NMAX, LDA
      PARAMETER        (NMAX=5,LDA=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX)
*     .. External Subroutines ..
      EXTERNAL         X04CEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04CEF Example Program Results'
      WRITE (NOUT,*)
*
*     Generate an array of data
      DO 40 J = 1, NMAX
         DO 20 I = 1, LDA
            A(I,J) = 10*I + J
   20    CONTINUE
   40 CONTINUE
*
      IFAIL = 0
*
*     Print 5 by 5 band matrix with 1 sub-diagonal and 1 super-diagonal
      CALL X04CEF(5,5,1,1,A,LDA,'Band Matrix:',IFAIL)
*
      STOP
      END
