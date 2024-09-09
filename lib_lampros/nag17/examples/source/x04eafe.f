*     X04EAF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NMAX, LDA
      PARAMETER        (NMAX=5,LDA=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      INTEGER          A(LDA,NMAX)
*     .. External Subroutines ..
      EXTERNAL         X04EAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04EAF Example Program Results'
      WRITE (NOUT,*)
*     Generate an array of data
      DO 40 J = 1, NMAX
         DO 20 I = 1, LDA
            A(I,J) = 10*I + J
   20    CONTINUE
   40 CONTINUE
*
      IFAIL = 0
*
*     Print 3 by 5 rectangular matrix
      CALL X04EAF('General',' ',3,5,A,LDA,'Example 1:',IFAIL)
*
      WRITE (NOUT,*)
*
*     Print 5 by 5 lower triangular matrix
      CALL X04EAF('Lower','Non-unit',5,5,A,LDA,'Example 2:',IFAIL)
*
      STOP
      END
