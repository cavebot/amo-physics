*     X04CFF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NMAX, LDA
      PARAMETER        (NMAX=5,LDA=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INDENT, J, NCOLS
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,LDA)
      CHARACTER*7      CLABS(NMAX), RLABS(NMAX)
*     .. External Subroutines ..
      EXTERNAL         X04CFF
*     .. Data statements ..
      DATA             CLABS/'Un', 'Deux', 'Trois', 'Quatre', 'Cinq'/
      DATA             RLABS/'Uno', 'Duo', 'Tre', 'Quattro', 'Cinque'/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04CFF Example Program Results'
      WRITE (NOUT,*)
*
*     Generate an array of data
      DO 40 J = 1, NMAX
         DO 20 I = 1, LDA
            A(I,J) = 10*I + J
   20    CONTINUE
   40 CONTINUE
*
      NCOLS = 80
      INDENT = 0
      IFAIL = 0
*
*     Print 5 by 5 band matrix with 1 sub-diagonal, 1 super-diagonal,
*     default format and integer row and column labels
      CALL X04CFF(5,5,1,1,A,LDA,' ','Example 1:','Integer',RLABS,
     +            'Integer',CLABS,NCOLS,INDENT,IFAIL)
*
      WRITE (NOUT,*)
*
*     Print 5 by 5 band matrix with 1 sub-diagonal, 2 super-diagonals,
*     user-supplied format and row and column labels
      CALL X04CFF(5,5,1,2,A,LDA,'F8.2','Example 2:','Character',RLABS,
     +            'Character',CLABS,NCOLS,INDENT,IFAIL)
*
      STOP
      END
