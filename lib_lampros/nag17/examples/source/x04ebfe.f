*     X04EBF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NMAX, LDA
      PARAMETER        (NMAX=5,LDA=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INDENT, J, NCOLS
*     .. Local Arrays ..
      INTEGER          A(LDA,NMAX)
      CHARACTER*7      CLABS(NMAX), RLABS(NMAX)
*     .. External Subroutines ..
      EXTERNAL         X04EBF
*     .. Data statements ..
      DATA             CLABS/'Un', 'Deux', 'Trois', 'Quatre', 'Cinq'/
      DATA             RLABS/'Uno', 'Duo', 'Tre', 'Quattro', 'Cinque'/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04EBF Example Program Results'
      WRITE (NOUT,*)
*     Generate an array of data
      DO 40 J = 1, NMAX
         DO 20 I = 1, LDA
            A(I,J) = 10*I + J
   20    CONTINUE
   40 CONTINUE
      NCOLS = 80
      INDENT = 0
      IFAIL = 0
*
*     Print 3 by 5 rectangular matrix with default format and integer
*     row and column labels
      CALL X04EBF('General',' ',3,5,A,LDA,' ','Example 1:','Integer',
     +            RLABS,'Integer',CLABS,NCOLS,INDENT,IFAIL)
*
      WRITE (NOUT,*)
*
*     Print 5 by 5 upper triangular matrix with user-supplied format
*     and row and column labels
      CALL X04EBF('Upper','Non-unit',5,5,A,LDA,'I8','Example 2:',
     +            'Character',RLABS,'Character',CLABS,NCOLS,INDENT,
     +            IFAIL)
*
      STOP
      END
