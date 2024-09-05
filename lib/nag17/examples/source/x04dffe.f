*     X04DFF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NMAX, LDA
      PARAMETER        (NMAX=5,LDA=NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION AA
      INTEGER          I, IFAIL, INDENT, J, NCOLS
      CHARACTER*19     FORMAT
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,LDA)
      CHARACTER*7      CLABS(NMAX), RLABS(NMAX)
*     .. External Subroutines ..
      EXTERNAL         X04DFF
*     .. Intrinsic Functions ..
      INTRINSIC        DCMPLX
*     .. Data statements ..
      DATA             CLABS/'Un', 'Deux', 'Trois', 'Quatre', 'Cinq'/
      DATA             RLABS/'Uno', 'Duo', 'Tre', 'Quattro', 'Cinque'/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04DFF Example Program Results'
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
      NCOLS = 80
      INDENT = 0
      IFAIL = 0
      FORMAT = ' '
*
*     Print 5 by 5 band matrix with 1 sub-diagonal, 1 super-diagonal,
*     default format, bracketed complex numbers, and integer row and
*     column labels
      CALL X04DFF(5,5,1,1,A,LDA,'Bracketed',FORMAT,'Example 1:',
     +            'Integer',RLABS,'Integer',CLABS,NCOLS,INDENT,IFAIL)
*
      WRITE (NOUT,*)
      FORMAT = 'SS,F7.1,SP,F6.1,''i'''
*
*     Print 4 by 4 band matrix with 1 sub-diagonal, 2 super-diagonals,
*     user-supplied format and row and column labels
      CALL X04DFF(4,4,1,2,A,LDA,'Direct',FORMAT,'Example 2:',
     +            'Character',RLABS,'Character',CLABS,NCOLS,INDENT,
     +            IFAIL)
*
      STOP
      END
