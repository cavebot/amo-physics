*     X04DBF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NMAX, LDA
      PARAMETER        (NMAX=4,LDA=NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION AA
      INTEGER          I, IFAIL, INDENT, J, NCOLS
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX)
      CHARACTER*7      CLABS(NMAX), RLABS(NMAX)
*     .. External Subroutines ..
      EXTERNAL         X04DBF
*     .. Intrinsic Functions ..
      INTRINSIC        DCMPLX
*     .. Data statements ..
      DATA             CLABS/'Un', 'Deux', 'Trois', 'Quatre'/
      DATA             RLABS/'Uno', 'Duo', 'Tre', 'Quattro'/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04DBF Example Program Results'
      WRITE (NOUT,*)
*     Generate an array of data
      DO 40 J = 1, NMAX
         DO 20 I = 1, LDA
            AA = 10*I + J
            A(I,J) = DCMPLX(AA,-AA)
   20    CONTINUE
   40 CONTINUE
      NCOLS = 80
      INDENT = 0
      IFAIL = 0
*
*     Print 3 by 4 rectangular matrix with default format and integer
*     row and column labels, and bracketed complex elements
      CALL X04DBF('General',' ',3,4,A,LDA,'Bracketed',' ','Example 1:',
     +            'Integer',RLABS,'Integer',CLABS,NCOLS,INDENT,IFAIL)
*
      WRITE (NOUT,*)
*
*     Print 4 by 4 upper triangular matrix with user-supplied format
*     and row and column labels, and complex elements with real part
*     above imaginary part
      CALL X04DBF('Upper','Non-unit',4,4,A,LDA,'Above','F8.2',
     +            'Example 2:','Character',RLABS,'Character',CLABS,
     +            NCOLS,INDENT,IFAIL)
*
      STOP
      END
