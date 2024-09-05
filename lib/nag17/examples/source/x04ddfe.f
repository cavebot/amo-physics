*     X04DDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NMAX, LA
      PARAMETER        (NMAX=4,LA=(NMAX*(NMAX+1))/2)
*     .. Local Scalars ..
      DOUBLE PRECISION AA
      INTEGER          I, IFAIL, INDENT, NCOLS
      CHARACTER*19     FORMAT
*     .. Local Arrays ..
      COMPLEX*16       A(LA)
      CHARACTER*7      CLABS(NMAX), RLABS(NMAX)
*     .. External Subroutines ..
      EXTERNAL         X04DDF
*     .. Intrinsic Functions ..
      INTRINSIC        DCMPLX
*     .. Data statements ..
      DATA             CLABS/'Un', 'Deux', 'Trois', 'Quatre'/
      DATA             RLABS/'Uno', 'Duo', 'Tre', 'Quattro'/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04DDF Example Program Results'
      WRITE (NOUT,*)
*
*     Generate an array of data
      DO 20 I = 1, LA
         AA = I
         A(I) = DCMPLX(AA,-AA)
   20 CONTINUE
*
      NCOLS = 80
      INDENT = 0
      IFAIL = 0
      FORMAT = ' '
*
*     Print order 4 lower triangular matrix with default format and
*     integer row and column labels, and bracketed complex elements
      CALL X04DDF('Lower','Non-unit',4,A,'Bracketed',FORMAT,
     +            'Example 1:','Integer',RLABS,'Integer',CLABS,NCOLS,
     +            INDENT,IFAIL)
*
      WRITE (NOUT,*)
      FORMAT = 'SS,F7.1,SP,F6.1,''i'''
*
*     Print order 4 upper triangular matrix with user-supplied format
*     and row and column labels, using the supplied format directly
      CALL X04DDF('Upper','Unit',4,A,'Direct',FORMAT,'Example 2:',
     +            'Character',RLABS,'Character',CLABS,NCOLS,INDENT,
     +            IFAIL)
*
      STOP
      END
