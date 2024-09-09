*     X04CDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NMAX, LA
      PARAMETER        (NMAX=5,LA=(NMAX*(NMAX+1))/2)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INDENT, NCOLS
*     .. Local Arrays ..
      DOUBLE PRECISION A(LA)
      CHARACTER*7      CLABS(NMAX), RLABS(NMAX)
*     .. External Subroutines ..
      EXTERNAL         X04CDF
*     .. Data statements ..
      DATA             CLABS/'Un', 'Deux', 'Trois', 'Quatre', 'Cinq'/
      DATA             RLABS/'Uno', 'Duo', 'Tre', 'Quattro', 'Cinque'/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04CDF Example Program Results'
      WRITE (NOUT,*)
*
*     Generate an array of data
      DO 20 I = 1, LA
         A(I) = I
   20 CONTINUE
*
      NCOLS = 80
      INDENT = 0
      IFAIL = 0
*
*     Print order 4 lower triangular matrix with default format and
*     integer row and column labels
      CALL X04CDF('Lower','Non-unit',4,A,' ','Example 1:','Integer',
     +            RLABS,'Integer',CLABS,NCOLS,INDENT,IFAIL)
*
      WRITE (NOUT,*)
*
*     Print order 5 upper triangular matrix with user-supplied format
*     and row and column labels
      CALL X04CDF('Upper','Unit',5,A,'F8.2','Example 2:','Character',
     +            RLABS,'Character',CLABS,NCOLS,INDENT,IFAIL)
*
      STOP
      END
