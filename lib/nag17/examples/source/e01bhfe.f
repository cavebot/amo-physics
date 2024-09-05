*     E01BHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=50)
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, PINT
      INTEGER          IFAIL, N, R
*     .. Local Arrays ..
      DOUBLE PRECISION D(NMAX), F(NMAX), X(NMAX)
*     .. External Subroutines ..
      EXTERNAL         E01BHF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E01BHF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         DO 20 R = 1, N
            READ (NIN,*) X(R), F(R), D(R)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) '                               Integral'
         WRITE (NOUT,*) '          A            B     over (A,B)'
*        Read A, B pairs until end of file and compute
*        definite integrals
   40    READ (NIN,*,END=60) A, B
         IFAIL = 0
*
         CALL E01BHF(N,X,F,D,A,B,PINT,IFAIL)
*
         WRITE (NOUT,99999) A, B, PINT
         GO TO 40
      END IF
   60 STOP
*
99999 FORMAT (1X,3F13.4)
      END
