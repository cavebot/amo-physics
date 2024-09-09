*     D01GAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=21)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ANS, ERROR
      INTEGER          I, IFAIL, N
*     .. Local Arrays ..
      DOUBLE PRECISION X(NMAX), Y(NMAX)
*     .. External Subroutines ..
      EXTERNAL         D01GAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01GAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LE.NMAX) THEN
         READ (NIN,*) (X(I),Y(I),I=1,N)
         IFAIL = 1
*
         CALL D01GAF(X,Y,N,ANS,ERROR,IFAIL)
*
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,99999) 'Integral = ', ANS,
     +        '     Estimated error = ', ERROR
         ELSE IF (IFAIL.EQ.1) THEN
            WRITE (NOUT,*) 'Less than 4 points supplied'
         ELSE IF (IFAIL.EQ.2) THEN
            WRITE (NOUT,*)
     +        'Points not in increasing or decreasing order'
         ELSE IF (IFAIL.EQ.3) THEN
            WRITE (NOUT,*) 'Points not all distinct'
         END IF
      ELSE
         WRITE (NOUT,*) 'More than NMAX data points'
      END IF
      STOP
*
99999 FORMAT (1X,A,F7.4,A,F7.4)
      END
