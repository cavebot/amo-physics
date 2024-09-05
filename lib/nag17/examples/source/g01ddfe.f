*     G01DDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=20)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION PW, W
      INTEGER          I, IFAIL, J, N
      LOGICAL          CALWTS
*     .. Local Arrays ..
      DOUBLE PRECISION A(NMAX), X(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G01DDF, M01CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01DDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      CALWTS = .TRUE.
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         DO 20 J = 1, 2
            READ (NIN,*) (X(I),I=1,N)
            IFAIL = 0
*
            CALL M01CAF(X,1,N,'A',IFAIL)
*
            IFAIL = 0
*
            CALL G01DDF(X,N,CALWTS,A,W,PW,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'For sample number ', J,
     +        ', value of W statistic = ', W
            WRITE (NOUT,99998)
     +        '                     Significance level is ', PW
            CALWTS = .FALSE.
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I1,A,F7.4)
99998 FORMAT (1X,A,F8.4)
      END
