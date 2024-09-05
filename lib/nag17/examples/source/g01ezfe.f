*     G01EZF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION D, PROB
      INTEGER          IFAIL, N1, N2
*     .. External Functions ..
      DOUBLE PRECISION G01EZF
      EXTERNAL         G01EZF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01EZF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    D      N1    N2    Two-sided probability'
      WRITE (NOUT,*)
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=40) N1, N2, D
      IFAIL = -1
*
      PROB = G01EZF(N1,N2,D,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) D, N1, N2, PROB
      ELSE
         WRITE (NOUT,99998) D, N1, N2, PROB, ' *IFAIL = ', IFAIL
      END IF
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,F7.4,2X,I4,2X,I4,10X,F7.4)
99998 FORMAT (1X,F7.4,2X,I4,2X,I4,10X,F7.4,A,I2)
      END
