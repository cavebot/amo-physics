*     G01FFF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, P, X
      INTEGER          IFAIL
*     .. External Functions ..
      DOUBLE PRECISION G01FFF
      EXTERNAL         G01FFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01FFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   P       A       B           X'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) P, A, B
      IFAIL = -1
*
      X = G01FFF(P,A,B,0.0D0,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) P, A, B, X
      ELSE
         WRITE (NOUT,99999) P, A, B, X, ' NOTE: IFAIL = ', IFAIL
      END IF
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,3F8.3,F10.3,A,I1)
      END
