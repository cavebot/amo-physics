*     C05AXF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION F, SCALE, TOL, X
      INTEGER          I, IFAIL, IND, IR
*     .. Local Arrays ..
      DOUBLE PRECISION C(26)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         C05AXF
*     .. Intrinsic Functions ..
      INTRINSIC        EXP, SQRT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C05AXF Example Program Results'
      SCALE = SQRT(X02AJF())
      IR = 0
      DO 40 I = 3, 4
         TOL = 10.0D0**(-I)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'TOL = ', TOL
         WRITE (NOUT,*)
         X = 1.0D0
         IFAIL = 1
         IND = 1
*
   20    CALL C05AXF(X,F,TOL,IR,SCALE,C,IND,IFAIL)
*
         IF (IND.NE.0) THEN
            F = X - EXP(-X)
            GO TO 20
         ELSE
            IF (IFAIL.GT.0) THEN
               WRITE (NOUT,99998) 'Error exit, IFAIL =', IFAIL
               IF (IFAIL.EQ.4 .OR. IFAIL.EQ.6) THEN
                  WRITE (NOUT,99997) 'Final value = ', X, ' THETA = ',
     +              C(5), ' LAMBDA = ', C(7)
               END IF
            ELSE
               WRITE (NOUT,99997) 'Root is ', X
            END IF
         END IF
   40 CONTINUE
      STOP
*
99999 FORMAT (1X,A,D10.4)
99998 FORMAT (1X,A,I2)
99997 FORMAT (1X,A,F14.5,A,F10.2,A,F10.2)
      END
