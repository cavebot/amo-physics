*     E02BBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NC7MAX
      PARAMETER        (NC7MAX=200)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, S, X
      INTEGER          IFAIL, J, M, NCAP, R
*     .. Local Arrays ..
      DOUBLE PRECISION C(NC7MAX), LAMDA(NC7MAX)
*     .. External Subroutines ..
      EXTERNAL         E02BBF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02BBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=80) M
      IF (M.GT.0) THEN
         READ (NIN,*) NCAP
         IF (NCAP+7.LE.NC7MAX) THEN
            READ (NIN,*) (LAMDA(J),J=1,NCAP+7)
            READ (NIN,*) (C(J),J=1,NCAP+3)
            A = LAMDA(4)
            B = LAMDA(NCAP+4)
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        '  J       LAMDA(J)    B-spline coefficient (J-2)'
            WRITE (NOUT,*)
            DO 40 J = 1, NCAP + 7
               IF (J.LT.3 .OR. J.GT.NCAP+5) THEN
                  WRITE (NOUT,99999) J, LAMDA(J)
               ELSE
                  WRITE (NOUT,99999) J, LAMDA(J), C(J-2)
               END IF
   40       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        '  R       Argument      Value of cubic spline'
            WRITE (NOUT,*)
            DO 60 R = 1, M
               X = (DBLE(M-R)*A+DBLE(R-1)*B)/DBLE(M-1)
               IFAIL = 0
*
               CALL E02BBF(NCAP+7,LAMDA,C,X,S,IFAIL)
*
               WRITE (NOUT,99999) R, X, S
   60       CONTINUE
            GO TO 20
         END IF
      END IF
   80 STOP
*
99999 FORMAT (1X,I3,F14.4,F21.4)
      END
