*     E02BAF Example Program Text
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          MMAX, NC7MAX
      PARAMETER        (MMAX=200,NC7MAX=50)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION FIT, SS, XARG
      INTEGER          IFAIL, IWGHT, J, M, NCAP, R
*     .. Local Arrays ..
      DOUBLE PRECISION C(NC7MAX), LAMDA(NC7MAX), W(MMAX), WORK1(MMAX),
     +                 WORK2(4*NC7MAX), X(MMAX), Y(MMAX)
*     .. External Subroutines ..
      EXTERNAL         E02BAF, E02BBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02BAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=100) M
      IF (M.GT.0 .AND. M.LE.MMAX) THEN
         READ (NIN,*) NCAP, IWGHT
         IF (NCAP+7.LE.NC7MAX) THEN
            IF (NCAP.GT.1) READ (NIN,*) (LAMDA(J),J=5,NCAP+3)
            DO 40 R = 1, M
               IF (IWGHT.EQ.1) THEN
                  READ (NIN,*) X(R), Y(R)
                  W(R) = 1.0D0
               ELSE
                  READ (NIN,*) X(R), Y(R), W(R)
               END IF
   40       CONTINUE
            IFAIL = 0
*
            CALL E02BAF(M,NCAP+7,X,Y,W,LAMDA,WORK1,WORK2,C,SS,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        '  J       LAMDA(J+2)     B-spline coeff C(J)'
            WRITE (NOUT,*)
            J = 1
            WRITE (NOUT,99998) J, C(1)
            DO 60 J = 2, NCAP + 2
               WRITE (NOUT,99999) J, LAMDA(J+2), C(J)
   60       CONTINUE
            WRITE (NOUT,99998) NCAP + 3, C(NCAP+3)
            WRITE (NOUT,*)
            WRITE (NOUT,99997) 'Residual sum of squares = ', SS
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Cubic spline approximation and residuals'
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +       '  R   Abscissa     Weight   Ordinate     Spline  Residual'
            WRITE (NOUT,*)
            DO 80 R = 1, M
               IFAIL = 0
*
               CALL E02BBF(NCAP+7,LAMDA,C,X(R),FIT,IFAIL)
*
               WRITE (NOUT,99995) R, X(R), W(R), Y(R), FIT, FIT - Y(R)
               IF (R.LT.M) THEN
                  XARG = 0.5D0*(X(R)+X(R+1))
*
                  CALL E02BBF(NCAP+7,LAMDA,C,XARG,FIT,IFAIL)
*
                  WRITE (NOUT,99996) XARG, FIT
               END IF
   80       CONTINUE
            GO TO 20
         END IF
      END IF
  100 STOP
*
99999 FORMAT (1X,I3,F15.4,F20.4)
99998 FORMAT (1X,I3,F35.4)
99997 FORMAT (1X,A,D12.2)
99996 FORMAT (1X,F14.4,F33.4)
99995 FORMAT (1X,I3,4F11.4,D10.2)
      END
