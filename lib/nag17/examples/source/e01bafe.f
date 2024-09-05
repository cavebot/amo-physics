*     E01BAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          M, LCK, LWRK
      PARAMETER        (M=7,LCK=M+4,LWRK=6*M+16)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION FIT, XARG
      INTEGER          I, IFAIL, J, R
*     .. Local Arrays ..
      DOUBLE PRECISION C(LCK), LAMDA(LCK), WRK(LWRK), X(M), Y(M)
*     .. External Subroutines ..
      EXTERNAL         E01BAF, E02BBF
*     .. Intrinsic Functions ..
      INTRINSIC        EXP
*     .. Data statements ..
      DATA             (X(I),I=1,M)/0.0D0, 0.2D0, 0.4D0, 0.6D0, 0.75D0,
     +                 0.9D0, 1.0D0/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E01BAF Example Program Results'
      DO 20 I = 1, M
         Y(I) = EXP(X(I))
   20 CONTINUE
      IFAIL = 0
*
      CALL E01BAF(M,X,Y,LAMDA,C,LCK,WRK,LWRK,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   J    Knot LAMDA(J+2)   B-spline coeff C(J)'
      WRITE (NOUT,*)
      J = 1
      WRITE (NOUT,99998) J, C(1)
      DO 40 J = 2, M - 1
         WRITE (NOUT,99999) J, LAMDA(J+2), C(J)
   40 CONTINUE
      WRITE (NOUT,99998) M, C(M)
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  '   R        Abscissa            Ordinate             Spline'
      WRITE (NOUT,*)
      DO 60 R = 1, M
         IFAIL = 0
*
         CALL E02BBF(M+4,LAMDA,C,X(R),FIT,IFAIL)
*
         WRITE (NOUT,99999) R, X(R), Y(R), FIT
         IF (R.LT.M) THEN
            XARG = 0.5D0*(X(R)+X(R+1))
*
            CALL E02BBF(M+4,LAMDA,C,XARG,FIT,IFAIL)
*
            WRITE (NOUT,99997) XARG, FIT
         END IF
   60 CONTINUE
      STOP
*
99999 FORMAT (1X,I4,F15.4,2F20.4)
99998 FORMAT (1X,I4,F35.4)
99997 FORMAT (1X,F19.4,F40.4)
      END
