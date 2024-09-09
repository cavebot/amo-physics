*     E02ADF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, KP1MAX, NROWS
      PARAMETER        (MMAX=200,KP1MAX=50,NROWS=KP1MAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION FIT, X1, XARG, XCAPR, XM
      INTEGER          I, IFAIL, IWGHT, J, K, M, R
*     .. Local Arrays ..
      DOUBLE PRECISION A(NROWS,KP1MAX), AK(KP1MAX), S(KP1MAX), W(MMAX),
     +                 WORK1(3*MMAX), WORK2(2*KP1MAX), X(MMAX), Y(MMAX)
*     .. External Subroutines ..
      EXTERNAL         E02ADF, E02AEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02ADF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=120) M
      IF (M.GT.0 .AND. M.LE.MMAX) THEN
         READ (NIN,*) K, IWGHT
         IF (K+1.LE.KP1MAX) THEN
            DO 40 R = 1, M
               IF (IWGHT.NE.1) THEN
                  READ (NIN,*) X(R), Y(R), W(R)
               ELSE
                  READ (NIN,*) X(R), Y(R)
                  W(R) = 1.0D0
               END IF
   40       CONTINUE
            IFAIL = 0
*
            CALL E02ADF(M,K+1,NROWS,X,Y,W,WORK1,WORK2,A,S,IFAIL)
*
            DO 60 I = 0, K
               WRITE (NOUT,*)
               WRITE (NOUT,99998) 'Degree', I, '   R.M.S. residual =',
     +           S(I+1)
               WRITE (NOUT,*)
               WRITE (NOUT,*) '  J  Chebyshev coeff A(J)'
               WRITE (NOUT,99997) (J,A(I+1,J),J=1,I+1)
   60       CONTINUE
            DO 80 J = 1, K + 1
               AK(J) = A(K+1,J)
   80       CONTINUE
            X1 = X(1)
            XM = X(M)
            WRITE (NOUT,*)
            WRITE (NOUT,99996)
     +        'Polynomial approximation and residuals for degree', K
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +      '  R   Abscissa     Weight   Ordinate  Polynomial  Residual'
            DO 100 R = 1, M
               XCAPR = ((X(R)-X1)-(XM-X(R)))/(XM-X1)
               IFAIL = 0
*
               CALL E02AEF(K+1,AK,XCAPR,FIT,IFAIL)
*
               WRITE (NOUT,99999) R, X(R), W(R), Y(R), FIT, FIT - Y(R)
               IF (R.LT.M) THEN
                  XARG = 0.5D0*(X(R)+X(R+1))
                  XCAPR = ((XARG-X1)-(XM-XARG))/(XM-X1)
*
                  CALL E02AEF(K+1,AK,XCAPR,FIT,IFAIL)
*
                  WRITE (NOUT,99995) XARG, FIT
               END IF
  100       CONTINUE
            GO TO 20
         END IF
      END IF
  120 STOP
*
99999 FORMAT (1X,I3,4F11.4,D11.2)
99998 FORMAT (1X,A,I4,A,D12.2)
99997 FORMAT (1X,I3,F15.4)
99996 FORMAT (1X,A,I4)
99995 FORMAT (4X,F11.4,22X,F11.4)
      END
