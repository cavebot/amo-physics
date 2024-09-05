*     E02AGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MFMAX, MMAX, KP1MAX, NROWS, LA, LIWRK, LYF, LWRK
      PARAMETER        (MFMAX=5,MMAX=20,KP1MAX=6,NROWS=KP1MAX,
     +                 LA=NROWS*KP1MAX,LIWRK=2*MFMAX+2,LYF=15,LWRK=200)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION FITI, XMAX, XMIN
      INTEGER          I, IFAIL, IY, J, K, M, MF, NP1
*     .. Local Arrays ..
      DOUBLE PRECISION A(NROWS,KP1MAX), S(KP1MAX), W(MMAX), WRK(LWRK),
     +                 X(MMAX), XF(MFMAX), Y(MMAX), YF(LYF)
      INTEGER          IP(MFMAX), IWRK(LIWRK)
*     .. External Subroutines ..
      EXTERNAL         E02AGF, E02AKF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02AGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=100) MF
      IF (MF.GT.0 .AND. MF.LE.MFMAX) THEN
         IY = 1
         DO 40 I = 1, MF
            READ (NIN,*) IP(I), XF(I), (YF(J),J=IY,IY+IP(I))
            IY = IY + IP(I) + 1
   40    CONTINUE
         READ (NIN,*) M
         IF (M.GT.0 .AND. M.LE.MMAX) THEN
            READ (NIN,*) (X(I),Y(I),W(I),I=1,M)
            READ (NIN,*) K, XMIN, XMAX
            IFAIL = 0
*
            CALL E02AGF(M,K+1,NROWS,XMIN,XMAX,X,Y,W,MF,XF,YF,LYF,IP,A,S,
     +                  NP1,WRK,LWRK,IWRK,LIWRK,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Degree  RMS residual'
            WRITE (NOUT,99999) (I,S(I+1),I=NP1-1,K)
            WRITE (NOUT,*)
            WRITE (NOUT,99996) 'Details of the fit of degree ', K
            WRITE (NOUT,*)
            WRITE (NOUT,*) '  Index   Coefficient'
            DO 60 I = 1, K + 1
               WRITE (NOUT,99997) I - 1, A(K+1,I)
   60       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        '     I      X(I)       Y(I)       Fit     Residual'
            DO 80 I = 1, M
*
               CALL E02AKF(K+1,XMIN,XMAX,A(K+1,1),NROWS,LA-K,X(I),FITI,
     +                     IFAIL)
*
               WRITE (NOUT,99998) I, X(I), Y(I), FITI, FITI - Y(I)
   80       CONTINUE
            GO TO 20
         END IF
      END IF
  100 STOP
*
99999 FORMAT (1X,I4,1P,D15.2)
99998 FORMAT (1X,I6,3F11.4,D11.2)
99997 FORMAT (1X,I6,F11.4)
99996 FORMAT (1X,A,I2)
      END
