*     G13DBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NSMAX, NSM, NLMAX, NKMAX, IWA
      PARAMETER        (NSMAX=6,NSM=NSMAX,NLMAX=5,NKMAX=NLMAX,
     +                 IWA=(2*NSMAX+1)*NSMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION V0
      INTEGER          I, I1, IFAIL, J, J1, K, NK, NL, NS, NVP
*     .. Local Arrays ..
      DOUBLE PRECISION C(NSM,NSM,NLMAX), C0(NSM,NSMAX),
     +                 D(NSM,NSM,NKMAX), DB(NSM,NSMAX), P(NKMAX),
     +                 V(NKMAX), W(NSM,NSM,NKMAX), WA(IWA),
     +                 WB(NSM,NSM,NKMAX)
*     .. External Subroutines ..
      EXTERNAL         G13DBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13DBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Read series length, and numbers of lags
      READ (NIN,*) NS, NL, NK
      IF (NS.GT.0 .AND. NS.LE.NSMAX .AND. NL.GT.0 .AND. NL.LE.
     +    NLMAX .AND. NK.GT.0 .AND. NK.LE.NKMAX) THEN
*        Read autocovariances
         READ (NIN,*) ((C0(I,J),J=1,NS),I=1,NS)
         READ (NIN,*) (((C(I,J,K),J=1,NS),I=1,NS),K=1,NL)
*        Call routine to calculate multivariate partial autocorrelation
*        function
         IFAIL = 1
*
         CALL G13DBF(C0,C,NSM,NS,NL,NK,P,V0,V,D,DB,W,WB,NVP,WA,IWA,
     +               IFAIL)
*
         WRITE (NOUT,*)
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'G13DBF fails. IFAIL =', IFAIL
            WRITE (NOUT,*)
         END IF
         IF (IFAIL.EQ.0 .OR. IFAIL.EQ.3) THEN
            WRITE (NOUT,99998) 'Number of valid parameters =', NVP
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Multivariate partial autocorrelations'
            WRITE (NOUT,99997) (P(I1),I1=1,NK)
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        'Zero lag predictor error variance determinant'
            WRITE (NOUT,*) 'followed by error variance ratios'
            WRITE (NOUT,99997) V0, (V(I1),I1=1,NK)
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Prediction error variances'
            DO 40 K = 1, NK
               WRITE (NOUT,*)
               WRITE (NOUT,99996) 'Lag =', K
               DO 20 I = 1, NS
                  WRITE (NOUT,99997) (D(I,J1,K),J1=1,NS)
   20          CONTINUE
   40       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Last backward prediction error variances'
            WRITE (NOUT,*)
            WRITE (NOUT,99996) 'Lag =', NVP
            DO 60 I = 1, NS
               WRITE (NOUT,99997) (DB(I,J1),J1=1,NS)
   60       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Prediction coefficients'
            DO 100 K = 1, NK
               WRITE (NOUT,*)
               WRITE (NOUT,99996) 'Lag =', K
               DO 80 I = 1, NS
                  WRITE (NOUT,99997) (W(I,J1,K),J1=1,NS)
   80          CONTINUE
  100       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Backward prediction coefficients'
            DO 140 K = 1, NK
               WRITE (NOUT,*)
               WRITE (NOUT,99996) 'Lag =', K
               DO 120 I = 1, NS
                  WRITE (NOUT,99997) (WB(I,J1,K),J1=1,NS)
  120          CONTINUE
  140       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,A,I10)
99997 FORMAT (1X,5F12.5)
99996 FORMAT (1X,A,I5)
      END
