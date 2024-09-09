*     G02CGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          K1, N, K, ISSP, ICORR, ICOEFF, IRINV, IC, IW
      PARAMETER        (K1=3,N=5,K=K1-1,ISSP=K1,ICORR=K1,ICOEFF=K,
     +                 IRINV=K,IC=K,IW=K)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION C(IC,K), COEFFT(ICOEFF,3), CON(3),
     +                 CORR(ICORR,K1), RESULT(13), RINV(IRINV,K),
     +                 SSP(ISSP,K1), W(IW,K), XBAR(K1)
*     .. External Subroutines ..
      EXTERNAL         G02CGF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02CGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) (XBAR(I),I=1,K1), ((SSP(I,J),J=1,K1),I=1,K1),
     +  ((CORR(I,J),J=1,K1),I=1,K1)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Means:'
      WRITE (NOUT,99999) (I,XBAR(I),I=1,K1)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Sums of squares and cross-products about means:'
      WRITE (NOUT,99998) (J,J=1,K1)
      WRITE (NOUT,99997) (I,(SSP(I,J),J=1,K1),I=1,K1)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Correlation coefficients:'
      WRITE (NOUT,99998) (J,J=1,K1)
      WRITE (NOUT,99997) (I,(CORR(I,J),J=1,K1),I=1,K1)
      WRITE (NOUT,*)
      IFAIL = 1
*
      CALL G02CGF(N,K1,K,XBAR,SSP,ISSP,CORR,ICORR,RESULT,COEFFT,ICOEFF,
     +            CON,RINV,IRINV,C,IC,W,IW,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99996) 'Routine fails, IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Vble     Coefft       Std err      t-value'
         WRITE (NOUT,99995) (I,(COEFFT(I,J),J=1,3),I=1,K)
         WRITE (NOUT,*)
         WRITE (NOUT,99994) 'Const', (CON(I),I=1,3)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Analysis of regression table :-'
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +'      Source        Sum of squares  D.F.    Mean square     F-val
     +ue'
         WRITE (NOUT,*)
         WRITE (NOUT,99993) 'Due to regression', (RESULT(I),I=1,4)
         WRITE (NOUT,99993) 'About  regression', (RESULT(I),I=5,7)
         WRITE (NOUT,99993) 'Total            ', (RESULT(I),I=8,9)
         WRITE (NOUT,*)
         WRITE (NOUT,99992) 'Standard error of estimate =', RESULT(10)
         WRITE (NOUT,99992) 'Multiple correlation (R)   =', RESULT(11)
         WRITE (NOUT,99992) 'Determination (R squared)  =', RESULT(12)
         WRITE (NOUT,99992) 'Corrected R squared        =', RESULT(13)
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'Inverse of correlation matrix of independent variables:'
         WRITE (NOUT,99991) (J,J=1,K)
         WRITE (NOUT,99990) (I,(RINV(I,J),J=1,K),I=1,K)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Modified inverse matrix:'
         WRITE (NOUT,99991) (J,J=1,K)
         WRITE (NOUT,99990) (I,(C(I,J),J=1,K),I=1,K)
      END IF
      STOP
*
99999 FORMAT (1X,I4,F10.4)
99998 FORMAT (1X,3I10)
99997 FORMAT (1X,I4,3F10.4)
99996 FORMAT (1X,A,I2)
99995 FORMAT (1X,I3,3F13.4)
99994 FORMAT (1X,A,F11.4,2F13.4)
99993 FORMAT (1X,A,F14.4,F8.0,2F14.4)
99992 FORMAT (1X,A,F8.4)
99991 FORMAT (1X,2I10)
99990 FORMAT (1X,I4,2F10.4)
      END
