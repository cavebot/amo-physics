*     G02CHF Example Program Text
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
      DOUBLE PRECISION C(IC,K), COEFFT(ICOEFF,3), CORR(ICORR,K1),
     +                 RESULT(13), RINV(IRINV,K), SSP(K1,K1), W(IW,K)
*     .. External Subroutines ..
      EXTERNAL         G02CHF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02CHF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) ((SSP(I,J),J=1,K1),I=1,K1),
     +  ((CORR(I,J),J=1,K1),I=1,K1)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Sums of squares and cross-products about zero:'
      WRITE (NOUT,99999) (J,J=1,K1)
      WRITE (NOUT,99998) (I,(SSP(I,J),J=1,K1),I=1,K1)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Correlation-like coefficients:'
      WRITE (NOUT,99999) (J,J=1,K1)
      WRITE (NOUT,99998) (I,(CORR(I,J),J=1,K1),I=1,K1)
      WRITE (NOUT,*)
      IFAIL = 1
*
      CALL G02CHF(N,K1,K,SSP,ISSP,CORR,ICORR,RESULT,COEFFT,ICOEFF,RINV,
     +            IRINV,C,IC,W,IW,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99997) 'Routine fails, IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Vble     Coefft       Std err      t-value'
         WRITE (NOUT,99996) (I,(COEFFT(I,J),J=1,3),I=1,K)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Analysis of regression table :-'
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +'      Source        Sum of squares  D.F.    Mean square     F-val
     +ue'
         WRITE (NOUT,*)
         WRITE (NOUT,99995) 'Due to regression', (RESULT(I),I=1,4)
         WRITE (NOUT,99995) 'About  regression', (RESULT(I),I=5,7)
         WRITE (NOUT,99995) 'Total            ', (RESULT(I),I=8,9)
         WRITE (NOUT,*)
         WRITE (NOUT,99994) 'Standard error of estimate =', RESULT(10)
         WRITE (NOUT,99994) 'Multiple correlation (R)   =', RESULT(11)
         WRITE (NOUT,99994) 'Determination (R squared)  =', RESULT(12)
         WRITE (NOUT,99994) 'Corrected R squared        =', RESULT(13)
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'Inverse of correlation matrix of independent variables:'
         WRITE (NOUT,99993) (J,J=1,K)
         WRITE (NOUT,99992) (I,(RINV(I,J),J=1,K),I=1,K)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Modified inverse matrix:'
         WRITE (NOUT,99993) (J,J=1,K)
         WRITE (NOUT,99992) (I,(C(I,J),J=1,K),I=1,K)
      END IF
      STOP
*
99999 FORMAT (1X,3I10)
99998 FORMAT (1X,I4,3F10.4)
99997 FORMAT (1X,A,I2)
99996 FORMAT (1X,I3,3F13.4)
99995 FORMAT (1X,A,F14.4,F8.0,2F14.4)
99994 FORMAT (1X,A,F8.4)
99993 FORMAT (1X,2I10)
99992 FORMAT (1X,I4,2F10.4)
      END
