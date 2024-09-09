*     G02BKF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          M, N, NV, IA, ISSP, ICORR
      PARAMETER        (M=4,N=5,NV=3,IA=N,ISSP=NV,ICORR=NV)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,M), AMEAN(NV), CORR(ICORR,NV), SSP(ISSP,NV),
     +                 STD(NV)
      INTEGER          KVAR(NV)
*     .. External Subroutines ..
      EXTERNAL         G02BKF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02BKF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) ((A(I,J),J=1,M),I=1,N)
      KVAR(1) = 4
      KVAR(2) = 1
      KVAR(3) = 2
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Number of variables (columns) =', M
      WRITE (NOUT,99999) 'Number of cases     (rows)    =', N
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Data matrix is:-'
      WRITE (NOUT,*)
      WRITE (NOUT,99998) (J,J=1,M)
      WRITE (NOUT,99997) (I,(A(I,J),J=1,M),I=1,N)
      WRITE (NOUT,*)
      IFAIL = 1
*
      CALL G02BKF(N,M,A,IA,NV,KVAR,AMEAN,STD,SSP,ISSP,CORR,ICORR,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Routine fails, IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Variable   Mean    St. dev.'
         WRITE (NOUT,99995) (KVAR(I),AMEAN(I),STD(I),I=1,NV)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Sums of squares and cross-products about zero'
         WRITE (NOUT,99998) (KVAR(I),I=1,NV)
         WRITE (NOUT,99996) (KVAR(I),(SSP(I,J),J=1,NV),I=1,NV)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Correlation-like coefficients'
         WRITE (NOUT,99998) (KVAR(I),I=1,NV)
         WRITE (NOUT,99996) (KVAR(I),(CORR(I,J),J=1,NV),I=1,NV)
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,4I12)
99997 FORMAT (1X,I3,4F12.4)
99996 FORMAT (1X,I3,3F12.4)
99995 FORMAT (1X,I5,2F11.4)
      END
