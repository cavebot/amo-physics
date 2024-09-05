*     G02BCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          M, N, IA, ISSP, ICORR, IC
      PARAMETER        (M=3,N=5,IA=N,ISSP=M,ICORR=M,IC=M)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, NCASES
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,M), AMEAN(M), CASES(IC,M), CORR(ICORR,M),
     +                 SSP(ISSP,M), STD(M), XMISS(M)
      INTEGER          MISS(M)
*     .. External Subroutines ..
      EXTERNAL         G02BCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02BCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) ((A(I,J),J=1,M),I=1,N)
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Number of variables (columns) =', M
      WRITE (NOUT,99999) 'Number of cases     (rows)    =', N
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Data matrix is:-'
      WRITE (NOUT,*)
      WRITE (NOUT,99998) (J,J=1,M)
      WRITE (NOUT,99997) (I,(A(I,J),J=1,M),I=1,N)
      WRITE (NOUT,*)
*
*     Set up missing values before calling routine
*
      MISS(1) = 1
      MISS(2) = 1
      MISS(3) = 1
      XMISS(1) = 0.0D0
      XMISS(2) = -1.0D0
      XMISS(3) = 0.0D0
      IFAIL = 1
*
      CALL G02BCF(N,M,A,IA,MISS,XMISS,AMEAN,STD,SSP,ISSP,CORR,ICORR,
     +            NCASES,CASES,IC,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99996) 'Routine fails, IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Variable   Mean    St. dev.'
         WRITE (NOUT,99995) (I,AMEAN(I),STD(I),I=1,M)
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'Sums of squares and cross-products of deviations'
         WRITE (NOUT,99998) (I,I=1,M)
         WRITE (NOUT,99997) (I,(SSP(I,J),J=1,M),I=1,M)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Correlation coefficients'
         WRITE (NOUT,99998) (I,I=1,M)
         WRITE (NOUT,99997) (I,(CORR(I,J),J=1,M),I=1,M)
         WRITE (NOUT,*)
         WRITE (NOUT,99999)
     +     'Minimum number of cases used for any pair of variables: ',
     +     NCASES
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Numbers used for each pair are:'
         WRITE (NOUT,99998) (I,I=1,M)
         WRITE (NOUT,99997) (I,(CASES(I,J),J=1,M),I=1,M)
      END IF
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,6I12)
99997 FORMAT (1X,I3,3F12.4)
99996 FORMAT (1X,A,I2)
99995 FORMAT (1X,I5,2F11.4)
      END
