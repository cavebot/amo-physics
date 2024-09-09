*     G02BSF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          M, N, IA, ICORR, IC
      PARAMETER        (M=3,N=9,IA=N,ICORR=M,IC=M)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, ITYPE, J, NCASES
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,M), CASES(IC,M), CORR(ICORR,M), WA(N),
     +                 WB(N), XMISS(M)
      INTEGER          IW(N), JW(N), KW(N), LW(N), MISS(M)
*     .. External Subroutines ..
      EXTERNAL         G02BSF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02BSF Example Program Results'
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
      XMISS(1) = 0.99D0
      XMISS(2) = 9.0D0
      XMISS(3) = 0.00D0
      ITYPE = 0
      IFAIL = 1
*
      CALL G02BSF(N,M,A,IA,MISS,XMISS,ITYPE,CORR,ICORR,NCASES,CASES,IC,
     +            IW,JW,KW,LW,WA,WB,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Routine fails, IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Matrix of rank correlation coefficients:'
         WRITE (NOUT,*) 'Upper triangle -- Spearman''s'
         WRITE (NOUT,*) 'Lower triangle -- Kendall''s tau'
         WRITE (NOUT,*)
         WRITE (NOUT,99998) (I,I=1,M)
         WRITE (NOUT,99997) (I,(CORR(I,J),J=1,M),I=1,M)
         WRITE (NOUT,*)
         WRITE (NOUT,99999)
     +     'Minimum number of cases used for any pair of variables:',
     +     NCASES
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Numbers used for each pair are:'
         WRITE (NOUT,99998) (I,I=1,M)
         WRITE (NOUT,99997) (I,(CASES(I,J),J=1,M),I=1,M)
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,3I12)
99997 FORMAT (1X,I3,3F12.4)
      END
