*     G02BQF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          M, N, IA, ICORR
      PARAMETER        (M=3,N=9,IA=N,ICORR=M)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, ITYPE, J
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,M), CORR(ICORR,M), WA(N), WB(N)
      INTEGER          IW(N), JW(N)
*     .. External Subroutines ..
      EXTERNAL         G02BQF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02BQF Example Program Results'
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
      IFAIL = 1
      ITYPE = 0
*
      CALL G02BQF(N,M,A,IA,ITYPE,CORR,ICORR,IW,JW,WA,WB,IFAIL)
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
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,3I12)
99997 FORMAT (1X,I3,3F12.4)
      END
