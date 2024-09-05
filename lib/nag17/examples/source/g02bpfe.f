*     G02BPF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          M, N, IA, ICORR
      PARAMETER        (M=3,N=9,IA=N,ICORR=M)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, ITYPE, J, NCASES
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,M), CORR(ICORR,M), WA(M), WB(M), XMISS(M)
      INTEGER          INOUT(N), IW(N), JW(N), KW(N), MISS(M)
*     .. External Subroutines ..
      EXTERNAL         G02BPF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02BPF Example Program Results'
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
      MISS(2) = 0
      MISS(3) = 1
      XMISS(1) = 0.99D0
      XMISS(3) = 0.00D0
      ITYPE = 0
      IFAIL = 1
*
      CALL G02BPF(N,M,A,IA,MISS,XMISS,ITYPE,CORR,ICORR,NCASES,INOUT,IW,
     +            JW,KW,WA,WB,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Routine fails, IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Matrix of ranks:-'
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     + '(1 in the column headed In/Out indicates the case was included,'
         WRITE (NOUT,*)
     + ' 0 in the column headed In/Out indicates the case was omitted.)'
         WRITE (NOUT,*)
         WRITE (NOUT,99996) 'Case   In/Out', (J,J=1,M)
         WRITE (NOUT,99995) (I,INOUT(I),(A(I,J),J=1,M),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Matrix of rank correlation coefficients:'
         WRITE (NOUT,*) 'Upper triangle -- Spearman''s'
         WRITE (NOUT,*) 'Lower triangle -- Kendall''s tau'
         WRITE (NOUT,*)
         WRITE (NOUT,99998) (I,I=1,M)
         WRITE (NOUT,99997) (I,(CORR(I,J),J=1,M),I=1,M)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Number of cases actually used:', NCASES
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,3I12)
99997 FORMAT (1X,I3,3F12.4)
99996 FORMAT (1X,A,I6,2I12)
99995 FORMAT (1X,I3,I7,3F12.4)
      END
