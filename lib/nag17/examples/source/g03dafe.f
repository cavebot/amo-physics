*     G03DAF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX, GPMAX
      PARAMETER        (NMAX=21,MMAX=2,GPMAX=3)
*     .. Local Scalars ..
      DOUBLE PRECISION DF, SIG, STAT
      INTEGER          I, IFAIL, J, M, N, NG, NVAR
      CHARACTER        WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION DET(GPMAX), GC((GPMAX+1)*MMAX*(MMAX+1)/2),
     +                 GMEAN(GPMAX,MMAX), WK(NMAX*(MMAX+1)), WT(NMAX),
     +                 X(NMAX,MMAX)
      INTEGER          ING(NMAX), ISX(MMAX), IWK(GPMAX), NIG(GPMAX)
*     .. External Subroutines ..
      EXTERNAL         G03DAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G03DAF Example Program Results'
*     Skip headings in data file
      READ (NIN,*)
      READ (NIN,*) N, M, NVAR, NG, WEIGHT
      IF (N.LE.NMAX .AND. M.LE.MMAX) THEN
         IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
            DO 20 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), ING(I), WT(I)
   20       CONTINUE
         ELSE
            DO 40 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), ING(I)
   40       CONTINUE
         END IF
         READ (NIN,*) (ISX(J),J=1,M)
         IFAIL = 0
*
         CALL G03DAF(WEIGHT,N,M,X,NMAX,ISX,NVAR,ING,NG,WT,NIG,GMEAN,
     +               GPMAX,DET,GC,STAT,DF,SIG,WK,IWK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Group means'
         WRITE (NOUT,*)
         DO 60 I = 1, NG
            WRITE (NOUT,99999) (GMEAN(I,J),J=1,NVAR)
   60    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' LOG of determinants'
         WRITE (NOUT,*)
         WRITE (NOUT,99999) (DET(J),J=1,NG)
         WRITE (NOUT,*)
         WRITE (NOUT,99998) ' STAT = ', STAT
         WRITE (NOUT,99998) '   DF = ', DF
         WRITE (NOUT,99998) '  SIG = ', SIG
      END IF
      STOP
*
99999 FORMAT (1X,3F10.4)
99998 FORMAT (1X,A,F7.4)
      END
