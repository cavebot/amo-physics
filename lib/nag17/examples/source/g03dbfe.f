*     G03DBF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX, GPMAX
      PARAMETER        (NMAX=21,MMAX=2,GPMAX=3)
*     .. Local Scalars ..
      DOUBLE PRECISION DF, SIG, STAT
      INTEGER          I, IFAIL, J, M, N, NG, NOBS, NVAR
      CHARACTER        EQUAL, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION D(NMAX,GPMAX), DET(GPMAX),
     +                 GC((GPMAX+1)*MMAX*(MMAX+1)/2), GMEAN(GPMAX,MMAX),
     +                 WK(NMAX*(MMAX+1)), WT(NMAX), X(NMAX,MMAX)
      INTEGER          ING(NMAX), ISX(MMAX), IWK(GPMAX), NIG(GPMAX)
*     .. External Subroutines ..
      EXTERNAL         G03DAF, G03DBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G03DBF Example Program Results'
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
         READ (NIN,*) NOBS, EQUAL
         IF (NOBS.LE.NMAX) THEN
            DO 60 I = 1, NOBS
               READ (NIN,*) (X(I,J),J=1,M)
   60       CONTINUE
            IFAIL = 0
*
            CALL G03DBF(EQUAL,'Sample points',NVAR,NG,GMEAN,GPMAX,GC,
     +                  NOBS,M,ISX,X,NMAX,D,NMAX,WK,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,*) '  Obs          Distances'
            WRITE (NOUT,*)
            DO 80 I = 1, NOBS
               WRITE (NOUT,99999) I, (D(I,J),J=1,NG)
   80       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,I3,3F10.3)
      END
