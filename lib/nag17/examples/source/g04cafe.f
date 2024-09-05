*     G04CAF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MAXF, MAXT, MTERM, BMAX
      PARAMETER        (NMAX=54,MAXF=2,MAXT=27,MTERM=6,BMAX=4)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INTER, IRDF, ITOTAL, J, K, L, N,
     +                 NBLOCK, NFAC, NTREAT
*     .. Local Arrays ..
      DOUBLE PRECISION BMEAN(BMAX), E(MAXT), R(NMAX), SEMEAN(MTERM),
     +                 TABLE(MTERM,5), TMEAN(MAXT), Y(NMAX)
      INTEGER          IMEAN(MTERM), IWK(NMAX+3*MAXF), LFAC(MAXF)
*     .. External Subroutines ..
      EXTERNAL         G04CAF
*     .. Executable Statements ..
      WRITE (NOUT,FMT=*) 'G04CAF Example Program Results'
*     Skip heading in data file
      READ (NIN,FMT=*)
      READ (NIN,FMT=*) N, NBLOCK, NFAC, INTER
      IF (N.LE.NMAX .AND. NBLOCK.LE.BMAX-1 .AND. NFAC.LE.MAXF) THEN
         READ (NIN,FMT=*) (LFAC(J),J=1,NFAC)
         READ (NIN,FMT=*) (Y(I),I=1,N)
         IRDF = 0
         IFAIL = -1
*
         CALL G04CAF(N,Y,NFAC,LFAC,NBLOCK,INTER,IRDF,MTERM,TABLE,ITOTAL,
     +               TMEAN,MAXT,E,IMEAN,SEMEAN,BMEAN,R,IWK,IFAIL)
*
         WRITE (NOUT,FMT=*)
         WRITE (NOUT,FMT=*) ' ANOVA table'
         WRITE (NOUT,FMT=*)
         WRITE (NOUT,FMT=*)
     +     '  Source      df         SS          MS          F',
     +     '        Prob'
         WRITE (NOUT,FMT=*)
         K = 0
         IF (NBLOCK.GT.1) THEN
            K = K + 1
            WRITE (NOUT,FMT=99998) ' Blocks    ', (TABLE(1,J),J=1,5)
         END IF
         NTREAT = ITOTAL - 2 - K
         DO 20 I = 1, NTREAT
            WRITE (NOUT,FMT=99997) ' Effect  ', I, (TABLE(K+I,J),J=1,5)
   20    CONTINUE
         WRITE (NOUT,FMT=99998) ' Residual  ', (TABLE(ITOTAL-1,J),J=1,3)
         WRITE (NOUT,FMT=99998) ' Total     ', (TABLE(ITOTAL,J),J=1,2)
         WRITE (NOUT,FMT=*)
         WRITE (NOUT,FMT=*) ' Treatment Means and Standard Errors'
         WRITE (NOUT,FMT=*)
         K = 1
         DO 40 I = 1, NTREAT
            L = IMEAN(I)
            WRITE (NOUT,FMT=99996) ' Effect ', I
            WRITE (NOUT,FMT=*)
            WRITE (NOUT,FMT=99999) (TMEAN(J),J=K,L)
            WRITE (NOUT,FMT=*)
            WRITE (NOUT,FMT=99995) ' SE of difference in means  = ',
     +        SEMEAN(I)
            WRITE (NOUT,FMT=*)
            K = L + 1
   40    CONTINUE
      END IF
      STOP
*
99999 FORMAT (8F10.2)
99998 FORMAT (A,3X,F3.0,2X,2(F10.0,2X),F10.3,2X,F9.4)
99997 FORMAT (A,I2,3X,F3.0,2X,2(F10.0,2X),F10.3,2X,F9.4)
99996 FORMAT (A,I2)
99995 FORMAT (A,F10.2)
      END
