*     G04AGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          K, LMAX, NMAX
      PARAMETER        (K=2,LMAX=8,NMAX=28)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION GM
      INTEGER          I, IFAIL, II, J, L, LI, N, NHI, NIJ, NLO, NSUB
*     .. Local Arrays ..
      DOUBLE PRECISION F(2), FP(2), GBAR(K), SGBAR(LMAX), SS(4), Y(NMAX)
      INTEGER          IDF(4), LSUB(K), NGP(K), NOBS(LMAX)
*     .. External Subroutines ..
      EXTERNAL         G04AGF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G04AGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Data values'
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' Group  Subgroup  Observations'
      LSUB(1) = 5
      LSUB(2) = 3
      L = LSUB(1) + LSUB(2)
      IF (L.LE.LMAX) THEN
         READ (NIN,*) (NOBS(I),I=1,L)
         N = 0
         DO 20 I = 1, L
            N = N + NOBS(I)
   20    CONTINUE
         IF (N.LE.NMAX) THEN
            READ (NIN,*) (Y(I),I=1,N)
            IFAIL = 1
            NSUB = 0
            NLO = 1
            DO 60 I = 1, K
               LI = LSUB(I)
               DO 40 J = 1, LI
                  NSUB = NSUB + 1
                  NIJ = NOBS(NSUB)
                  NHI = NLO + NIJ - 1
                  WRITE (NOUT,99999) I, J, (Y(II),II=NLO,NHI)
                  NLO = NLO + NIJ
   40          CONTINUE
   60       CONTINUE
*
            CALL G04AGF(Y,N,K,LSUB,NOBS,L,NGP,GBAR,SGBAR,GM,SS,IDF,F,FP,
     +                  IFAIL)
*
            IF (IFAIL.NE.0) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,99997) 'Failed in G04AGF. IFAIL = ', IFAIL
            ELSE
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Subgroup means'
               WRITE (NOUT,*)
               WRITE (NOUT,*) '   Group  Subgroup  Mean'
               II = 0
               DO 100 I = 1, K
                  LI = LSUB(I)
                  DO 80 J = 1, LI
                     II = II + 1
                     WRITE (NOUT,99998) I, J, SGBAR(II)
   80             CONTINUE
  100          CONTINUE
               WRITE (NOUT,*)
               WRITE (NOUT,99996) '    Group 1 mean = ', GBAR(1),
     +           '   (', NGP(1), ' observations)'
               WRITE (NOUT,99996) '    Group 2 mean = ', GBAR(2),
     +           '   (', NGP(2), ' observations)'
               WRITE (NOUT,99996) '    Grand mean   = ', GM, '   (', N,
     +           ' observations)'
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Analysis of variance table'
               WRITE (NOUT,*)
               WRITE (NOUT,*)
     +           '   Source                SS     DF  F ratio  Sig'
               WRITE (NOUT,*)
               WRITE (NOUT,99995) 'Between groups         ', SS(1),
     +           IDF(1), F(1), FP(1)
               WRITE (NOUT,99995) 'Bet sbgps within gps   ', SS(2),
     +           IDF(2), F(2), FP(2)
               WRITE (NOUT,99995) 'Residual               ', SS(3),
     +           IDF(3)
               WRITE (NOUT,*)
               WRITE (NOUT,99995) 'Total                  ', SS(4),
     +           IDF(4)
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,I5,I9,4X,10F4.1)
99998 FORMAT (1X,I6,I8,F10.2)
99997 FORMAT (1X,A,I2)
99996 FORMAT (1X,A,F4.2,A,I2,A)
99995 FORMAT (1X,A,F5.3,I5,F7.2,F8.3)
      END
