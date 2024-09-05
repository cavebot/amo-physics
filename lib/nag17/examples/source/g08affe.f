*     G08AFF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          K, LMAX
      PARAMETER        (K=5,LMAX=35)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION H, P
      INTEGER          I, IFAIL, II, LX, NHI, NI, NLO
*     .. Local Arrays ..
      DOUBLE PRECISION W1(LMAX), X(LMAX)
      INTEGER          L(K)
*     .. External Subroutines ..
      EXTERNAL         G08AFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08AFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) L
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Kruskal-Wallis test'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Data values'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '  Group    Observations'
      LX = 0
      DO 20 I = 1, K
         LX = LX + L(I)
   20 CONTINUE
      IF (LX.LE.LMAX) THEN
         READ (NIN,*) (X(I),I=1,LX)
         IFAIL = 0
         NLO = 1
         DO 40 I = 1, K
            NI = L(I)
            NHI = NLO + NI - 1
            WRITE (NOUT,99999) I, (X(II),II=NLO,NHI)
            NLO = NLO + NI
   40    CONTINUE
*
         CALL G08AFF(X,LX,L,K,W1,H,P,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Test statistic       ', H
         WRITE (NOUT,99997) 'Degrees of freedom   ', K - 1
         WRITE (NOUT,99998) 'Significance         ', P
      END IF
      STOP
*
99999 FORMAT (1X,I5,5X,10F4.0)
99998 FORMAT (1X,A,F9.3)
99997 FORMAT (1X,A,I9)
      END
