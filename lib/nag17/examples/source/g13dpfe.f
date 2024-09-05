*     G13DPF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          KMAX, NMAX, MMAX, LWORK
      PARAMETER        (KMAX=4,NMAX=50,MMAX=10,LWORK=2081)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, M, MAXLAG, N
*     .. Local Arrays ..
      DOUBLE PRECISION LOGLHD(MMAX), PARLAG(KMAX,KMAX,MMAX),
     +                 PVALUE(MMAX), QQ(KMAX,KMAX,MMAX),
     +                 SE(KMAX,KMAX,MMAX), W(KMAX,NMAX), WORK(LWORK),
     +                 X(MMAX)
      INTEGER          IWORK(KMAX*MMAX)
*     .. External Subroutines ..
      EXTERNAL         G13DPF, ZPRINT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13DPF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) K, N, M
*
      IF (K.GT.0 .AND. K.LE.KMAX .AND. N.GE.1 .AND. N.LE.NMAX .AND.
     +    M.GE.1 .AND. M.LE.MMAX) THEN
*
         DO 20 I = 1, K
            READ (NIN,*) (W(I,J),J=1,N)
   20    CONTINUE
*
         IFAIL = 0
*
         CALL G13DPF(K,N,W,KMAX,M,MAXLAG,PARLAG,SE,QQ,X,PVALUE,LOGLHD,
     +               WORK,LWORK,IWORK,IFAIL)
*
         CALL ZPRINT(K,N,M,KMAX,MAXLAG,PARLAG,SE,QQ,X,PVALUE,NOUT,IFAIL)
*
      END IF
      STOP
      END
*
      SUBROUTINE ZPRINT(K,N,M,KMAX,MAXLAG,PARLAG,SE,QQ,X,PVALUE,NOUT,
     +                  IFAIL)
*
*     .. Scalar Arguments ..
      INTEGER           IFAIL, K, KMAX, M, MAXLAG, N, NOUT
*     .. Array Arguments ..
      DOUBLE PRECISION  PARLAG(KMAX,KMAX,M), PVALUE(M), QQ(KMAX,KMAX,M),
     +                  SE(KMAX,KMAX,M), X(M)
*     .. Local Scalars ..
      DOUBLE PRECISION  SUM
      INTEGER           I, I2, J, L
*     .. Local Arrays ..
      CHARACTER*6       ST(6)
*     .. Executable Statements ..
*
      IF (K.GT.1) WRITE (NOUT,99999)
      IF (K.EQ.1) WRITE (NOUT,99998)
      DO 80 L = 1, MAXLAG
         DO 20 J = 1, K
            SUM = PARLAG(1,J,L)
            ST(J) = '.'
            IF (SUM.GT.1.96D0*SE(1,J,L)) ST(J) = '+'
            IF (SUM.LT.-1.96D0*SE(1,J,L)) ST(J) = '-'
   20    CONTINUE
         IF (K.EQ.1) THEN
            WRITE (NOUT,99997) L, (PARLAG(1,J,L),J=1,K),
     +        (ST(I2),I2=1,K), QQ(1,1,L), X(L), PVALUE(L)
            WRITE (NOUT,99996) (SE(1,J,L),J=1,K)
         ELSE IF (K.EQ.2) THEN
            WRITE (NOUT,99995) L, (PARLAG(1,J,L),J=1,K),
     +        (ST(I2),I2=1,K), QQ(1,1,L), X(L), PVALUE(L)
            WRITE (NOUT,99994) (SE(1,J,L),J=1,K)
         ELSE IF (K.EQ.3) THEN
            WRITE (NOUT,99993) L, (PARLAG(1,J,L),J=1,K),
     +        (ST(I2),I2=1,K), QQ(1,1,L), X(L), PVALUE(L)
            WRITE (NOUT,99992) (SE(1,J,L),J=1,K)
         ELSE IF (K.EQ.4) THEN
            WRITE (NOUT,99991) L
            WRITE (NOUT,99984) (PARLAG(1,J,L),J=1,K), (ST(I2),I2=1,K),
     +        QQ(1,1,L), X(L), PVALUE(L)
            WRITE (NOUT,99990) (SE(1,J,L),J=1,K)
         END IF
*
         DO 60 I = 2, K
*
            DO 40 J = 1, K
               SUM = PARLAG(I,J,L)
               ST(J) = '.'
               IF (SUM.GT.1.96D0*SE(I,J,L)) ST(J) = '+'
               IF (SUM.LT.-1.96D0*SE(I,J,L)) ST(J) = '-'
   40       CONTINUE
            IF (K.EQ.2) THEN
               WRITE (NOUT,99987) (PARLAG(I,J,L),J=1,K),
     +           (ST(I2),I2=1,K), QQ(I,I,L)
               WRITE (NOUT,99994) (SE(I,J,L),J=1,K)
            ELSE IF (K.EQ.3) THEN
               WRITE (NOUT,99986) (PARLAG(I,J,L),J=1,K),
     +           (ST(I2),I2=1,K), QQ(I,I,L)
               WRITE (NOUT,99992) (SE(I,J,L),J=1,K)
            ELSE IF (K.EQ.4) THEN
               WRITE (NOUT,99985) (PARLAG(I,J,L),J=1,K),
     +           (ST(I2),I2=1,K), QQ(I,I,L)
               WRITE (NOUT,99990) (SE(I,J,L),J=1,K)
            END IF
*
   60    CONTINUE
   80 CONTINUE
*
      WRITE (NOUT,99983) IFAIL
*
      RETURN
*
99999 FORMAT (/' Partial Autoregression Matrices',4X,'Indicator',2X,
     +       'Residual',3X,'Chi-Square',2X,'Pvalue',/37X,'Symbols',3X,
     +       'Variances',3X,'Statistic',/' ---------------------------',
     +       '----',4X,'---------',2X,'---------',2X,'-----------',1X,
     +       '------')
99998 FORMAT (/' Partial Autoregression Function',4X,'Indicator',2X,
     +       'Residual',3X,'Chi-Square',2X,'Pvalue',/37X,'Symbols',3X,
     +       'Variances',3X,'Statistic',/' ---------------------------',
     +       '----',4X,'---------',2X,'---------',2X,'-----------',1X,
     +       '------')
99997 FORMAT (/' Lag',I3,1X,':',F7.3,22X,A1,F14.3,3X,F10.3,F9.3)
99996 FORMAT (10X,'(',F5.3,')')
99995 FORMAT (/' Lag',I3,1X,':',2F8.3,14X,2A1,F13.3,3X,F10.3,F9.3)
99994 FORMAT (11X,'(',F5.3,') (',F5.3,')')
99993 FORMAT (/' Lag',I3,1X,':',3F8.3,6X,3A1,F12.3,3X,F10.3,F9.3)
99992 FORMAT (11X,'(',F5.3,') (',F5.3,') (',F5.3,')')
99991 FORMAT (/' Lag',I3)
99990 FORMAT (3X,'(',F5.3,') (',F5.3,') (',F5.3,') (',F5.3,')')
99989 FORMAT (/' Lag',I3,1X,':',5F7.3,1X,5A1,F10.3,3X,F10.3,F9.3)
99988 FORMAT (10X,'(',F5.3,')(',F5.3,')(',F5.3,')(',F5.3,')(',F5.3,')')
99987 FORMAT (9X,2F8.3,14X,2A1,F13.3)
99986 FORMAT (9X,3F8.3,6X,3A1,F12.3)
99985 FORMAT (1X,4F8.3,5X,4A1,F12.3)
99984 FORMAT (1X,4F8.3,5X,4A1,F12.3,3X,F10.3,F9.3)
99983 FORMAT (/' Value of IFAIL parameter on exit from G13DPF = ',I2)
      END
