*     G13DNF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          KMAX, IK, NMAX, MMAX, LWORK
      PARAMETER        (KMAX=3,IK=KMAX,NMAX=100,MMAX=20,LWORK=(5*MMAX+6)
     +                 *KMAX*KMAX+KMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, M, MAXLAG, N
*     .. Local Arrays ..
      DOUBLE PRECISION PARLAG(IK,IK,MMAX), PVALUE(MMAX), R(IK,IK,MMAX),
     +                 R0(IK,KMAX), W(IK,NMAX), WMEAN(KMAX),
     +                 WORK(LWORK), X(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G13DMF, G13DNF, ZPRINT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13DNF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) K, N, M
      IF (K.GT.0 .AND. K.LE.KMAX .AND. N.GE.1 .AND. N.LE.NMAX .AND.
     +    M.GE.1 .AND. M.LE.MMAX) THEN
         DO 20 I = 1, K
            READ (NIN,*) (W(I,J),J=1,N)
   20    CONTINUE
         IFAIL = 0
*
         CALL G13DMF('R-correlation',K,N,M,W,IK,WMEAN,R0,R,IFAIL)
*
         IFAIL = 0
*
         CALL G13DNF(K,N,M,IK,R0,R,MAXLAG,PARLAG,X,PVALUE,WORK,LWORK,
     +               IFAIL)
*
         CALL ZPRINT(K,N,M,IK,PARLAG,X,PVALUE,NOUT)
      END IF
      STOP
*
      END
*
      SUBROUTINE ZPRINT(K,N,M,IK,PARLAG,X,PVALUE,NOUT)
*     .. Scalar Arguments ..
      INTEGER           IK, K, M, N, NOUT
*     .. Array Arguments ..
      DOUBLE PRECISION  PARLAG(IK,IK,M), PVALUE(M), X(M)
*     .. Local Scalars ..
      DOUBLE PRECISION  C1, C2, C3, C5, C6, C7, CONST, SUM
      INTEGER           I, I2, IFAIL2, J, L, LL
*     .. Local Arrays ..
      CHARACTER*1       CLABS(1), RLABS(1)
      CHARACTER*80      REC(7)
*     .. External Subroutines ..
      EXTERNAL          X04CBF
*     .. Intrinsic Functions ..
      INTRINSIC         DBLE, SQRT
*     .. Executable Statements ..
*
*     Print the partial lag correlation matrices.
*
      CONST = 1.0D0/SQRT(DBLE(N))
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' PARTIAL LAG CORRELATION MATRICES'
      WRITE (NOUT,*) ' --------------------------------'
      DO 20 L = 1, M
         WRITE (NOUT,99999) ' Lag = ', L
         IFAIL2 = 0
         CALL X04CBF('G','N',K,K,PARLAG(1,1,L),IK,'F9.3',' ','N',RLABS,
     +               'N',CLABS,80,5,IFAIL2)
   20 CONTINUE
      WRITE (NOUT,99998) ' Standard error = 1 / SQRT(N) = ', CONST
*
*     Print indicator symbols to indicate significant elements.
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' TABLES OF INDICATOR SYMBOLS'
      WRITE (NOUT,*) ' ---------------------------'
      WRITE (NOUT,99999) ' For Lags 1 to ', M
*
*     Set up annotation for the plots.
*
      WRITE (REC(1),99997) '              0.005  :'
      WRITE (REC(2),99997) '        +     0.01   :'
      WRITE (REC(3),99997) '              0.05   :'
      WRITE (REC(4)(1:23),99997) '   Sig. Level        :'
      WRITE (REC(4)(24:),99997) '- - - - - - - - - -  Lags'
      WRITE (REC(5),99997) '              0.05   :'
      WRITE (REC(6),99997) '        -     0.01   :'
      WRITE (REC(7),99997) '              0.005  :'
*
*     Set up the critical values
*
      C1 = 3.29D0*CONST
      C2 = 2.58D0*CONST
      C3 = 1.96D0*CONST
      C5 = -C3
      C6 = -C2
      C7 = -C1
*
      DO 120 I = 1, K
         DO 100 J = 1, K
            WRITE (NOUT,*)
            IF (I.EQ.J) THEN
               WRITE (NOUT,99996) ' Auto-correlation function for',
     +           ' series ', I
            ELSE
               WRITE (NOUT,99995) ' Cross-correlation function for',
     +           ' series ', I, ' and series', J
            END IF
            DO 60 L = 1, M
               LL = 23 + 2*L
               SUM = PARLAG(I,J,L)
*
*              Clear the last plot with blanks
*
               DO 40 I2 = 1, 7
                  IF (I2.NE.4) REC(I2) (LL:LL) = ' '
   40          CONTINUE
*
*              Check for significance
*
               IF (SUM.GT.C1) REC(1) (LL:LL) = '*'
               IF (SUM.GT.C2) REC(2) (LL:LL) = '*'
               IF (SUM.GT.C3) REC(3) (LL:LL) = '*'
               IF (SUM.LT.C5) REC(5) (LL:LL) = '*'
               IF (SUM.LT.C6) REC(6) (LL:LL) = '*'
               IF (SUM.LT.C7) REC(7) (LL:LL) = '*'
   60       CONTINUE
*
*           Print
*
            DO 80 I2 = 1, 7
               WRITE (NOUT,99997) REC(I2)
   80       CONTINUE
  100    CONTINUE
  120 CONTINUE
*
*     Print the chi-square statistics and p-values.
*
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' Lag     Chi-square statistic     P-value'
      WRITE (NOUT,*) ' ---     --------------------     -------'
      WRITE (NOUT,*)
      DO 140 L = 1, M
         WRITE (NOUT,99994) L, X(L), PVALUE(L)
  140 CONTINUE
      RETURN
*
99999 FORMAT (/1X,A,I2)
99998 FORMAT (/1X,A,F5.3,A)
99997 FORMAT (1X,A)
99996 FORMAT (//1X,A,A,I2,/)
99995 FORMAT (//1X,A,A,I2,A,I2,/)
99994 FORMAT (1X,I4,10X,F8.3,11X,F8.4)
      END
