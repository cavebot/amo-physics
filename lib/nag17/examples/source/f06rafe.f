*     F06RAF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          LDA, LDAB, MAXN
      PARAMETER        (LDA=17,LDAB=33,MAXN=6)
      DOUBLE PRECISION RDUMMY, ONE
      PARAMETER        (RDUMMY=-1.0D+10,ONE=1.0D+0)
*     .. Local Scalars ..
      DOUBLE PRECISION DUM, MAXEL, RNORM, TNORM, TOL
      INTEGER          HIGHIN, I, II, IJK, IM, IMAT, IN, INORM, J, JJ,
     +                 K, KK, KL, KU, LL, LOWIN, M, MM, N, NN
      LOGICAL          PASS
      CHARACTER        DIAG, NORM, UPLO
      CHARACTER*3      MATRIX, MATTYP
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,LDA), AB(LDAB,LDA), ACOPY(LDA,LDA),
     +                 AP(LDA*(LDA+1)/2), RWORK(LDA), WORK(LDAB)
      INTEGER          ENS(MAXN), KS(3)
      CHARACTER        DIAGS(2), NORMS(4), UPLOS(2)
      CHARACTER*3      MATS(12)
*     .. External Functions ..
      DOUBLE PRECISION F06RAF, F06RBF, F06RCF, F06RDF, F06REF, F06RJF,
     +                 F06RKF, F06RLF, F06RMF, G05CAF, TRUNRM, X02AJF
      EXTERNAL         F06RAF, F06RBF, F06RCF, F06RDF, F06REF, F06RJF,
     +                 F06RKF, F06RLF, F06RMF, G05CAF, TRUNRM, X02AJF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, MAX, MIN
*     .. Data statements ..
      DATA             ENS/0, 1, 2, 4, 8, 17/
      DATA             NORMS/'M', '1', 'I', 'F'/
      DATA             MATS/'GE', 'SYU', 'SYL', 'TRL', 'TRU', 'GB',
     +                 'SB', 'SP', 'HS', 'TP', 'TBU', 'TBL'/
      DATA             DIAGS/'N', 'U'/
      DATA             UPLOS/'U', 'L'/
      DATA             KS/0, 1, 2/
*     .. Executable Statements ..
      WRITE (NOUT,99999)
      TOL = X02AJF()*100
      PASS = .TRUE.
      DO 1580 INORM = 1, 4
         NORM = NORMS(INORM)
         DO 1560 IMAT = 1, 12
            MATRIX = MATS(IMAT)
            DO 1540 MM = 1, MAXN
               M = ENS(MM)
               DO 1520 NN = 1, MAXN
                  N = ENS(NN)
                  IF ((MATRIX.EQ.'GE' .OR. MATRIX.EQ.'TRL' .OR.
     +                MATRIX.EQ.'TRU') .OR. M.EQ.N) THEN
                     MAXEL = M*G05CAF(DUM)
                     DO 40 I = 1, M
                        DO 20 J = 1, N
                           A(I,J) = MAXEL*G05CAF(DUM)
                           IF (MATRIX.EQ.'TRU' .OR. MATRIX.EQ.'SYU')
     +                         THEN
                              IF (J.LT.I) A(I,J) = RDUMMY
                           ELSE IF (MATRIX.EQ.'TRL' .OR. MATRIX.EQ.
     +                              'SYL') THEN
                              IF (J.GT.I) A(I,J) = RDUMMY
                           END IF
   20                   CONTINUE
   40                CONTINUE
                     IM = G05CAF(DUM)*MIN(M,N) + 1
                     IN = G05CAF(DUM)*MIN(M,N) + 1
                     IF (MATRIX.EQ.'GB') THEN
                        DO 80 I = 1, N
                           DO 60 J = 1, N
                              ACOPY(I,J) = A(I,J)
   60                      CONTINUE
   80                   CONTINUE
                     ELSE IF (MATRIX.NE.'SB') THEN
                        IF (IM.GT.IN) THEN
                           IF (MATRIX.EQ.'TRU' .OR. MATRIX.EQ.'SYU')
     +                         THEN
                              A(IN,IM) = MAXEL
                           ELSE
                              A(IM,IN) = MAXEL
                              IF (MATRIX.EQ.'SP' .OR. MATRIX.EQ.'TP')
     +                            A(IN,IM) = A(IM,IN)
                              IF (MATRIX.EQ.'HS') THEN
                                 DO 120 KK = 1, N - 2
                                    DO 100 LL = KK + 2, N
                                       IF (A(LL,KK).EQ.MAXEL) A(KK,LL)
     +                                     = MAXEL
  100                               CONTINUE
  120                            CONTINUE
                              END IF
                           END IF
                        ELSE IF (IM.LT.IN) THEN
                           IF (MATRIX.EQ.'TRL' .OR. MATRIX.EQ.'SYL')
     +                         THEN
                              A(IN,IM) = MAXEL
                           ELSE
                              A(IM,IN) = MAXEL
                              IF (MATRIX.EQ.'SP' .OR. MATRIX.EQ.'TP')
     +                            A(IN,IM) = A(IM,IN)
                           END IF
                        ELSE
                           A(IM,IN) = MAXEL
                        END IF
                     END IF
                     IF (MATRIX.EQ.'TP') THEN
                        DO 160 I = 1, N
                           DO 140 J = 1, N
                              ACOPY(I,J) = A(I,J)
  140                      CONTINUE
  160                   CONTINUE
                     END IF
*
                     IF (MATRIX.EQ.'GE') THEN
                        RNORM = F06RAF(NORM,M,N,A,LDA,WORK)
                        TNORM = TRUNRM(M,N,MAXEL,A,LDA,NORM,MATRIX,'N')
                        IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM)) THEN
                           WRITE (NOUT,99998) NORM, M, N, LDA,
     +                       RNORM, TNORM
                           PASS = .FALSE.
                           GO TO 1600
                        END IF
                     ELSE IF (MATRIX.EQ.'GB') THEN
                        DO 360 LL = 3, 1, -1
                           KU = KS(LL)
                           IF (KU.LE.N-1 .OR. N.EQ.0) THEN
                              DO 200 I = 1, N
                                 DO 180 J = 1, N
                                    A(I,J) = ACOPY(I,J)
  180                            CONTINUE
  200                         CONTINUE
                              DO 240 II = 1, N - 1 - KU
                                 DO 220 JJ = II + 1 + KU, N
                                    A(II,JJ) = RDUMMY
  220                            CONTINUE
  240                         CONTINUE
                              DO 340 KK = 3, 1, -1
                                 KL = KS(KK)
                                 IF (KL.LE.N-1 .OR. N.EQ.0) THEN
                                    LOWIN = MAX(1,IM-KL)
                                    HIGHIN = MIN(N,IM+KU)
                                    IN = LOWIN + G05CAF(DUM)
     +                                   *(HIGHIN-LOWIN+1)
                                    A(IM,IN) = MAXEL
                                    DO 280 II = 1, N - 1 - KL
                                       DO 260 JJ = II + 1 + KL, N
                                          A(JJ,II) = RDUMMY
  260                                  CONTINUE
  280                               CONTINUE
                                    DO 320 J = 1, N
                                       K = KU + 1 - J
                                       DO 300 I = MAX(1,J-KU),
     +                                         MIN(N,J+KL)
                                          AB(K+I,J) = A(I,J)
  300                                  CONTINUE
  320                               CONTINUE
                                    RNORM = F06RBF(NORM,N,KL,KU,AB,LDAB,
     +                                      WORK)
                                    TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,
     +                                      MATRIX,'N')
                                    IF (ABS(RNORM-TNORM)
     +                                  .GT.ABS(TOL*TNORM)) THEN
                                       WRITE (NOUT,99997) NORM, N,
     +                                   KL, KU, LDAB, RNORM, TNORM
                                       PASS = .FALSE.
                                       GO TO 1600
                                    END IF
                                 END IF
  340                         CONTINUE
                           END IF
  360                   CONTINUE
                     ELSE IF (MATRIX.EQ.'SYU') THEN
                        UPLO = 'U'
                        RNORM = F06RCF(NORM,UPLO,N,A,LDA,WORK)
                        TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,MATRIX,'N')
                        IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM)) THEN
                           WRITE (NOUT,99996) NORM, UPLO, N, LDA,
     +                       RNORM, TNORM
                           PASS = .FALSE.
                           GO TO 1600
                        END IF
                     ELSE IF (MATRIX.EQ.'SYL') THEN
                        UPLO = 'L'
                        RNORM = F06RCF(NORM,UPLO,N,A,LDA,WORK)
                        TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,MATRIX,'N')
                        IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM)) THEN
                           WRITE (NOUT,99996) NORM, UPLO, N, LDA,
     +                       RNORM, TNORM
                           PASS = .FALSE.
                           GO TO 1600
                        END IF
                     ELSE IF (MATRIX.EQ.'SP') THEN
                        DO 460 KK = 1, 2
                           UPLO = UPLOS(KK)
                           IF (UPLO.EQ.'U') THEN
                              DO 400 I = 1, N
                                 DO 380 J = I, N
                                    AP(I+J*(J-1)/2) = A(I,J)
  380                            CONTINUE
  400                         CONTINUE
                              MATTYP = 'SYU'
                           ELSE IF (UPLO.EQ.'L') THEN
                              DO 440 I = 1, N
                                 DO 420 J = 1, I
                                    AP(I+(2*N-J)*(J-1)/2) = A(I,J)
  420                            CONTINUE
  440                         CONTINUE
                              MATTYP = 'SYL'
                           END IF
                           RNORM = F06RDF(NORM,UPLO,N,AP,WORK)
                           TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,MATTYP,
     +                             'N')
                           IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM)) THEN
                              WRITE (NOUT,99995) NORM, UPLO, N,
     +                          RNORM, TNORM
                              PASS = .FALSE.
                              GO TO 1600
                           END IF
  460                   CONTINUE
                     ELSE IF (MATRIX.EQ.'SB') THEN
                        DO 620 LL = 3, 1, -1
                           K = KS(LL)
                           IF (K.LE.N-1 .OR. N.EQ.0) THEN
                              LOWIN = MAX(1,IM-K)
                              HIGHIN = MIN(N,IM+K)
                              IN = LOWIN + G05CAF(DUM)*(HIGHIN-LOWIN+1)
                              A(IM,IN) = MAXEL
                              IF (IM.NE.IN) A(IN,IM) = MAXEL
                              DO 500 II = 1, N - 1 - K
                                 DO 480 JJ = II + 1 + K, N
                                    A(II,JJ) = RDUMMY
                                    A(JJ,II) = A(II,JJ)
  480                            CONTINUE
  500                         CONTINUE
                              DO 600 KK = 1, 2
                                 UPLO = UPLOS(KK)
                                 IF (UPLO.EQ.'U') THEN
                                    DO 540 J = 1, N
                                       IJK = K + 1 - J
                                       DO 520 I = MAX(1,J-K), J
                                          AB(IJK+I,J) = A(I,J)
  520                                  CONTINUE
  540                               CONTINUE
                                    MATTYP = 'SYU'
                                 ELSE IF (UPLO.EQ.'L') THEN
                                    DO 580 J = 1, N
                                       IJK = 1 - J
                                       DO 560 I = J, MIN(N,J+K)
                                          AB(IJK+I,J) = A(I,J)
  560                                  CONTINUE
  580                               CONTINUE
                                    MATTYP = 'SYL'
                                 END IF
                                 RNORM = F06REF(NORM,UPLO,N,K,AB,LDAB,
     +                                   WORK)
                                 TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,
     +                                   MATTYP,'N')
                                 IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM))
     +                               THEN
                                    WRITE (NOUT,99994) NORM, UPLO,
     +                                N, K, LDAB, RNORM, TNORM
                                    PASS = .FALSE.
                                    GO TO 1600
                                 END IF
  600                         CONTINUE
                           END IF
  620                   CONTINUE
                     ELSE IF (MATRIX.EQ.'TRL') THEN
                        UPLO = 'L'
                        DO 680 KK = 1, 2
                           DIAG = DIAGS(KK)
                           IF (DIAG.EQ.'U') THEN
                              DO 640 LL = 1, MIN(M,N)
                                 A(LL,LL) = RDUMMY
  640                         CONTINUE
                           END IF
                           RNORM = F06RJF(NORM,UPLO,DIAG,M,N,A,LDA,WORK)
                           IF (DIAG.EQ.'U') THEN
                              DO 660 LL = 1, MIN(M,N)
                                 A(LL,LL) = ONE
  660                         CONTINUE
                           END IF
                           TNORM = TRUNRM(M,N,MAXEL,A,LDA,NORM,MATRIX,
     +                             DIAG)
                           IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM)) THEN
                              WRITE (NOUT,99993) NORM, UPLO, DIAG,
     +                          M, N, LDA, RNORM, TNORM
                              PASS = .FALSE.
                              GO TO 1600
                           END IF
  680                   CONTINUE
                     ELSE IF (MATRIX.EQ.'TRU') THEN
                        UPLO = 'U'
                        DO 740 KK = 1, 2
                           DIAG = DIAGS(KK)
                           IF (DIAG.EQ.'U') THEN
                              DO 700 LL = 1, MIN(M,N)
                                 A(LL,LL) = RDUMMY
  700                         CONTINUE
                           END IF
                           RNORM = F06RJF(NORM,UPLO,DIAG,M,N,A,LDA,WORK)
                           IF (DIAG.EQ.'U') THEN
                              DO 720 LL = 1, MIN(M,N)
                                 A(LL,LL) = ONE
  720                         CONTINUE
                           END IF
                           TNORM = TRUNRM(M,N,MAXEL,A,LDA,NORM,MATRIX,
     +                             DIAG)
                           IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM)) THEN
                              WRITE (NOUT,99993) NORM, UPLO, DIAG,
     +                          M, N, LDA, RNORM, TNORM
                              PASS = .FALSE.
                              GO TO 1600
                           END IF
  740                   CONTINUE
                     ELSE IF (MATRIX.EQ.'TP') THEN
                        DO 1020 KK = 1, 2
                           DIAG = DIAGS(KK)
                           DO 1000 JJ = 1, 2
                              IF (NORM.NE.'M') THEN
                                 DO 780 I = 1, N
                                    DO 760 J = 1, N
                                       A(I,J) = RDUMMY
  760                               CONTINUE
  780                            CONTINUE
                              END IF
                              UPLO = UPLOS(JJ)
                              IF (UPLO.EQ.'U') THEN
                                 DO 820 I = 1, N
                                    DO 800 J = I, N
                                       A(I,J) = ACOPY(I,J)
  800                               CONTINUE
  820                            CONTINUE
                                 MATTYP = 'TRU'
                              ELSE IF (UPLO.EQ.'L') THEN
                                 DO 860 I = 1, N
                                    DO 840 J = 1, I
                                       A(I,J) = ACOPY(I,J)
  840                               CONTINUE
  860                            CONTINUE
                                 MATTYP = 'TRL'
                              END IF
                              IF (DIAG.EQ.'U') THEN
                                 DO 880 LL = 1, N
                                    A(LL,LL) = RDUMMY
  880                            CONTINUE
                              END IF
                              IF (UPLO.EQ.'U') THEN
                                 DO 920 I = 1, N
                                    DO 900 J = I, N
                                       AP(I+J*(J-1)/2) = A(I,J)
  900                               CONTINUE
  920                            CONTINUE
                              ELSE IF (UPLO.EQ.'L') THEN
                                 DO 960 I = 1, N
                                    DO 940 J = 1, I
                                       AP(I+(2*N-J)*(J-1)/2) = A(I,J)
  940                               CONTINUE
  960                            CONTINUE
                              END IF
                              RNORM = F06RKF(NORM,UPLO,DIAG,N,AP,WORK)
                              IF (DIAG.EQ.'U') THEN
                                 DO 980 LL = 1, N
                                    A(LL,LL) = ONE
  980                            CONTINUE
                              END IF
                              TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,
     +                                MATTYP,DIAG)
                              IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM))
     +                            THEN
                                 WRITE (NOUT,99992) NORM, UPLO,
     +                             DIAG, N, RNORM, TNORM
                                 PASS = .FALSE.
                                 GO TO 1600
                              END IF
 1000                      CONTINUE
 1020                   CONTINUE
                     ELSE IF (MATRIX.EQ.'TBU') THEN
                        UPLO = 'U'
                        DO 1060 I = 1, N - 1
                           DO 1040 J = (I+1), N
                              A(J,I) = RDUMMY
 1040                      CONTINUE
 1060                   CONTINUE
                        DO 1240 LL = 3, 1, -1
                           K = KS(LL)
                           IF (K.LE.N-1 .OR. N.EQ.0) THEN
                              DO 1100 I = 1, N - 1 - K
                                 DO 1080 J = I + 1 + K, N
                                    A(I,J) = RDUMMY
 1080                            CONTINUE
 1100                         CONTINUE
                              LOWIN = MAX(1,IM-K)
                              HIGHIN = MIN(N,IM+K)
                              IN = LOWIN + G05CAF(DUM)*(HIGHIN-LOWIN+1)
                              IF (IM.LE.IN) THEN
                                 A(IM,IN) = MAXEL
                              ELSE
                                 A(IN,IM) = MAXEL
                              END IF
                              DO 1220 KK = 1, 2
                                 DIAG = DIAGS(KK)
                                 IF (DIAG.EQ.'U') THEN
                                    DO 1120 IJK = 1, N
                                       RWORK(IJK) = A(IJK,IJK)
                                       A(IJK,IJK) = RDUMMY
 1120                               CONTINUE
                                 END IF
                                 DO 1160 J = 1, N
                                    IJK = K + 1 - J
                                    DO 1140 I = MAX(1,J-K), J
                                       AB(IJK+I,J) = A(I,J)
 1140                               CONTINUE
 1160                            CONTINUE
                                 RNORM = F06RLF(NORM,UPLO,DIAG,N,K,AB,
     +                                   LDAB,WORK)
                                 IF (DIAG.EQ.'U') THEN
                                    DO 1180 IJK = 1, N
                                       A(IJK,IJK) = ONE
 1180                               CONTINUE
                                 END IF
                                 TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,
     +                                   MATRIX,'Z')
                                 IF (DIAG.EQ.'U') THEN
                                    DO 1200 IJK = 1, N
                                       A(IJK,IJK) = RWORK(IJK)
 1200                               CONTINUE
                                 END IF
                                 IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM))
     +                               THEN
                                    WRITE (NOUT,99991) NORM, UPLO,
     +                                DIAG, N, K, LDA, RNORM, TNORM
                                    PASS = .FALSE.
                                    GO TO 1600
                                 END IF
 1220                         CONTINUE
                           END IF
 1240                   CONTINUE
                     ELSE IF (MATRIX.EQ.'TBL') THEN
                        UPLO = 'L'
                        DO 1280 I = 1, N - 1
                           DO 1260 J = (I+1), N
                              A(I,J) = RDUMMY
 1260                      CONTINUE
 1280                   CONTINUE
                        DO 1460 LL = 3, 1, -1
                           K = KS(LL)
                           IF (K.LE.N-1 .OR. N.EQ.0) THEN
                              DO 1320 I = 1, N - 1 - K
                                 DO 1300 J = I + 1 + K, N
                                    A(J,I) = RDUMMY
 1300                            CONTINUE
 1320                         CONTINUE
                              LOWIN = MAX(1,IM-K)
                              HIGHIN = MIN(N,IM+K)
                              IN = LOWIN + G05CAF(DUM)*(HIGHIN-LOWIN+1)
                              IF (IM.GE.IN) THEN
                                 A(IM,IN) = MAXEL
                              ELSE
                                 A(IN,IM) = MAXEL
                              END IF
                              DO 1440 KK = 1, 2
                                 DIAG = DIAGS(KK)
                                 IF (DIAG.EQ.'U') THEN
                                    DO 1340 IJK = 1, N
                                       RWORK(IJK) = A(IJK,IJK)
                                       A(IJK,IJK) = RDUMMY
 1340                               CONTINUE
                                 END IF
                                 DO 1380 J = 1, N
                                    IJK = 1 - J
                                    DO 1360 I = J, MIN(N,J+K)
                                       AB(IJK+I,J) = A(I,J)
 1360                               CONTINUE
 1380                            CONTINUE
                                 RNORM = F06RLF(NORM,UPLO,DIAG,N,K,AB,
     +                                   LDAB,WORK)
                                 IF (DIAG.EQ.'U') THEN
                                    DO 1400 IJK = 1, N
                                       A(IJK,IJK) = ONE
 1400                               CONTINUE
                                 END IF
                                 TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,
     +                                   MATRIX,'Z')
                                 IF (DIAG.EQ.'U') THEN
                                    DO 1420 IJK = 1, N
                                       A(IJK,IJK) = RWORK(IJK)
 1420                               CONTINUE
                                 END IF
                                 IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM))
     +                               THEN
                                    WRITE (NOUT,99991) NORM, UPLO,
     +                                DIAG, N, K, LDA, RNORM, TNORM
                                    PASS = .FALSE.
                                    GO TO 1600
                                 END IF
 1440                         CONTINUE
                           END IF
 1460                   CONTINUE
                     ELSE IF (MATRIX.EQ.'HS') THEN
                        DO 1500 KK = 1, N - 2
                           DO 1480 LL = KK + 2, N
                              A(LL,KK) = RDUMMY
 1480                      CONTINUE
 1500                   CONTINUE
                        RNORM = F06RMF(NORM,N,A,LDA,WORK)
                        TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,MATRIX,'N')
                        IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM)) THEN
                           WRITE (NOUT,99990) NORM, N, LDA, RNORM,
     +                       TNORM
                           PASS = .FALSE.
                           GO TO 1600
                        END IF
                     END IF
                  END IF
 1520          CONTINUE
 1540       CONTINUE
 1560    CONTINUE
 1580 CONTINUE
 1600 IF (PASS) THEN
         WRITE (NOUT,99989)
      ELSE
         WRITE (NOUT,99988)
      END IF
      STOP
*
99999 FORMAT (' F06RAF Example Program Results',/1X)
99998 FORMAT (' The call',/' CALL F06RAF(''',A,''',',I3,',',I3,',A,',I3,
     +       ',WORK)',/' returned norm(A) as ',1P,D13.5,' instead of ',
     +       D13.5,' .')
99997 FORMAT (' The call',/' CALL F06RBF(''',A,''',',I3,',',I3,',',I3,
     +       ',A,',I3,',WORK)',/' returned norm(A) as ',1P,D13.5,' ins',
     +       'tead of ',D13.5,' .')
99996 FORMAT (' The call',/' CALL F06RCF(''',A,''',''',A,''',',I3,',A,',
     +       I3,',WORK)',/' returned norm(A) as ',1P,D13.5,' instead o',
     +       'f ',D13.5,' .')
99995 FORMAT (' The call',/' CALL F06RDF(''',A,''',''',A,''',',I3,',AP',
     +       ',WORK)',/' returned norm(A) as ',1P,D13.5,' instead of ',
     +       D13.5,' .')
99994 FORMAT (' The call',/' CALL F06REF(''',A,''',''',A,''',',I3,',',
     +       I3,',A,',I3,',WORK)',/' returned norm(A) as ',1P,D13.5,
     +       ' instead of ',D13.5,' .')
99993 FORMAT (' The call',/' CALL F06RJF(''',A,''',''',A,''',''',A,
     +       ''',',I3,',',I3,',A,',I3,',WORK)',/' returned norm(A) as ',
     +       1P,D13.5,' instead of ',D13.5,' .')
99992 FORMAT (' The call',/' CALL F06RKF(''',A,''',''',A,''',''',A,
     +       ''',',I3,',AP,WORK)',/' returned norm(A) as ',1P,D13.5,
     +       ' instead of ',D13.5,' .')
99991 FORMAT (' The call',/' CALL F06RLF(''',A,''',''',A,''',''',A,
     +       ''',',I3,',',I3,',A,',I3,',WORK)',/' returned norm(A) as ',
     +       1P,D13.5,' instead of ',D13.5,' .')
99990 FORMAT (' The call',/' CALL F06RMF(''',A,''',',I3,',A,',I3,',WOR',
     +       'K)',/' returned norm(A) as ',1P,D13.5,' instead of ',
     +       D13.5,' .')
99989 FORMAT (' F06RAF Example Program ends OK')
99988 FORMAT (' F06RAF Example Program ends with ERRORS')
      END
      DOUBLE PRECISION FUNCTION TRUNRM(M,N,MAXEL,A,LDA,NORM,MATRIX,DIAG)
*     .. Parameters ..
      DOUBLE PRECISION                 RDUMMY, ZERO, ONE
      PARAMETER                        (RDUMMY=-1.0D+10,ZERO=0.0D+0,
     +                                 ONE=1.0D+0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION                 MAXEL
      INTEGER                          LDA, M, N
      CHARACTER                        DIAG, NORM
      CHARACTER*3                      MATRIX
*     .. Array Arguments ..
      DOUBLE PRECISION                 A(LDA,*)
*     .. Local Scalars ..
      DOUBLE PRECISION                 COLSUM, ROWSUM
      INTEGER                          I, J
      LOGICAL                          SYMMET
*     .. Intrinsic Functions ..
      INTRINSIC                        ABS, MAX, MIN, SQRT
*     .. Executable Statements ..
      SYMMET = MATRIX .EQ. 'SYU' .OR. MATRIX .EQ. 'SYL'
      IF (NORM.EQ.'M') THEN
         IF (MIN(M,N).GT.ZERO) THEN
            TRUNRM = ABS(MAXEL)
            IF (DIAG.EQ.'U') THEN
               IF (MATRIX.EQ.'TRL') THEN
                  TRUNRM = ZERO
                  DO 40 I = 1, M
                     DO 20 J = 1, MIN(I,N)
                        TRUNRM = MAX(TRUNRM,ABS(A(I,J)))
   20                CONTINUE
   40             CONTINUE
               ELSE IF (MATRIX.EQ.'TRU') THEN
                  TRUNRM = ZERO
                  DO 80 I = 1, M
                     DO 60 J = I, N
                        TRUNRM = MAX(TRUNRM,ABS(A(I,J)))
   60                CONTINUE
   80             CONTINUE
               END IF
            ELSE IF (MATRIX.EQ.'TBU') THEN
               TRUNRM = ZERO
               DO 120 I = 1, N
                  DO 100 J = I, N
                     IF (A(I,J).NE.RDUMMY) TRUNRM = MAX(TRUNRM,
     +                   ABS(A(I,J)))
  100             CONTINUE
  120          CONTINUE
            ELSE IF (MATRIX.EQ.'TBL') THEN
               TRUNRM = ZERO
               DO 160 I = 1, N
                  DO 140 J = 1, I
                     IF (A(I,J).NE.RDUMMY) TRUNRM = MAX(TRUNRM,
     +                   ABS(A(I,J)))
  140             CONTINUE
  160          CONTINUE
            END IF
         ELSE
            TRUNRM = ZERO
         END IF
      ELSE IF (NORM.EQ.'1') THEN
         TRUNRM = ZERO
         IF (MATRIX.EQ.'TBU') THEN
            DO 200 I = 1, N
               COLSUM = ZERO
               DO 180 J = 1, I
                  IF (A(J,I).NE.RDUMMY) COLSUM = COLSUM + ABS(A(J,I))
  180          CONTINUE
               TRUNRM = MAX(TRUNRM,COLSUM)
  200       CONTINUE
         ELSE IF (MATRIX.EQ.'TBL') THEN
            DO 240 I = 1, N
               COLSUM = ZERO
               DO 220 J = I, N
                  IF (A(J,I).NE.RDUMMY) COLSUM = COLSUM + ABS(A(J,I))
  220          CONTINUE
               TRUNRM = MAX(TRUNRM,COLSUM)
  240       CONTINUE
         ELSE
            DO 280 J = 1, N
               COLSUM = ZERO
               DO 260 I = 1, M
                  IF (MATRIX.EQ.'GE') THEN
                     COLSUM = COLSUM + ABS(A(I,J))
                  ELSE IF (MATRIX.EQ.'TRU') THEN
                     IF (I.LT.J) THEN
                        COLSUM = COLSUM + ABS(A(I,J))
                     ELSE IF (I.EQ.J) THEN
                        IF (DIAG.EQ.'N') THEN
                           COLSUM = COLSUM + ABS(A(I,J))
                        ELSE IF (DIAG.EQ.'U') THEN
                           COLSUM = COLSUM + ONE
                        END IF
                     END IF
                  ELSE IF (MATRIX.EQ.'TRL') THEN
                     IF (I.GT.J) THEN
                        COLSUM = COLSUM + ABS(A(I,J))
                     ELSE IF (I.EQ.J) THEN
                        IF (DIAG.EQ.'N') THEN
                           COLSUM = COLSUM + ABS(A(I,J))
                        ELSE IF (DIAG.EQ.'U') THEN
                           COLSUM = COLSUM + ONE
                        END IF
                     END IF
                  ELSE IF (MATRIX.EQ.'SYU') THEN
                     IF (I.LE.J) THEN
                        IF (A(I,J).NE.RDUMMY) COLSUM = COLSUM +
     +                      ABS(A(I,J))
                     ELSE IF (I.GT.J) THEN
                        IF (A(J,I).NE.RDUMMY) COLSUM = COLSUM +
     +                      ABS(A(J,I))
                     END IF
                  ELSE IF (MATRIX.EQ.'SYL') THEN
                     IF (I.GE.J) THEN
                        IF (A(I,J).NE.RDUMMY) COLSUM = COLSUM +
     +                      ABS(A(I,J))
                     ELSE IF (I.LT.J) THEN
                        IF (A(J,I).NE.RDUMMY) COLSUM = COLSUM +
     +                      ABS(A(J,I))
                     END IF
                  ELSE IF (MATRIX.EQ.'HS' .OR. MATRIX.EQ.'GB') THEN
                     IF (A(I,J).NE.RDUMMY) COLSUM = COLSUM + ABS(A(I,J))
                  END IF
  260          CONTINUE
               TRUNRM = MAX(TRUNRM,COLSUM)
  280       CONTINUE
         END IF
      ELSE IF (NORM.EQ.'I') THEN
         TRUNRM = ZERO
         IF (MATRIX.EQ.'TBU') THEN
            DO 320 I = 1, N
               ROWSUM = ZERO
               DO 300 J = I, N
                  IF (A(I,J).NE.RDUMMY) ROWSUM = ROWSUM + ABS(A(I,J))
  300          CONTINUE
               TRUNRM = MAX(TRUNRM,ROWSUM)
  320       CONTINUE
         ELSE IF (MATRIX.EQ.'TBL') THEN
            DO 360 I = 1, N
               ROWSUM = ZERO
               DO 340 J = 1, I
                  IF (A(I,J).NE.RDUMMY) ROWSUM = ROWSUM + ABS(A(I,J))
  340          CONTINUE
               TRUNRM = MAX(TRUNRM,ROWSUM)
  360       CONTINUE
         ELSE
            DO 400 I = 1, M
               ROWSUM = ZERO
               DO 380 J = 1, N
                  IF (MATRIX.EQ.'GE') THEN
                     ROWSUM = ROWSUM + ABS(A(I,J))
                  ELSE IF (MATRIX.EQ.'TRU') THEN
                     IF (I.LT.J) THEN
                        ROWSUM = ROWSUM + ABS(A(I,J))
                     ELSE IF (I.EQ.J) THEN
                        IF (DIAG.EQ.'N') THEN
                           ROWSUM = ROWSUM + ABS(A(I,J))
                        ELSE IF (DIAG.EQ.'U') THEN
                           ROWSUM = ROWSUM + ONE
                        END IF
                     END IF
                  ELSE IF (MATRIX.EQ.'TRL') THEN
                     IF (I.GT.J) THEN
                        ROWSUM = ROWSUM + ABS(A(I,J))
                     ELSE IF (I.EQ.J) THEN
                        IF (DIAG.EQ.'N') THEN
                           ROWSUM = ROWSUM + ABS(A(I,J))
                        ELSE IF (DIAG.EQ.'U') THEN
                           ROWSUM = ROWSUM + ONE
                        END IF
                     END IF
                  ELSE IF (MATRIX.EQ.'SYU') THEN
                     IF (I.LE.J) THEN
                        IF (A(I,J).NE.RDUMMY) ROWSUM = ROWSUM +
     +                      ABS(A(I,J))
                     ELSE IF (I.GT.J) THEN
                        IF (A(J,I).NE.RDUMMY) ROWSUM = ROWSUM +
     +                      ABS(A(J,I))
                     END IF
                  ELSE IF (MATRIX.EQ.'SYL') THEN
                     IF (I.GE.J) THEN
                        IF (A(I,J).NE.RDUMMY) ROWSUM = ROWSUM +
     +                      ABS(A(I,J))
                     ELSE IF (I.LT.J) THEN
                        IF (A(J,I).NE.RDUMMY) ROWSUM = ROWSUM +
     +                      ABS(A(J,I))
                     END IF
                  ELSE IF (MATRIX.EQ.'HS' .OR. MATRIX.EQ.'GB') THEN
                     IF (A(I,J).NE.RDUMMY) ROWSUM = ROWSUM + ABS(A(I,J))
                  END IF
  380          CONTINUE
               TRUNRM = MAX(TRUNRM,ROWSUM)
  400       CONTINUE
         END IF
      ELSE IF (NORM.EQ.'F') THEN
         TRUNRM = ZERO
         IF (MATRIX.EQ.'TBU') THEN
            DO 440 I = 1, N
               DO 420 J = I, N
                  IF (A(I,J).NE.RDUMMY) TRUNRM = TRUNRM + A(I,J)**2
  420          CONTINUE
  440       CONTINUE
            TRUNRM = SQRT(TRUNRM)
         ELSE IF (MATRIX.EQ.'TBL') THEN
            DO 480 I = 1, N
               DO 460 J = 1, I
                  IF (A(I,J).NE.RDUMMY) TRUNRM = TRUNRM + A(I,J)**2
  460          CONTINUE
  480       CONTINUE
            TRUNRM = SQRT(TRUNRM)
         ELSE IF ( .NOT. SYMMET) THEN
            DO 520 I = 1, M
               DO 500 J = 1, N
                  IF (A(I,J).NE.RDUMMY) TRUNRM = TRUNRM + A(I,J)**2
  500          CONTINUE
  520       CONTINUE
            TRUNRM = SQRT(TRUNRM)
         ELSE IF (MATRIX.EQ.'SYU') THEN
            DO 560 I = 1, M
               DO 540 J = I, M
                  IF (I.NE.J .AND. A(I,J).NE.RDUMMY) TRUNRM = TRUNRM +
     +                2*A(I,J)**2
  540          CONTINUE
               TRUNRM = TRUNRM + A(I,I)**2
  560       CONTINUE
            TRUNRM = SQRT(TRUNRM)
         ELSE IF (MATRIX.EQ.'SYL') THEN
            DO 600 I = 1, M
               DO 580 J = 1, I
                  IF (I.NE.J .AND. A(I,J).NE.RDUMMY) TRUNRM = TRUNRM +
     +                2*A(I,J)**2
  580          CONTINUE
               TRUNRM = TRUNRM + A(I,I)**2
  600       CONTINUE
            TRUNRM = SQRT(TRUNRM)
         END IF
      END IF
      RETURN
      END
