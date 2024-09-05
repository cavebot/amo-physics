*     F06UAF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          LDA, LDAB, MAXN
      PARAMETER        (LDA=17,LDAB=33,MAXN=6)
      DOUBLE PRECISION RDUMMY, ZERO, ONE
      PARAMETER        (RDUMMY=-1.0D+10,ZERO=0.0D+0,ONE=1.0D+0)
*     .. Local Scalars ..
      COMPLEX*16       CDUMMY, MAXEL
      DOUBLE PRECISION DUM, RNORM, TNORM, TOL
      INTEGER          HIGHIN, I, II, IJK, IM, IMAT, IN, INORM, J, JJ,
     +                 K, KK, KL, KU, LL, LOWIN, M, MM, N, NN
      LOGICAL          PASS
      CHARACTER        DIAG, NORM, UPLO
      CHARACTER*3      MATRIX, MATTYP
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,LDA), AB(LDAB,LDA), ACOPY(LDA,LDA),
     +                 AP(LDA*(LDA+1)/2), CWORK(LDA)
      DOUBLE PRECISION WORK(LDAB)
      INTEGER          ENS(MAXN), KS(3)
      CHARACTER        DIAGS(2), NORMS(4), UPLOS(2)
      CHARACTER*3      MATS(16)
*     .. External Functions ..
      DOUBLE PRECISION F06UAF, F06UBF, F06UCF, F06UDF, F06UEF, F06UFF,
     +                 F06UGF, F06UHF, F06UJF, F06UKF, F06ULF, F06UMF,
     +                 G05CAF, TRUNRM, X02AJF
      EXTERNAL         F06UAF, F06UBF, F06UCF, F06UDF, F06UEF, F06UFF,
     +                 F06UGF, F06UHF, F06UJF, F06UKF, F06ULF, F06UMF,
     +                 G05CAF, TRUNRM, X02AJF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, DBLE, DCMPLX, MAX, MIN
*     .. Data statements ..
      DATA             ENS/0, 1, 2, 4, 8, 17/
      DATA             NORMS/'M', '1', 'I', 'F'/
      DATA             MATS/'GE', 'SYU', 'SYL', 'TRL', 'TRU', 'GB',
     +                 'SB', 'SP', 'HS', 'TP', 'TBU', 'TBL', 'HEU',
     +                 'HEL', 'HB', 'HP'/
      DATA             DIAGS/'N', 'U'/
      DATA             UPLOS/'U', 'L'/
      DATA             KS/0, 1, 2/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F06UAF Example Program Results'
      CDUMMY = DCMPLX(RDUMMY,RDUMMY)
      TOL = X02AJF()*100
      PASS = .TRUE.
      DO 1620 INORM = 1, 4
         NORM = NORMS(INORM)
         DO 1600 IMAT = 1, 16
            MATRIX = MATS(IMAT)
            DO 1580 MM = 1, MAXN
               M = ENS(MM)
               DO 1560 NN = 1, MAXN
                  N = ENS(NN)
                  IF ((MATRIX.EQ.'GE' .OR. MATRIX.EQ.'TRL' .OR.
     +                MATRIX.EQ.'TRU') .OR. M.EQ.N) THEN
                     MAXEL = M*DCMPLX(G05CAF(DUM),G05CAF(DUM))
                     DO 40 I = 1, M
                        DO 20 J = 1, N
                           A(I,J) = MAXEL*G05CAF(DUM)
                           IF (MATRIX.EQ.'TRU' .OR. MATRIX.EQ.'SYU' .OR.
     +                         MATRIX.EQ.'HEU') THEN
                              IF (J.LT.I) A(I,J) = CDUMMY
                           ELSE IF (MATRIX.EQ.'TRL' .OR. MATRIX.EQ.
     +                              'SYL' .OR. MATRIX.EQ.'HEL') THEN
                              IF (J.GT.I) A(I,J) = CDUMMY
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
                     ELSE IF (MATRIX.NE.'SB' .OR. MATRIX.NE.'HB') THEN
                        IF (IM.GT.IN) THEN
                           IF (MATRIX.EQ.'TRU' .OR. MATRIX.EQ.'SYU' .OR.
     +                         MATRIX.EQ.'HEU') THEN
                              A(IN,IM) = MAXEL
                           ELSE
                              A(IM,IN) = MAXEL
                              IF (MATRIX.EQ.'SP' .OR. MATRIX.EQ.
     +                            'HP' .OR. MATRIX.EQ.'TP') A(IN,IM)
     +                            = A(IM,IN)
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
                           IF (MATRIX.EQ.'TRL' .OR. MATRIX.EQ.'SYL' .OR.
     +                         MATRIX.EQ.'HEL') THEN
                              A(IN,IM) = MAXEL
                           ELSE
                              A(IM,IN) = MAXEL
                              IF (MATRIX.EQ.'SP' .OR. MATRIX.EQ.
     +                            'HP' .OR. MATRIX.EQ.'TP') A(IN,IM)
     +                            = A(IM,IN)
                           END IF
                        ELSE
                           IF (MATRIX.EQ.'HEU' .OR. MATRIX.EQ.'HEL' .OR.
     +                         MATRIX.EQ.'HP') MAXEL = DCMPLX(ABS(MAXEL)
     +                         ,ZERO)
                           A(IM,IN) = MAXEL
                        END IF
                     END IF
                     IF (MATRIX.EQ.'TP') THEN
                        DO 160 I = 1, N
                           DO 140 J = 1, N
                              ACOPY(I,J) = A(I,J)
  140                      CONTINUE
  160                   CONTINUE
                     ELSE IF (MATRIX.EQ.'HEU' .OR. MATRIX.EQ.'HEL' .OR.
     +                        MATRIX.EQ.'HP') THEN
                        DO 180 I = 1, N
                           A(I,I) = DCMPLX(DBLE(A(I,I)),ZERO)
  180                   CONTINUE
                     END IF
*
                     IF (MATRIX.EQ.'GE') THEN
                        RNORM = F06UAF(NORM,M,N,A,LDA,WORK)
                        TNORM = TRUNRM(M,N,MAXEL,A,LDA,NORM,MATRIX,'N')
                        IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM)) THEN
                           WRITE (NOUT,99999) NORM, M, N, LDA,
     +                       RNORM, TNORM
                           PASS = .FALSE.
                           GO TO 1640
                        END IF
                     ELSE IF (MATRIX.EQ.'GB') THEN
                        DO 380 LL = 3, 1, -1
                           KU = KS(LL)
                           IF (KU.LE.N-1 .OR. N.EQ.0) THEN
                              DO 220 I = 1, N
                                 DO 200 J = 1, N
                                    A(I,J) = ACOPY(I,J)
  200                            CONTINUE
  220                         CONTINUE
                              DO 260 II = 1, N - 1 - KU
                                 DO 240 JJ = II + 1 + KU, N
                                    A(II,JJ) = CDUMMY
  240                            CONTINUE
  260                         CONTINUE
                              DO 360 KK = 3, 1, -1
                                 KL = KS(KK)
                                 IF (KL.LE.N-1 .OR. N.EQ.0) THEN
                                    LOWIN = MAX(1,IM-KL)
                                    HIGHIN = MIN(N,IM+KU)
                                    IN = LOWIN + G05CAF(DUM)
     +                                   *(HIGHIN-LOWIN+1)
                                    A(IM,IN) = MAXEL
                                    DO 300 II = 1, N - 1 - KL
                                       DO 280 JJ = II + 1 + KL, N
                                          A(JJ,II) = CDUMMY
  280                                  CONTINUE
  300                               CONTINUE
                                    DO 340 J = 1, N
                                       K = KU + 1 - J
                                       DO 320 I = MAX(1,J-KU),
     +                                         MIN(N,J+KL)
                                          AB(K+I,J) = A(I,J)
  320                                  CONTINUE
  340                               CONTINUE
                                    RNORM = F06UBF(NORM,N,KL,KU,AB,LDAB,
     +                                      WORK)
                                    TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,
     +                                      MATRIX,'N')
                                    IF (ABS(RNORM-TNORM)
     +                                  .GT.ABS(TOL*TNORM)) THEN
                                       WRITE (NOUT,99998) NORM, N,
     +                                   KL, KU, LDAB, RNORM, TNORM
                                       PASS = .FALSE.
                                       GO TO 1640
                                    END IF
                                 END IF
  360                         CONTINUE
                           END IF
  380                   CONTINUE
                     ELSE IF (MATRIX.EQ.'SYU' .OR. MATRIX.EQ.'HEU') THEN
                        UPLO = 'U'
                        IF (MATRIX.EQ.'HEU') THEN
                           RNORM = F06UCF(NORM,UPLO,N,A,LDA,WORK)
                        ELSE IF (MATRIX.EQ.'SYU') THEN
                           RNORM = F06UFF(NORM,UPLO,N,A,LDA,WORK)
                        END IF
                        TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,MATRIX,'N')
                        IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM)) THEN
                           IF (MATRIX.EQ.'HEU') THEN
                              WRITE (NOUT,99997) NORM, UPLO, N, LDA,
     +                          RNORM, TNORM
                           ELSE IF (MATRIX.EQ.'SYU') THEN
                              WRITE (NOUT,99994) NORM, UPLO, N, LDA,
     +                          RNORM, TNORM
                           END IF
                           PASS = .FALSE.
                           GO TO 1640
                        END IF
                     ELSE IF (MATRIX.EQ.'SYL' .OR. MATRIX.EQ.'HEL') THEN
                        UPLO = 'L'
                        IF (MATRIX.EQ.'HEL') THEN
                           RNORM = F06UCF(NORM,UPLO,N,A,LDA,WORK)
                        ELSE IF (MATRIX.EQ.'SYL') THEN
                           RNORM = F06UFF(NORM,UPLO,N,A,LDA,WORK)
                        END IF
                        TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,MATRIX,'N')
                        IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM)) THEN
                           IF (MATRIX.EQ.'HEL') THEN
                              WRITE (NOUT,99997) NORM, UPLO, N, LDA,
     +                          RNORM, TNORM
                           ELSE IF (MATRIX.EQ.'SYL') THEN
                              WRITE (NOUT,99994) NORM, UPLO, N, LDA,
     +                          RNORM, TNORM
                           END IF
                           PASS = .FALSE.
                           GO TO 1640
                        END IF
                     ELSE IF (MATRIX.EQ.'SP' .OR. MATRIX.EQ.'HP') THEN
                        DO 480 KK = 1, 2
                           UPLO = UPLOS(KK)
                           IF (UPLO.EQ.'U') THEN
                              DO 420 I = 1, N
                                 DO 400 J = I, N
                                    AP(I+J*(J-1)/2) = A(I,J)
  400                            CONTINUE
  420                         CONTINUE
                              IF (MATRIX.EQ.'SP') THEN
                                 MATTYP = 'SYU'
                              ELSE IF (MATRIX.EQ.'HP') THEN
                                 MATTYP = 'HEU'
                              END IF
                           ELSE IF (UPLO.EQ.'L') THEN
                              DO 460 I = 1, N
                                 DO 440 J = 1, I
                                    AP(I+(2*N-J)*(J-1)/2) = A(I,J)
  440                            CONTINUE
  460                         CONTINUE
                              IF (MATRIX.EQ.'SP') THEN
                                 MATTYP = 'SYL'
                              ELSE IF (MATRIX.EQ.'HP') THEN
                                 MATTYP = 'HEL'
                              END IF
                           END IF
                           IF (MATRIX.EQ.'HP') THEN
                              RNORM = F06UDF(NORM,UPLO,N,AP,WORK)
                           ELSE IF (MATRIX.EQ.'SP') THEN
                              RNORM = F06UGF(NORM,UPLO,N,AP,WORK)
                           END IF
                           TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,MATTYP,
     +                             'N')
                           IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM)) THEN
                              IF (MATRIX.EQ.'HP') THEN
                                 WRITE (NOUT,99996) NORM, UPLO, N,
     +                             RNORM, TNORM
                              ELSE IF (MATRIX.EQ.'SP') THEN
                                 WRITE (NOUT,99993) NORM, UPLO, N,
     +                             RNORM, TNORM
                              END IF
                              PASS = .FALSE.
                              GO TO 1640
                           END IF
  480                   CONTINUE
                     ELSE IF (MATRIX.EQ.'SB' .OR. MATRIX.EQ.'HB') THEN
                        DO 660 LL = 3, 1, -1
                           K = KS(LL)
                           IF (K.LE.N-1 .OR. N.EQ.0) THEN
                              LOWIN = MAX(1,IM-K)
                              HIGHIN = MIN(N,IM+K)
                              IN = LOWIN + G05CAF(DUM)*(HIGHIN-LOWIN+1)
                              IF (MATRIX.EQ.'HB' .AND. IM.EQ.IN)
     +                            MAXEL = DCMPLX(ABS(MAXEL),ZERO)
                              A(IM,IN) = MAXEL
                              IF (IM.NE.IN) A(IN,IM) = MAXEL
                              DO 520 II = 1, N - 1 - K
                                 DO 500 JJ = II + 1 + K, N
                                    A(II,JJ) = CDUMMY
                                    A(JJ,II) = A(II,JJ)
  500                            CONTINUE
  520                         CONTINUE
                              IF (MATRIX.EQ.'HB') THEN
                                 DO 540 II = 1, N
                                    A(II,II) = DCMPLX(DBLE(A(II,II)),
     +                                         ZERO)
  540                            CONTINUE
                              END IF
                              DO 640 KK = 1, 2
                                 UPLO = UPLOS(KK)
                                 IF (UPLO.EQ.'U') THEN
                                    DO 580 J = 1, N
                                       IJK = K + 1 - J
                                       DO 560 I = MAX(1,J-K), J
                                          AB(IJK+I,J) = A(I,J)
  560                                  CONTINUE
  580                               CONTINUE
                                    IF (MATRIX.EQ.'SB') THEN
                                       MATTYP = 'SYU'
                                    ELSE IF (MATRIX.EQ.'HB') THEN
                                       MATTYP = 'HEU'
                                    END IF
                                 ELSE IF (UPLO.EQ.'L') THEN
                                    DO 620 J = 1, N
                                       IJK = 1 - J
                                       DO 600 I = J, MIN(N,J+K)
                                          AB(IJK+I,J) = A(I,J)
  600                                  CONTINUE
  620                               CONTINUE
                                    IF (MATRIX.EQ.'SB') THEN
                                       MATTYP = 'SYL'
                                    ELSE IF (MATRIX.EQ.'HB') THEN
                                       MATTYP = 'HEL'
                                    END IF
                                 END IF
                                 IF (MATRIX.EQ.'HB') THEN
                                    RNORM = F06UEF(NORM,UPLO,N,K,AB,
     +                                      LDAB,WORK)
                                 ELSE IF (MATRIX.EQ.'SB') THEN
                                    RNORM = F06UHF(NORM,UPLO,N,K,AB,
     +                                      LDAB,WORK)
                                 END IF
                                 TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,
     +                                   MATTYP,'N')
                                 IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM))
     +                               THEN
                                    IF (MATRIX.EQ.'HB') THEN
                                       WRITE (NOUT,99995) NORM,
     +                                   UPLO, N, K, LDAB, RNORM, TNORM
                                    ELSE IF (MATRIX.EQ.'SB') THEN
                                       WRITE (NOUT,99992) NORM,
     +                                   UPLO, N, K, LDAB, RNORM, TNORM
                                    END IF
                                    PASS = .FALSE.
                                    GO TO 1640
                                 END IF
  640                         CONTINUE
                           END IF
  660                   CONTINUE
                     ELSE IF (MATRIX.EQ.'TRL') THEN
                        UPLO = 'L'
                        DO 720 KK = 1, 2
                           DIAG = DIAGS(KK)
                           IF (DIAG.EQ.'U') THEN
                              DO 680 LL = 1, MIN(M,N)
                                 A(LL,LL) = CDUMMY
  680                         CONTINUE
                           END IF
                           RNORM = F06UJF(NORM,UPLO,DIAG,M,N,A,LDA,WORK)
                           IF (DIAG.EQ.'U') THEN
                              DO 700 LL = 1, MIN(M,N)
                                 A(LL,LL) = ONE
  700                         CONTINUE
                           END IF
                           TNORM = TRUNRM(M,N,MAXEL,A,LDA,NORM,MATRIX,
     +                             DIAG)
                           IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM)) THEN
                              WRITE (NOUT,99991) NORM, UPLO, DIAG,
     +                          M, N, LDA, RNORM, TNORM
                              PASS = .FALSE.
                              GO TO 1640
                           END IF
  720                   CONTINUE
                     ELSE IF (MATRIX.EQ.'TRU') THEN
                        UPLO = 'U'
                        DO 780 KK = 1, 2
                           DIAG = DIAGS(KK)
                           IF (DIAG.EQ.'U') THEN
                              DO 740 LL = 1, MIN(M,N)
                                 A(LL,LL) = CDUMMY
  740                         CONTINUE
                           END IF
                           RNORM = F06UJF(NORM,UPLO,DIAG,M,N,A,LDA,WORK)
                           IF (DIAG.EQ.'U') THEN
                              DO 760 LL = 1, MIN(M,N)
                                 A(LL,LL) = ONE
  760                         CONTINUE
                           END IF
                           TNORM = TRUNRM(M,N,MAXEL,A,LDA,NORM,MATRIX,
     +                             DIAG)
                           IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM)) THEN
                              WRITE (NOUT,99991) NORM, UPLO, DIAG,
     +                          M, N, LDA, RNORM, TNORM
                              PASS = .FALSE.
                              GO TO 1640
                           END IF
  780                   CONTINUE
                     ELSE IF (MATRIX.EQ.'TP') THEN
                        DO 1060 KK = 1, 2
                           DIAG = DIAGS(KK)
                           DO 1040 JJ = 1, 2
                              IF (NORM.NE.'M') THEN
                                 DO 820 I = 1, N
                                    DO 800 J = 1, N
                                       A(I,J) = CDUMMY
  800                               CONTINUE
  820                            CONTINUE
                              END IF
                              UPLO = UPLOS(JJ)
                              IF (UPLO.EQ.'U') THEN
                                 DO 860 I = 1, N
                                    DO 840 J = I, N
                                       A(I,J) = ACOPY(I,J)
  840                               CONTINUE
  860                            CONTINUE
                                 MATTYP = 'TRU'
                              ELSE IF (UPLO.EQ.'L') THEN
                                 DO 900 I = 1, N
                                    DO 880 J = 1, I
                                       A(I,J) = ACOPY(I,J)
  880                               CONTINUE
  900                            CONTINUE
                                 MATTYP = 'TRL'
                              END IF
                              IF (DIAG.EQ.'U') THEN
                                 DO 920 LL = 1, N
                                    A(LL,LL) = CDUMMY
  920                            CONTINUE
                              END IF
                              IF (UPLO.EQ.'U') THEN
                                 DO 960 I = 1, N
                                    DO 940 J = I, N
                                       AP(I+J*(J-1)/2) = A(I,J)
  940                               CONTINUE
  960                            CONTINUE
                              ELSE IF (UPLO.EQ.'L') THEN
                                 DO 1000 I = 1, N
                                    DO 980 J = 1, I
                                       AP(I+(2*N-J)*(J-1)/2) = A(I,J)
  980                               CONTINUE
 1000                            CONTINUE
                              END IF
                              RNORM = F06UKF(NORM,UPLO,DIAG,N,AP,WORK)
                              IF (DIAG.EQ.'U') THEN
                                 DO 1020 LL = 1, N
                                    A(LL,LL) = ONE
 1020                            CONTINUE
                              END IF
                              TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,
     +                                MATTYP,DIAG)
                              IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM))
     +                            THEN
                                 WRITE (NOUT,99990) NORM, UPLO,
     +                             DIAG, N, RNORM, TNORM
                                 PASS = .FALSE.
                                 GO TO 1640
                              END IF
 1040                      CONTINUE
 1060                   CONTINUE
                     ELSE IF (MATRIX.EQ.'TBU') THEN
                        UPLO = 'U'
                        DO 1100 I = 1, N - 1
                           DO 1080 J = (I+1), N
                              A(J,I) = CDUMMY
 1080                      CONTINUE
 1100                   CONTINUE
                        DO 1280 LL = 3, 1, -1
                           K = KS(LL)
                           IF (K.LE.N-1 .OR. N.EQ.0) THEN
                              DO 1140 I = 1, N - 1 - K
                                 DO 1120 J = I + 1 + K, N
                                    A(I,J) = CDUMMY
 1120                            CONTINUE
 1140                         CONTINUE
                              LOWIN = MAX(1,IM-K)
                              HIGHIN = MIN(N,IM+K)
                              IN = LOWIN + G05CAF(DUM)*(HIGHIN-LOWIN+1)
                              IF (IM.LE.IN) THEN
                                 A(IM,IN) = MAXEL
                              ELSE
                                 A(IN,IM) = MAXEL
                              END IF
                              DO 1260 KK = 1, 2
                                 DIAG = DIAGS(KK)
                                 IF (DIAG.EQ.'U') THEN
                                    DO 1160 IJK = 1, N
                                       CWORK(IJK) = A(IJK,IJK)
                                       A(IJK,IJK) = CDUMMY
 1160                               CONTINUE
                                 END IF
                                 DO 1200 J = 1, N
                                    IJK = K + 1 - J
                                    DO 1180 I = MAX(1,J-K), J
                                       AB(IJK+I,J) = A(I,J)
 1180                               CONTINUE
 1200                            CONTINUE
                                 RNORM = F06ULF(NORM,UPLO,DIAG,N,K,AB,
     +                                   LDAB,WORK)
                                 IF (DIAG.EQ.'U') THEN
                                    DO 1220 IJK = 1, N
                                       A(IJK,IJK) = ONE
 1220                               CONTINUE
                                 END IF
                                 TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,
     +                                   MATRIX,'Z')
                                 IF (DIAG.EQ.'U') THEN
                                    DO 1240 IJK = 1, N
                                       A(IJK,IJK) = CWORK(IJK)
 1240                               CONTINUE
                                 END IF
                                 IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM))
     +                               THEN
                                    WRITE (NOUT,99989) NORM, UPLO,
     +                                DIAG, N, K, LDA, RNORM, TNORM
                                    PASS = .FALSE.
                                    GO TO 1640
                                 END IF
 1260                         CONTINUE
                           END IF
 1280                   CONTINUE
                     ELSE IF (MATRIX.EQ.'TBL') THEN
                        UPLO = 'L'
                        DO 1320 I = 1, N - 1
                           DO 1300 J = (I+1), N
                              A(I,J) = CDUMMY
 1300                      CONTINUE
 1320                   CONTINUE
                        DO 1500 LL = 3, 1, -1
                           K = KS(LL)
                           IF (K.LE.N-1 .OR. N.EQ.0) THEN
                              DO 1360 I = 1, N - 1 - K
                                 DO 1340 J = I + 1 + K, N
                                    A(J,I) = CDUMMY
 1340                            CONTINUE
 1360                         CONTINUE
                              LOWIN = MAX(1,IM-K)
                              HIGHIN = MIN(N,IM+K)
                              IN = LOWIN + G05CAF(DUM)*(HIGHIN-LOWIN+1)
                              IF (IM.GE.IN) THEN
                                 A(IM,IN) = MAXEL
                              ELSE
                                 A(IN,IM) = MAXEL
                              END IF
                              DO 1480 KK = 1, 2
                                 DIAG = DIAGS(KK)
                                 IF (DIAG.EQ.'U') THEN
                                    DO 1380 IJK = 1, N
                                       CWORK(IJK) = A(IJK,IJK)
                                       A(IJK,IJK) = CDUMMY
 1380                               CONTINUE
                                 END IF
                                 DO 1420 J = 1, N
                                    IJK = 1 - J
                                    DO 1400 I = J, MIN(N,J+K)
                                       AB(IJK+I,J) = A(I,J)
 1400                               CONTINUE
 1420                            CONTINUE
                                 RNORM = F06ULF(NORM,UPLO,DIAG,N,K,AB,
     +                                   LDAB,WORK)
                                 IF (DIAG.EQ.'U') THEN
                                    DO 1440 IJK = 1, N
                                       A(IJK,IJK) = ONE
 1440                               CONTINUE
                                 END IF
                                 TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,
     +                                   MATRIX,'Z')
                                 IF (DIAG.EQ.'U') THEN
                                    DO 1460 IJK = 1, N
                                       A(IJK,IJK) = CWORK(IJK)
 1460                               CONTINUE
                                 END IF
                                 IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM))
     +                               THEN
                                    WRITE (NOUT,99989) NORM, UPLO,
     +                                DIAG, N, K, LDA, RNORM, TNORM
                                    PASS = .FALSE.
                                    GO TO 1640
                                 END IF
 1480                         CONTINUE
                           END IF
 1500                   CONTINUE
                     ELSE IF (MATRIX.EQ.'HS') THEN
                        DO 1540 KK = 1, N - 2
                           DO 1520 LL = KK + 2, N
                              A(LL,KK) = CDUMMY
 1520                      CONTINUE
 1540                   CONTINUE
                        RNORM = F06UMF(NORM,N,A,LDA,WORK)
                        TNORM = TRUNRM(N,N,MAXEL,A,LDA,NORM,MATRIX,'N')
                        IF (ABS(RNORM-TNORM).GT.ABS(TOL*TNORM)) THEN
                           WRITE (NOUT,99988) NORM, N, LDA, RNORM,
     +                       TNORM
                           PASS = .FALSE.
                           GO TO 1640
                        END IF
                     END IF
                  END IF
 1560          CONTINUE
 1580       CONTINUE
 1600    CONTINUE
 1620 CONTINUE
 1640 IF (PASS) THEN
         WRITE (NOUT,99987)
      ELSE
         WRITE (NOUT,99986)
      END IF
      STOP
*
99999 FORMAT (' The call',/' CALL F06UAF(''',A,''',',I3,',',I3,',A,',I3,
     +       ',WORK)',/' returned norm(A) as ',1P,D13.5,' instead of ',
     +       D13.5,' .')
99998 FORMAT (' The call',/' CALL F06UBF(''',A,''',',I3,',',I3,',',I3,
     +       ',A,',I3,',WORK)',/' returned norm(A) as ',1P,D13.5,' ins',
     +       'tead of ',D13.5,' .')
99997 FORMAT (' The call',/' CALL F06UCF(''',A,''',''',A,''',',I3,',A,',
     +       I3,',WORK)',/' returned norm(A) as ',1P,D13.5,' instead o',
     +       'f ',D13.5,' .')
99996 FORMAT (' The call',/' CALL F06UDF(''',A,''',''',A,''',',I3,',AP',
     +       ',WORK)',/' returned norm(A) as ',1P,D13.5,' instead of ',
     +       D13.5,' .')
99995 FORMAT (' The call',/' CALL F06UEF(''',A,''',''',A,''',',I3,',',
     +       I3,',A,',I3,',WORK)',/' returned norm(A) as ',1P,D13.5,
     +       ' instead of ',D13.5,' .')
99994 FORMAT (' The call',/' CALL F06UFF(''',A,''',''',A,''',',I3,',A,',
     +       I3,',WORK)',/' returned norm(A) as ',1P,D13.5,' instead o',
     +       'f ',D13.5,' .')
99993 FORMAT (' The call',/' CALL F06UGF(''',A,''',''',A,''',',I3,',AP',
     +       ',WORK)',/' returned norm(A) as ',1P,D13.5,' instead of ',
     +       D13.5,' .')
99992 FORMAT (' The call',/' CALL F06UHF(''',A,''',''',A,''',',I3,',',
     +       I3,',A,',I3,',WORK)',/' returned norm(A) as ',1P,D13.5,
     +       ' instead of ',D13.5,' .')
99991 FORMAT (' The call',/' CALL F06UJF(''',A,''',''',A,''',''',A,
     +       ''',',I3,',',I3,',A,',I3,',WORK)',/' returned norm(A) as ',
     +       1P,D13.5,' instead of ',D13.5,' .')
99990 FORMAT (' The call',/' CALL F06UKF(''',A,''',''',A,''',''',A,
     +       ''',',I3,',AP,WORK)',/' returned norm(A) as ',1P,D13.5,
     +       ' instead of ',D13.5,' .')
99989 FORMAT (' The call',/' CALL F06ULF(''',A,''',''',A,''',''',A,
     +       ''',',I3,',',I3,',A,',I3,',WORK)',/' returned norm(A) as ',
     +       1P,D13.5,' instead of ',D13.5,' .')
99988 FORMAT (' The call',/' CALL F06UMF(''',A,''',',I3,',A,',I3,',WOR',
     +       'K)',/' returned norm(A) as ',1P,D13.5,' instead of ',
     +       D13.5,' .')
99987 FORMAT (' F06UAF Example Program ends OK')
99986 FORMAT (' F06UAF Example Program ends with ERRORS')
      END
      DOUBLE PRECISION FUNCTION TRUNRM(M,N,MAXEL,A,LDA,NORM,MATRIX,DIAG)
*     .. Parameters ..
      DOUBLE PRECISION                 RDUMMY, ZERO, ONE
      PARAMETER                        (RDUMMY=-1.0D+10,ZERO=0.0D+0,
     +                                 ONE=1.0D+0)
*     .. Scalar Arguments ..
      COMPLEX*16                       MAXEL
      INTEGER                          LDA, M, N
      CHARACTER                        DIAG, NORM
      CHARACTER*3                      MATRIX
*     .. Array Arguments ..
      COMPLEX*16                       A(LDA,*)
*     .. Local Scalars ..
      COMPLEX*16                       CDUMMY
      DOUBLE PRECISION                 COLSUM, ROWSUM
      INTEGER                          I, J
      LOGICAL                          HERMIT, SYMMET
*     .. Intrinsic Functions ..
      INTRINSIC                        ABS, DCMPLX, MAX, MIN, SQRT
*     .. Executable Statements ..
      CDUMMY = DCMPLX(RDUMMY,RDUMMY)
      SYMMET = MATRIX .EQ. 'SYU' .OR. MATRIX .EQ. 'SYL'
      HERMIT = MATRIX .EQ. 'HEU' .OR. MATRIX .EQ. 'HEL'
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
                     IF (A(I,J).NE.CDUMMY) TRUNRM = MAX(TRUNRM,
     +                   ABS(A(I,J)))
  100             CONTINUE
  120          CONTINUE
            ELSE IF (MATRIX.EQ.'TBL') THEN
               TRUNRM = ZERO
               DO 160 I = 1, N
                  DO 140 J = 1, I
                     IF (A(I,J).NE.CDUMMY) TRUNRM = MAX(TRUNRM,
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
                  IF (A(J,I).NE.CDUMMY) COLSUM = COLSUM + ABS(A(J,I))
  180          CONTINUE
               TRUNRM = MAX(TRUNRM,COLSUM)
  200       CONTINUE
         ELSE IF (MATRIX.EQ.'TBL') THEN
            DO 240 I = 1, N
               COLSUM = ZERO
               DO 220 J = I, N
                  IF (A(J,I).NE.CDUMMY) COLSUM = COLSUM + ABS(A(J,I))
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
                  ELSE IF (MATRIX.EQ.'SYU' .OR. MATRIX.EQ.'HEU') THEN
                     IF (I.LE.J) THEN
                        IF (A(I,J).NE.CDUMMY) COLSUM = COLSUM +
     +                      ABS(A(I,J))
                     ELSE IF (I.GT.J) THEN
                        IF (A(J,I).NE.CDUMMY) COLSUM = COLSUM +
     +                      ABS(A(J,I))
                     END IF
                  ELSE IF (MATRIX.EQ.'SYL' .OR. MATRIX.EQ.'HEL') THEN
                     IF (I.GE.J) THEN
                        IF (A(I,J).NE.CDUMMY) COLSUM = COLSUM +
     +                      ABS(A(I,J))
                     ELSE IF (I.LT.J) THEN
                        IF (A(J,I).NE.CDUMMY) COLSUM = COLSUM +
     +                      ABS(A(J,I))
                     END IF
                  ELSE IF (MATRIX.EQ.'HS' .OR. MATRIX.EQ.'GB') THEN
                     IF (A(I,J).NE.CDUMMY) COLSUM = COLSUM + ABS(A(I,J))
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
                  IF (A(I,J).NE.CDUMMY) ROWSUM = ROWSUM + ABS(A(I,J))
  300          CONTINUE
               TRUNRM = MAX(TRUNRM,ROWSUM)
  320       CONTINUE
         ELSE IF (MATRIX.EQ.'TBL') THEN
            DO 360 I = 1, N
               ROWSUM = ZERO
               DO 340 J = 1, I
                  IF (A(I,J).NE.CDUMMY) ROWSUM = ROWSUM + ABS(A(I,J))
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
                  ELSE IF (MATRIX.EQ.'SYU' .OR. MATRIX.EQ.'HEU') THEN
                     IF (I.LE.J) THEN
                        IF (A(I,J).NE.CDUMMY) ROWSUM = ROWSUM +
     +                      ABS(A(I,J))
                     ELSE IF (I.GT.J) THEN
                        IF (A(J,I).NE.CDUMMY) ROWSUM = ROWSUM +
     +                      ABS(A(J,I))
                     END IF
                  ELSE IF (MATRIX.EQ.'SYL' .OR. MATRIX.EQ.'HEL') THEN
                     IF (I.GE.J) THEN
                        IF (A(I,J).NE.CDUMMY) ROWSUM = ROWSUM +
     +                      ABS(A(I,J))
                     ELSE IF (I.LT.J) THEN
                        IF (A(J,I).NE.CDUMMY) ROWSUM = ROWSUM +
     +                      ABS(A(J,I))
                     END IF
                  ELSE IF (MATRIX.EQ.'HS' .OR. MATRIX.EQ.'GB') THEN
                     IF (A(I,J).NE.CDUMMY) ROWSUM = ROWSUM + ABS(A(I,J))
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
                  IF (A(I,J).NE.CDUMMY) TRUNRM = TRUNRM + ABS(A(I,J))**2
  420          CONTINUE
  440       CONTINUE
            TRUNRM = SQRT(TRUNRM)
         ELSE IF (MATRIX.EQ.'TBL') THEN
            DO 480 I = 1, N
               DO 460 J = 1, I
                  IF (A(I,J).NE.CDUMMY) TRUNRM = TRUNRM + ABS(A(I,J))**2
  460          CONTINUE
  480       CONTINUE
            TRUNRM = SQRT(TRUNRM)
         ELSE IF ( .NOT. SYMMET .AND. .NOT. HERMIT) THEN
            DO 520 I = 1, M
               DO 500 J = 1, N
                  IF (A(I,J).NE.CDUMMY) TRUNRM = TRUNRM + ABS(A(I,J))**2
  500          CONTINUE
  520       CONTINUE
            TRUNRM = SQRT(TRUNRM)
         ELSE IF (MATRIX.EQ.'SYU' .OR. MATRIX.EQ.'HEU') THEN
            DO 560 I = 1, M
               DO 540 J = I, M
                  IF (I.NE.J .AND. A(I,J).NE.CDUMMY) TRUNRM = TRUNRM +
     +                2*ABS(A(I,J))**2
  540          CONTINUE
               TRUNRM = TRUNRM + ABS(A(I,I))**2
  560       CONTINUE
            TRUNRM = SQRT(TRUNRM)
         ELSE IF (MATRIX.EQ.'SYL' .OR. MATRIX.EQ.'HEL') THEN
            DO 600 I = 1, M
               DO 580 J = 1, I
                  IF (I.NE.J .AND. A(I,J).NE.CDUMMY) TRUNRM = TRUNRM +
     +                2*ABS(A(I,J))**2
  580          CONTINUE
               TRUNRM = TRUNRM + ABS(A(I,I))**2
  600       CONTINUE
            TRUNRM = SQRT(TRUNRM)
         END IF
      END IF
      RETURN
      END
