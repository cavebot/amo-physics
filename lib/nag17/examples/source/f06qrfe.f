*     F06QRF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      DOUBLE PRECISION RROGUE, ZERO
      PARAMETER        (RROGUE=-1.0D+10,ZERO=0.0D+0)
      INTEGER          LDA, NMAX
      PARAMETER        (LDA=17,NMAX=5)
*     .. Local Scalars ..
      DOUBLE PRECISION DIF, DUM, TOL
      INTEGER          I, J, K1, K2, KK1, KK2, N, NN, ONCER, SID
      LOGICAL          PASS
      CHARACTER*1      SIDE
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,LDA), C(LDA), S(LDA), SAVA(LDA,LDA),
     +                 SAVS(LDA)
      INTEGER          ENS(5)
*     .. External Functions ..
      DOUBLE PRECISION G05CAF, X02AJF
      EXTERNAL         G05CAF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         F06QRF, F06QVF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, MAX, MIN
*     .. Data statements ..
      DATA             ENS/0, 1, 2, 8, 17/
*     .. Executable Statements ..
      WRITE (NOUT,99999)
      TOL = X02AJF()*100
      PASS = .TRUE.
      DO 400 SID = 1, 2
         IF (SID.EQ.1) THEN
            SIDE = 'L'
         ELSE
            SIDE = 'R'
         END IF
         DO 380 NN = 1, NMAX
            N = ENS(NN)
            DO 360 KK1 = 1, NMAX
               K1 = ENS(KK1)
               DO 340 KK2 = 1, NMAX
                  K2 = ENS(KK2)
                  DO 320 ONCER = 0, 1
                     DO 60 I = 1, N
                        A(I,I) = G05CAF(DUM)
                        SAVA(I,I) = A(I,I)
                        IF (I.GE.K1 .AND. I.LT.K2) THEN
                           IF (ONCER.EQ.1) THEN
                              S(I) = ZERO
                           ELSE
                              S(I) = G05CAF(DUM)
                           END IF
                           SAVS(I) = S(I)
                        ELSE
                           S(I) = RROGUE
                        END IF
                        DO 20 J = 1, I - 1
                           A(I,J) = RROGUE
                           SAVA(I,J) = RROGUE
   20                   CONTINUE
                        DO 40 J = I + 1, N
                           A(I,J) = G05CAF(DUM)
                           SAVA(I,J) = A(I,J)
   40                   CONTINUE
   60                CONTINUE
                     IF (N.GT.0) S(N) = RROGUE
                     CALL F06QRF(SIDE,N,K1,K2,C,S,A,LDA)
*                    Check whether A has been changed below diagonal by
*                    F06QRF.
                     DO 100 I = 2, N
                        DO 80 J = 1, I - 1
                           IF (A(I,J).NE.RROGUE) THEN
                              WRITE (NOUT,99998) I, J
                              WRITE (NOUT,99993) 'QRF', SIDE, N, K1, K2,
     +                          LDA
                              WRITE (NOUT,99996)
                              PASS = .FALSE.
                              GO TO 420
                           END IF
   80                   CONTINUE
  100                CONTINUE
*                    Check whether 'unreferenced' parts of S have
*                    changed.
                     DO 120 I = 1, MIN(K1-1,N)
                        IF (S(I).NE.RROGUE) THEN
                           WRITE (NOUT,99997) I
                           WRITE (NOUT,99993) 'QRF', SIDE, N, K1, K2,
     +                       LDA
                           WRITE (NOUT,99996)
                           PASS = .FALSE.
                           GO TO 420
                        END IF
  120                CONTINUE
                     DO 140 I = MAX(K2,1), N
                        IF (S(I).NE.RROGUE) THEN
                           WRITE (NOUT,99997) I
                           WRITE (NOUT,99993) 'QRF', SIDE, N, K1, K2,
     +                       LDA
                           WRITE (NOUT,99996)
                           PASS = .FALSE.
                           GO TO 420
                        END IF
  140                CONTINUE
                     IF (MIN(N,K1).GE.1 .AND. K2.GT.K1 .AND. K2.LE.N)
     +                   THEN
*                       Take the transpose of P.
                        DO 160 I = K1, K2 - 1
                           S(I) = -S(I)
  160                   CONTINUE
                     END IF
                     CALL F06QVF(SIDE,N,K1,K2,C,S,A,LDA)
*                    Check whether A has been changed below diagonal by
*                    F06QVF.
                     DO 200 I = 2, N
                        DO 180 J = 1, I - 1
                           IF (A(I,J).NE.RROGUE) THEN
                              WRITE (NOUT,99998) I, J
                              WRITE (NOUT,99993) 'QVF', SIDE, N, K1, K2,
     +                          LDA
                              WRITE (NOUT,99996)
                              PASS = .FALSE.
                              GO TO 420
                           END IF
  180                   CONTINUE
  200                CONTINUE
                     DO 220 I = 1, MIN(K1-1,N)
                        IF (S(I).NE.RROGUE) THEN
                           WRITE (NOUT,99997) I
                           WRITE (NOUT,99993) 'QVF', SIDE, N, K1, K2,
     +                       LDA
                           WRITE (NOUT,99996)
                           PASS = .FALSE.
                           GO TO 420
                        END IF
  220                CONTINUE
                     DO 240 I = MAX(K2,1), N
                        IF (S(I).NE.RROGUE) THEN
                           WRITE (NOUT,99997) I
                           WRITE (NOUT,99993) 'QVF', SIDE, N, K1, K2,
     +                       LDA
                           WRITE (NOUT,99996)
                           PASS = .FALSE.
                           GO TO 420
                        END IF
  240                CONTINUE
*                    The two transformations should cancel each other,
*                    i.e. A and S should end up as they were originally.
*                    Compare with the saved arrays.
                     DO 280 I = 1, N
                        DO 260 J = I, N
                           DIF = ABS(A(I,J)-SAVA(I,J))
                           IF (DIF.GT.TOL) THEN
                              WRITE (NOUT,99995) I, J
                              WRITE (NOUT,99993) 'QRF', SIDE, N, K1, K2,
     +                          LDA
                              WRITE (NOUT,99993) 'QVF', SIDE, N, K1, K2,
     +                          LDA
                              WRITE (NOUT,99992) A(I,J), SAVA(I,J)
                              PASS = .FALSE.
                              GO TO 420
                           END IF
  260                   CONTINUE
  280                CONTINUE
                     DO 300 I = MAX(K1,1), MIN(K2,N) - 1
                        DIF = ABS(S(I)-SAVS(I))
                        IF (DIF.GT.TOL) THEN
                           WRITE (NOUT,99994) I
                           WRITE (NOUT,99993) 'QRF', SIDE, N, K1, K2,
     +                       LDA
                           WRITE (NOUT,99993) 'QVF', SIDE, N, K1, K2,
     +                       LDA
                           WRITE (NOUT,99992) S(I), SAVS(I)
                           PASS = .FALSE.
                           GO TO 420
                        END IF
  300                CONTINUE
  320             CONTINUE
  340          CONTINUE
  360       CONTINUE
  380    CONTINUE
  400 CONTINUE
  420 IF (PASS) THEN
         WRITE (NOUT,99991)
      ELSE
         WRITE (NOUT,99990)
      END IF
      STOP
*
99999 FORMAT (' F06QRF Example Program Results',/1X)
99998 FORMAT (' Element A(',I3,',',I3,') was altered by the call:')
99997 FORMAT (' Element S(',I3,') was altered by the call:')
99996 FORMAT (' although it should not have been referenced.')
99995 FORMAT (' Element A(',I3,',',I3,') was incorrectly computed by t',
     +       'he calls:')
99994 FORMAT (' Element S(',I3,') was incorrectly computed by the call',
     +       's:')
99993 FORMAT (' CALL F06',A,'(''',A,''',',I3,',',I3,',',I3,',C,S,A,',I3,
     +       ')')
99992 FORMAT ('  as',1P,D13.5,' instead of ',D13.5,'.')
99991 FORMAT (' F06QRF Example Program ends OK')
99990 FORMAT (' F06QRF Example Program ends with ERRORS')
      END
