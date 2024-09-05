*     F06TRF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      DOUBLE PRECISION RROGUE, ZERO
      PARAMETER        (RROGUE=-1.0D+10,ZERO=0.0D+0)
      INTEGER          LDA, NMAX
      PARAMETER        (LDA=17,NMAX=5)
*     .. Local Scalars ..
      COMPLEX*16       CROGUE, DIAG
      DOUBLE PRECISION DIF, DUM, TOL
      INTEGER          I, J, K1, K2, KK1, KK2, N, NN, ONCER, SID
      LOGICAL          PASS
      CHARACTER*1      SIDE
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,LDA), C(LDA), SAVA(LDA,LDA)
      DOUBLE PRECISION S(LDA), SAVS(LDA)
      INTEGER          ENS(5)
*     .. External Functions ..
      DOUBLE PRECISION G05CAF, X02AJF
      EXTERNAL         G05CAF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         F06TRF, F06TVF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, DCMPLX, DCONJG, MAX, MIN
*     .. Data statements ..
      DATA             ENS/0, 1, 2, 8, 17/
*     .. Executable Statements ..
      WRITE (NOUT,99999)
      CROGUE = DCMPLX(RROGUE,RROGUE)
      TOL = X02AJF()*100
      PASS = .TRUE.
      DO 440 SID = 1, 2
         IF (SID.EQ.1) THEN
            SIDE = 'L'
         ELSE
            SIDE = 'R'
         END IF
         DO 420 NN = 1, NMAX
            N = ENS(NN)
            DO 400 KK1 = 1, NMAX
               K1 = ENS(KK1)
               DO 380 KK2 = 1, NMAX
                  K2 = ENS(KK2)
                  DO 360 ONCER = 0, 1
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
                           A(I,J) = CROGUE
                           SAVA(I,J) = CROGUE
   20                   CONTINUE
                        DO 40 J = I + 1, N
                           A(I,J) = DCMPLX(G05CAF(DUM),G05CAF(DUM))
                           SAVA(I,J) = A(I,J)
   40                   CONTINUE
   60                CONTINUE
                     IF (N.GT.0) S(N) = RROGUE
                     CALL F06TRF(SIDE,N,K1,K2,C,S,A,LDA)
*                    Check whether A has been changed below diagonal by
*                    F06TRF.
                     DO 100 I = 2, N
                        DO 80 J = 1, I - 1
                           IF (A(I,J).NE.CROGUE) THEN
                              WRITE (NOUT,99998) I, J
                              WRITE (NOUT,99993) 'TRF', SIDE, N, K1, K2,
     +                          LDA
                              WRITE (NOUT,99996)
                              PASS = .FALSE.
                              GO TO 460
                           END IF
   80                   CONTINUE
  100                CONTINUE
*                    Check whether 'unreferenced' parts of S have
*                    changed.
                     DO 120 I = 1, MIN(K1-1,N)
                        IF (S(I).NE.RROGUE) THEN
                           WRITE (NOUT,99997) I
                           WRITE (NOUT,99993) 'TRF', SIDE, N, K1, K2,
     +                       LDA
                           WRITE (NOUT,99996)
                           PASS = .FALSE.
                           GO TO 460
                        END IF
  120                CONTINUE
                     DO 140 I = MAX(K2,1), N
                        IF (S(I).NE.RROGUE) THEN
                           WRITE (NOUT,99997) I
                           WRITE (NOUT,99993) 'TRF', SIDE, N, K1, K2,
     +                       LDA
                           WRITE (NOUT,99996)
                           PASS = .FALSE.
                           GO TO 460
                        END IF
  140                CONTINUE
                     IF (MIN(N,K1).GE.1 .AND. K2.GT.K1 .AND. K2.LE.N)
     +                   THEN
*                       Take the Hermitian transpose of P.
                        DO 160 I = K1, K2 - 1
                           C(I) = DCONJG(C(I))
                           S(I) = -S(I)
  160                   CONTINUE
                        DIAG = C(K2)
                        IF (SIDE.EQ.'L') THEN
*                          Premultiply A by the Hermitian transpose
*                          of D.
                           DO 180 I = K2, N
                              A(K2,I) = DCONJG(DIAG)*A(K2,I)
  180                      CONTINUE
                        ELSE
*                          Postmultiply A by D.
                           DO 200 I = 1, K1
                              A(I,K1) = A(I,K1)*DIAG
  200                      CONTINUE
                        END IF
                     END IF
                     CALL F06TVF(SIDE,N,K1,K2,C,S,A,LDA)
*                    Check whether A has been changed below diagonal by
*                    F06TVF.
                     DO 240 I = 2, N
                        DO 220 J = 1, I - 1
                           IF (A(I,J).NE.CROGUE) THEN
                              WRITE (NOUT,99998) I, J
                              WRITE (NOUT,99993) 'TVF', SIDE, N, K1, K2,
     +                          LDA
                              WRITE (NOUT,99996)
                              PASS = .FALSE.
                              GO TO 460
                           END IF
  220                   CONTINUE
  240                CONTINUE
                     DO 260 I = 1, MIN(K1-1,N)
                        IF (S(I).NE.RROGUE) THEN
                           WRITE (NOUT,99997) I
                           WRITE (NOUT,99993) 'TVF', SIDE, N, K1, K2,
     +                       LDA
                           WRITE (NOUT,99996)
                           PASS = .FALSE.
                           GO TO 460
                        END IF
  260                CONTINUE
                     DO 280 I = MAX(K2,1), N
                        IF (S(I).NE.RROGUE) THEN
                           WRITE (NOUT,99997) I
                           WRITE (NOUT,99993) 'TVF', SIDE, N, K1, K2,
     +                       LDA
                           WRITE (NOUT,99996)
                           PASS = .FALSE.
                           GO TO 460
                        END IF
  280                CONTINUE
*                    The two transformations should cancel each other,
*                    i.e. A and S should end up as they were originally.
*                    Compare with the saved arrays.
                     DO 320 I = 1, N
                        DO 300 J = I, N
                           DIF = ABS(A(I,J)-SAVA(I,J))
                           IF (DIF.GT.TOL) THEN
                              WRITE (NOUT,99995) I, J
                              WRITE (NOUT,99993) 'TRF', SIDE, N, K1, K2,
     +                          LDA
                              WRITE (NOUT,99993) 'TVF', SIDE, N, K1, K2,
     +                          LDA
                              WRITE (NOUT,99992) A(I,J), SAVA(I,J)
                              PASS = .FALSE.
                              GO TO 460
                           END IF
  300                   CONTINUE
  320                CONTINUE
                     DO 340 I = MAX(K1,1), MIN(K2,N) - 1
                        DIF = ABS(S(I)-SAVS(I))
                        IF (DIF.GT.TOL) THEN
                           WRITE (NOUT,99994) I
                           WRITE (NOUT,99993) 'TRF', SIDE, N, K1, K2,
     +                       LDA
                           WRITE (NOUT,99993) 'TVF', SIDE, N, K1, K2,
     +                       LDA
                           WRITE (NOUT,99991) S(I), SAVS(I)
                           PASS = .FALSE.
                           GO TO 460
                        END IF
  340                CONTINUE
  360             CONTINUE
  380          CONTINUE
  400       CONTINUE
  420    CONTINUE
  440 CONTINUE
  460 IF (PASS) THEN
         WRITE (NOUT,99990)
      ELSE
         WRITE (NOUT,99989)
      END IF
      STOP
*
99999 FORMAT (' F06TRF Example Program Results',/1X)
99998 FORMAT (' Element A(',I3,',',I3,') was altered by the call:')
99997 FORMAT (' Element S(',I3,') was altered by the call:')
99996 FORMAT (' although it should not have been referenced.')
99995 FORMAT (' Element A(',I3,',',I3,') was incorrectly computed by t',
     +       'he calls:')
99994 FORMAT (' Element S(',I3,') was incorrectly computed by the call',
     +       's:')
99993 FORMAT (' CALL F06',A,'(''',A,''',',I3,',',I3,',',I3,',C,S,A,',I3,
     +       ')')
99992 FORMAT ('  as (',1P,D13.5,',',D13.5,') instead of (',D13.5,',',
     +       D13.5,').')
99991 FORMAT ('  as',1P,D13.5,' instead of',D13.5,' .')
99990 FORMAT (' F06TRF Example Program ends OK')
99989 FORMAT (' F06TRF Example Program ends with ERRORS')
      END
