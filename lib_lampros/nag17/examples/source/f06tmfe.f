*     F06TMF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      DOUBLE PRECISION ONE, RROGUE, ZERO
      PARAMETER        (ONE=1.0D+0,RROGUE=-1.0D+10,ZERO=0.0D+0)
      INTEGER          LDA, NMAX
      PARAMETER        (LDA=17,NMAX=5)
*     .. Local Scalars ..
      COMPLEX*16       CROGUE
      DOUBLE PRECISION DUM, TOL
      INTEGER          DIR, I, J, K1, K2, KK1, KK2, N, NN, ONCER, PIV,
     +                 TRI
      LOGICAL          PASS
      CHARACTER*1      DIRECT, PIVOT, UPLO
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,LDA), B(LDA,LDA), S(LDA), SAVA(LDA,LDA)
      DOUBLE PRECISION C(LDA)
      INTEGER          ENS(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION G05CAF, X02AJF
      EXTERNAL         G05CAF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         F06TMF, F06TXF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, DCMPLX, DCONJG, MAX, DBLE
*     .. Data statements ..
      DATA             ENS/0, 1, 2, 8, 17/
*     .. Executable Statements ..
      WRITE (NOUT,99999)
      TOL = X02AJF()*100
      CROGUE = DCMPLX(RROGUE,RROGUE)
      PASS = .TRUE.
      DO 300 TRI = 1, 2
         IF (TRI.EQ.1) THEN
            UPLO = 'L'
         ELSE
            UPLO = 'U'
         END IF
         DO 280 PIV = 1, 3
            IF (PIV.EQ.1) THEN
               PIVOT = 'T'
            ELSE IF (PIV.EQ.2) THEN
               PIVOT = 'B'
            ELSE
               PIVOT = 'V'
            END IF
            DO 260 DIR = 1, 2
               IF (DIR.EQ.1) THEN
                  DIRECT = 'F'
               ELSE
                  DIRECT = 'B'
               END IF
               DO 240 NN = 1, NMAX
                  N = ENS(NN)
                  DO 220 KK1 = 1, NN
                     K1 = ENS(KK1)
                     DO 200 KK2 = 1, NN
                        K2 = ENS(KK2)
                        DO 180 ONCER = 0, 1
                           DO 40 I = 1, N
                              C(I) = G05CAF(DUM)
                              S(I) = DCMPLX(G05CAF(DUM),G05CAF(DUM))
                              IF (ONCER.EQ.1) THEN
                                 C(I) = ONE
                                 S(I) = ZERO
                              END IF
                              DO 20 J = I, N
                                 IF (UPLO.EQ.'L') THEN
                                    A(J,I) = DCMPLX(G05CAF(DUM),
     +                                       G05CAF(DUM))
                                    IF (I.EQ.J) A(J,I)
     +                                  = DCMPLX(DBLE(A(J,I)),ZERO)
                                    B(J,I) = A(J,I)
                                    IF (I.NE.J) THEN
                                       A(I,J) = CROGUE
                                       B(I,J) = DCONJG(B(J,I))
                                    END IF
                                 ELSE
                                    A(I,J) = DCMPLX(G05CAF(DUM),
     +                                       G05CAF(DUM))
                                    IF (I.EQ.J) A(I,J)
     +                                  = DCMPLX(DBLE(A(I,J)),ZERO)
                                    B(I,J) = A(I,J)
                                    IF (I.NE.J) THEN
                                       A(J,I) = CROGUE
                                       B(J,I) = DCONJG(B(I,J))
                                    END IF
                                 END IF
   20                         CONTINUE
   40                      CONTINUE
*                          Make a copy of A in SAVA.
                           DO 80 I = 1, N
                              DO 60 J = 1, N
                                 SAVA(I,J) = A(I,J)
   60                         CONTINUE
   80                      CONTINUE
*                          Do the computation.
                           CALL F06TMF(UPLO,PIVOT,DIRECT,N,K1,K2,C,S,A,
     +                                 LDA)
                           CALL F06TXF('Right',PIVOT,DIRECT,N,N,K1,K2,C,
     +                                 S,B,LDA)
                           CALL F06TXF('Left',PIVOT,DIRECT,N,N,K1,K2,C,
     +                                 S,B,LDA)
*                          Check whether any element that should not
*                          have been touched was altered.
                           DO 120 I = 1, N
                              DO 100 J = 1, N
                                 IF ((UPLO.EQ.'L' .AND. (I.LT.K1 .OR.
     +                               J.GT.K2 .OR. (I.GT.K2 .AND. J.LT.
     +                               K1) .OR. (I.GE.K1 .AND. I.LE.
     +                               K2 .AND. J.GT.I)))
     +                               .OR. (UPLO.EQ.'U' .AND. (I.GT.
     +                               K2 .OR. J.LT.K1 .OR. (I.LT.K1 .AND.
     +                               J.GT.K2) .OR. (I.GE.K1 .AND. I.LE.
     +                               K2 .AND. J.LT.I)))) THEN
                                    IF (A(I,J).NE.SAVA(I,J)) THEN
                                       WRITE (NOUT,99998) I, J
                                       WRITE (NOUT,99995) UPLO, PIVOT,
     +                                   DIRECT, N, K1, K2, LDA
                                       WRITE (NOUT,99997)
                                       PASS = .FALSE.
                                       GO TO 320
                                    END IF
                                 END IF
  100                         CONTINUE
  120                      CONTINUE
*                          Check that the required elements were
*                          computed correctly.
                           DO 160 I = MAX(K1,1), K2
                              DO 140 J = I, K2
                                 IF (UPLO.EQ.'L') THEN
                                    IF (ABS(A(J,I)-B(J,I))
     +                                  .GT.TOL .OR. ABS(DCONJG(A(J,I))
     +                                  -B(I,J)).GT.TOL) THEN
                                       WRITE (NOUT,99996) J, I, A(J,I)
                                       WRITE (NOUT,99995) UPLO, PIVOT,
     +                                   DIRECT, N, K1, K2, LDA
                                       WRITE (NOUT,99994) B(J,I)
                                       WRITE (NOUT,99993) 'R', PIVOT,
     +                                   DIRECT, N, N, K1, K2, LDA
                                       WRITE (NOUT,99993) 'L', PIVOT,
     +                                   DIRECT, N, N, K1, K2, LDA
                                       WRITE (NOUT,99992)
                                       PASS = .FALSE.
                                       GO TO 320
                                    END IF
                                 ELSE
                                    IF (ABS(A(I,J)-B(I,J))
     +                                  .GT.TOL .OR. ABS(DCONJG(A(I,J))
     +                                  -B(J,I)).GT.TOL) THEN
                                       WRITE (NOUT,99996) I, J, A(I,J)
                                       WRITE (NOUT,99995) UPLO, PIVOT,
     +                                   DIRECT, N, K1, K2, LDA
                                       WRITE (NOUT,99994) B(I,J)
                                       WRITE (NOUT,99993) 'R', PIVOT,
     +                                   DIRECT, N, N, K1, K2, LDA
                                       WRITE (NOUT,99993) 'L', PIVOT,
     +                                   DIRECT, N, N, K1, K2, LDA
                                       WRITE (NOUT,99992)
                                       PASS = .FALSE.
                                       GO TO 320
                                    END IF
                                 END IF
  140                         CONTINUE
  160                      CONTINUE
  180                   CONTINUE
  200                CONTINUE
  220             CONTINUE
  240          CONTINUE
  260       CONTINUE
  280    CONTINUE
  300 CONTINUE
  320 IF (PASS) THEN
         WRITE (NOUT,99991)
      ELSE
         WRITE (NOUT,99990)
      END IF
      STOP
*
99999 FORMAT (' F06TMF Example Program Results',/1X)
99998 FORMAT (' Element A(',I3,',',I3,') was altered by the call:')
99997 FORMAT (' although it should not have been referenced.')
99996 FORMAT (' Element A(',I3,',',I3,') was computed as (',1P,D13.5,
     +       ',',D13.5,') by the call:')
99995 FORMAT (' CALL F06TMF(''',A,''',''',A,''',''',A,''',',I3,',',I3,
     +       ',',I3,',C,S,A,',I3,')')
99994 FORMAT (' and as (',1P,D13.5,',',D13.5,') by the calls:')
99993 FORMAT (' CALL F06TXF(''',A,''',''',A,''',''',A,''',',I3,',',I3,
     +       ',',I3,',',I3,',C,S,A,',I3,')')
99992 FORMAT (' These results should be identical.')
99991 FORMAT (' F06TMF Example Program ends OK')
99990 FORMAT (' F06TMF Example Program ends with ERRORS')
      END
