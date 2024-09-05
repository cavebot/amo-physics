*     F06QTF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      DOUBLE PRECISION ONE, RROGUE, ZERO
      PARAMETER        (ONE=1.0D+0,RROGUE=-1.0D+10,ZERO=0.0D+0)
      INTEGER          LDA, NMAX
      PARAMETER        (LDA=17,NMAX=5)
*     .. Local Scalars ..
      DOUBLE PRECISION DUM, TOL
      INTEGER          I, J, K1, K2, KK1, KK2, N, NN, SID
      LOGICAL          PASS
      CHARACTER*1      SIDE
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,LDA), CP(LDA), CQ(LDA), SAVA(LDA,LDA),
     +                 SP(LDA), SQ(LDA)
      INTEGER          ENS(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION G05CAF, X02AJF
      EXTERNAL         G05CAF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         F06QTF, F06QXF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, SQRT
*     .. Data statements ..
      DATA             ENS/0, 1, 2, 8, 17/
*     .. Executable Statements ..
      WRITE (NOUT,99999)
      TOL = X02AJF()*100
      PASS = .TRUE.
      DO 240 SID = 1, 2
         IF (SID.EQ.1) THEN
            SIDE = 'L'
         ELSE
            SIDE = 'R'
         END IF
         DO 220 NN = 1, NMAX
            N = ENS(NN)
            DO 200 KK1 = 1, NN
               K1 = ENS(KK1)
               DO 180 KK2 = 1, NN
                  K2 = ENS(KK2)
*                 Create a unitary matrix P.
                  DO 20 I = 1, N - 1
                     CP(I) = G05CAF(DUM)
                     SP(I) = SQRT(ONE-CP(I)*CP(I))
                     CQ(I) = CP(I)
                     SQ(I) = SP(I)
   20             CONTINUE
*                 Generate a random triangular matrix, A, and copy it.
                  DO 60 I = 1, N
                     DO 40 J = I, N
                        A(I,J) = G05CAF(DUM)
                        SAVA(I,J) = A(I,J)
                        IF (I.NE.J) A(J,I) = RROGUE
   40                CONTINUE
   60             CONTINUE
*
                  CALL F06QTF(SIDE,N,K1,K2,CQ,SQ,A,LDA)
*
*                 Check that 'unreferenced' elements below diagonal
*                 are unchanged, and replace them by zeros for the
*                 calls to F06QXF below.
                  DO 100 I = 1, N
                     DO 80 J = 1, I - 1
                        IF (A(I,J).NE.RROGUE) THEN
                           WRITE (NOUT,99997) I, J
                           WRITE (NOUT,99994) SIDE, N, K1, K2, LDA
                           WRITE (NOUT,99998)
                           PASS = .FALSE.
                           GO TO 260
                        ELSE
                           A(I,J) = ZERO
                        END IF
   80                CONTINUE
  100             CONTINUE
*                 Take the Hermitian transposes of P and Q.
                  DO 120 I = 1, N - 1
                     SP(I) = -SP(I)
                     SQ(I) = -SQ(I)
  120             CONTINUE
                  IF (SIDE.EQ.'L') THEN
*                    Hit A from the left with P'.
                     CALL F06QXF('L','V','B',N,N,K1,K2,CP,SP,A,LDA)
*                    Hit A from the right with Q.
                     CALL F06QXF('R','V','B',N,N,K1,K2,CQ,SQ,A,LDA)
                  ELSE
*                    Hit A from the left with Q'.
                     CALL F06QXF('L','V','F',N,N,K1,K2,CQ,SQ,A,LDA)
*                    Hit A from the right with P.
                     CALL F06QXF('R','V','F',N,N,K1,K2,CP,SP,A,LDA)
                  END IF
*                 Now A should be as it was originally.
*                 Check that all elements were computed correctly.
                  DO 160 I = 1, N
                     DO 140 J = I, N
                        IF (ABS(A(I,J)-SAVA(I,J)).GT.TOL) THEN
                           WRITE (NOUT,99995) I, J
                           WRITE (NOUT,99994) SIDE, N, K1, K2, LDA
                           WRITE (NOUT,99996)
                           IF (SIDE.EQ.'L') THEN
                              WRITE (NOUT,99993) 'L', 'V', 'B', N, N,
     +                          K1, K2, 'CP', 'SP', LDA
                              WRITE (NOUT,99993) 'R', 'V', 'F', N, N,
     +                          K1, K2, 'CQ', 'SQ', LDA
                           ELSE
                              WRITE (NOUT,99993) 'L', 'V', 'B', N, N,
     +                          K1, K2, 'CQ', 'SQ', LDA
                              WRITE (NOUT,99993) 'R', 'V', 'F', N, N,
     +                          K1, K2, 'CP', 'SP', LDA
                           END IF
                           WRITE (NOUT,99992) A(I,J), SAVA(I,J)
                           PASS = .FALSE.
                           GO TO 260
                        END IF
  140                CONTINUE
  160             CONTINUE
  180          CONTINUE
  200       CONTINUE
  220    CONTINUE
  240 CONTINUE
  260 IF (PASS) THEN
         WRITE (NOUT,99991)
      ELSE
         WRITE (NOUT,99990)
      END IF
      STOP
*
99999 FORMAT (' F06QTF Example Program Results',/1X)
99998 FORMAT (' although it should not have been referenced.')
99997 FORMAT (' Element A(',I3,',',I3,') was altered by the call:')
99996 FORMAT (' P = P''',/' Q = Q''')
99995 FORMAT (' Element A(',I3,',',I3,') was incorrectly computed by t',
     +       'he sequence:')
99994 FORMAT (' CALL F06QTF(''',A,''',',I3,',',I3,',',I3,',CQ,SQ,A,',I3,
     +       ')')
99993 FORMAT (' CALL F06QXF(''',A,''',''',A,''',''',A,''',',I3,',',I3,
     +       ',',I3,',',I3,',',A,',',A,',A,',I3,')')
99992 FORMAT ('  as ',1P,D13.5,' instead of ',D13.5,' .')
99991 FORMAT (' F06QTF Example Program ends OK')
99990 FORMAT (' F06QTF Example Program ends with ERRORS')
      END
