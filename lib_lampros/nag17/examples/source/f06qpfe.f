*     F06QPF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          LDA, INCMAX, NINCS, NMAX
      PARAMETER        (LDA=17,INCMAX=5,NINCS=3,NMAX=5)
      DOUBLE PRECISION RROGUE, ZERO
      PARAMETER        (RROGUE=-1.0D+10,ZERO=0.0D+0)
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, DIF, DUM, T, TOL
      INTEGER          I, INC1, INC2, INCX, INCY, J, N, NN
      LOGICAL          PASS
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,LDA), C1(LDA), C2(LDA), S1(LDA), S2(LDA),
     +                 SAVA(LDA,LDA), SAVX(LDA*INCMAX), X(LDA*INCMAX),
     +                 Y(LDA*INCMAX)
      INTEGER          ENS(NMAX), INCS(NINCS)
*     .. External Functions ..
      DOUBLE PRECISION G05CAF, X02AJF
      EXTERNAL         G05CAF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         F06BCF, F06QPF, F06QXF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS
*     .. Data statements ..
      DATA             ENS/0, 1, 2, 8, 17/
      DATA             INCS/1, 3, 5/
*     .. Executable Statements ..
      WRITE (NOUT,99999)
      TOL = X02AJF()*100
      PASS = .TRUE.
      DO 480 NN = 1, NMAX
         N = ENS(NN)
         DO 40 I = 1, N
            DO 20 J = I, N
               SAVA(I,J) = G05CAF(DUM)
               IF (I.NE.J) THEN
                  SAVA(J,I) = RROGUE
               END IF
   20       CONTINUE
   40    CONTINUE
         DO 460 INC1 = 1, NINCS
            INCX = INCS(INC1)
            DO 80 I = 1, (N-1)*INCX + 1, INCX
               SAVX(I) = G05CAF(DUM)
               DO 60 J = I + 1, I + INCX - 1
                  SAVX(J) = RROGUE
   60          CONTINUE
   80       CONTINUE
            DO 440 INC2 = 1, NINCS
               INCY = INCS(INC2)
               DO 120 I = 1, (N-1)*INCY + 1, INCY
                  Y(I) = G05CAF(DUM)
                  DO 100 J = I + 1, I + INCY - 1
                     Y(J) = RROGUE
  100             CONTINUE
  120          CONTINUE
               ALPHA = G05CAF(DUM)
*              Copy A from SAVA.
               DO 160 I = 1, N
                  DO 140 J = 1, N
                     A(I,J) = SAVA(I,J)
  140             CONTINUE
  160          CONTINUE
*              Copy X from SAVX
               DO 180 I = 1, N*INCX
                  X(I) = SAVX(I)
  180          CONTINUE
*              Factorise (ALPHA*X*Y' + A) into QR.
               CALL F06QPF(N,ALPHA,X,INCX,Y,INCY,A,LDA,C2,S2)
*              Regenerate C1 and S1 from X.
               DO 200 I = 1, N - 1
                  CALL F06BCF(X((I-1)*INCX+1),C1(I),S1(I))
  200          CONTINUE
*              Check that below diagonal has not been altered.
               DO 240 I = 1, N
                  DO 220 J = 1, I - 1
                     IF (A(I,J).NE.RROGUE) THEN
                        WRITE (NOUT,99998) I, J
                        WRITE (NOUT,99997) N, ALPHA, INCX, INCY, LDA
                        WRITE (NOUT,99996)
                        PASS = .FALSE.
                        GO TO 500
                     END IF
  220             CONTINUE
  240          CONTINUE
*              Check that 'unreferenced' elements of X have not been
*              altered.
               DO 280 I = 1, (N-1)*INCX + 1, INCX
                  DO 260 J = I + 1, I + INCX - 1
                     IF (X(J).NE.RROGUE) THEN
                        WRITE (NOUT,99995) 'X', J
                        WRITE (NOUT,99997) N, ALPHA, INCX, INCY, LDA
                        WRITE (NOUT,99996)
                        PASS = .FALSE.
                        GO TO 500
                     END IF
  260             CONTINUE
  280          CONTINUE
*              Check that 'unreferenced' elements of Y have not been
*              altered.
               DO 320 I = 1, (N-1)*INCY + 1, INCY
                  DO 300 J = I + 1, I + INCY - 1
                     IF (Y(J).NE.RROGUE) THEN
                        WRITE (NOUT,99995) 'Y', J
                        WRITE (NOUT,99997) N, ALPHA, INCY, INCY, LDA
                        WRITE (NOUT,99996)
                        PASS = .FALSE.
                        GO TO 500
                     END IF
  300             CONTINUE
  320          CONTINUE
*              Set Q = Q'.
               DO 340 I = 1, N - 1
                  S1(I) = -S1(I)
                  S2(I) = -S2(I)
  340          CONTINUE
*              Insert zeros below diagonal of A for call to F06TXF.
               DO 380 I = 1, N
                  DO 360 J = 1, I - 1
                     A(I,J) = ZERO
  360             CONTINUE
  380          CONTINUE
*              Premultiply R by Q.
               CALL F06QXF('L','B','B',N,N,1,N,C2,S2,A,LDA)
               CALL F06QXF('L','B','F',N,N,1,N,C1,S1,A,LDA)
*              Now (A) should be the same as SAVA + ALPHA*SAVX*Y'.
               DO 420 I = 1, N
                  DO 400 J = 1, N
                     T = ALPHA*SAVX((I-1)*INCX+1)*Y((J-1)*INCY+1)
                     IF (J.GE.I) T = T + SAVA(I,J)
                     DIF = ABS(A(I,J)-T)
                     IF (DIF.GT.TOL) THEN
                        WRITE (NOUT,99994) I, J
                        WRITE (NOUT,99997) N, ALPHA, INCX, INCY, LDA
                        WRITE (NOUT,99993) N, N, 1, N, LDA, N, N, 1, N,
     +                    LDA
                        WRITE (NOUT,99992) A(I,J), T
                        PASS = .FALSE.
                        GO TO 500
                     END IF
  400             CONTINUE
  420          CONTINUE
  440       CONTINUE
  460    CONTINUE
  480 CONTINUE
  500 IF (PASS) THEN
         WRITE (NOUT,99991)
      ELSE
         WRITE (NOUT,99990)
      END IF
      STOP
*
99999 FORMAT (' F06QPF Example Program Results',/1X)
99998 FORMAT (' Element A(',I3,',',I3,') was altered by the call:')
99997 FORMAT (' CALL F06QPF(',I3,', ',1P,D13.5,' ,X,',I3,',Y,',I3,',A,',
     +       I3,',C,S)')
99996 FORMAT (' although it should not have been referenced.')
99995 FORMAT (' Element ',A,'(',I3,') was altered by the call:')
99994 FORMAT (' Element A(',I3,',',I3,') was incorrectly computed by t',
     +       'he calls:')
99993 FORMAT (' CALL F06QXF(''L'',''B'',''B'',',I3,',',I3,',',I3,',',I3,
     +       ',C2,S2,A,',I3,')',/' CALL F06TXF(''L'',''B'',''F'',',I3,
     +       ',',I3,',',I3,',',I3,',C1,S1,A,',I3,')')
99992 FORMAT ('  as ',1P,D13.5,' instead of ',D13.5,' .')
99991 FORMAT (' F06QPF Example Program ends OK')
99990 FORMAT (' F06QPF Example Program ends with ERRORS')
      END
