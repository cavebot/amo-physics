*     F06TQF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          LDA, INCMAX, NINCS, NMAX
      PARAMETER        (LDA=18,INCMAX=5,NINCS=3,NMAX=5)
      DOUBLE PRECISION RROGUE, ZERO
      PARAMETER        (RROGUE=-1.0D+10,ZERO=0.0D+0)
*     .. Local Scalars ..
      COMPLEX*16       ALPHA, CROGUE
      DOUBLE PRECISION DIF, DUM, TOL
      INTEGER          I, INC, INCX, J, N, NN
      LOGICAL          PASS
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,LDA), S(LDA), SAVA(LDA,LDA),
     +                 SAVX(LDA*INCMAX), X(LDA*INCMAX)
      DOUBLE PRECISION C(LDA)
      INTEGER          ENS(NMAX), INCS(NINCS)
*     .. External Functions ..
      DOUBLE PRECISION G05CAF, X02AJF
      EXTERNAL         G05CAF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         F06TQF, F06TXF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, DCMPLX
*     .. Data statements ..
      DATA             ENS/0, 1, 2, 8, 17/
      DATA             INCS/1, 3, 5/
*     .. Executable Statements ..
      WRITE (NOUT,99999)
      CROGUE = DCMPLX(RROGUE,RROGUE)
      TOL = X02AJF()*100
      PASS = .TRUE.
      DO 380 NN = 1, NMAX
         N = ENS(NN)
         DO 40 I = 1, N
            DO 20 J = I, N
               SAVA(I,J) = DCMPLX(G05CAF(DUM),G05CAF(DUM))
               IF (I.NE.J) THEN
                  SAVA(J,I) = CROGUE
               END IF
   20       CONTINUE
   40    CONTINUE
         DO 60 I = 1, N + 1
            SAVA(I,N+1) = ZERO
            SAVA(N+1,I) = ZERO
   60    CONTINUE
         DO 360 INC = 1, NINCS
            INCX = INCS(INC)
            DO 100 I = 1, (N-1)*INCX + 1, INCX
               X(I) = DCMPLX(G05CAF(DUM),G05CAF(DUM))
               SAVX(I) = X(I)
               DO 80 J = I + 1, I + INCX - 1
                  X(J) = CROGUE
                  SAVX(J) = X(J)
   80          CONTINUE
  100       CONTINUE
            ALPHA = DCMPLX(G05CAF(DUM),G05CAF(DUM))
*           Copy A from SAVA.
            DO 140 I = 1, N + 1
               DO 120 J = 1, N + 1
                  A(I,J) = SAVA(I,J)
  120          CONTINUE
  140       CONTINUE
*           Factorise A into QR.
            CALL F06TQF(N,ALPHA,X,INCX,A,LDA,C,S)
*           Check that below diagonal has not been altered.
            DO 180 I = 1, N
               DO 160 J = 1, I - 1
                  IF (A(I,J).NE.CROGUE) THEN
                     WRITE (NOUT,99998) I, J
                     WRITE (NOUT,99997) N, ALPHA, INCX, LDA
                     WRITE (NOUT,99996)
                     PASS = .FALSE.
                     GO TO 400
                  END IF
  160          CONTINUE
  180       CONTINUE
*           Check that zeros in row and column N+1 are unchanged.
            DO 200 I = 1, N + 1
               IF (A(I,N+1).NE.ZERO) THEN
                  WRITE (NOUT,99998) I, N + 1
                  WRITE (NOUT,99997) N, ALPHA, INCX, LDA
                  WRITE (NOUT,99996)
                  PASS = .FALSE.
                  GO TO 400
               ELSE IF (A(N+1,I).NE.ZERO) THEN
                  WRITE (NOUT,99998) N + 1, I
                  WRITE (NOUT,99997) N, ALPHA, INCX, LDA
                  WRITE (NOUT,99996)
                  PASS = .FALSE.
                  GO TO 400
               END IF
  200       CONTINUE
*           Check that 'unreferenced' elements of X have not
*           been altered.
            DO 240 I = 1, (N-1)*INCX + 1, INCX
               DO 220 J = I + 1, I + INCX - 1
                  IF (X(J).NE.CROGUE) THEN
                     WRITE (NOUT,99995) J
                     WRITE (NOUT,99997) N, ALPHA, INCX, LDA
                     WRITE (NOUT,99996)
                     PASS = .FALSE.
                     GO TO 400
                  END IF
  220          CONTINUE
  240       CONTINUE
*           Set Q = conjg(Q').
            DO 260 I = 1, N
               S(I) = -S(I)
  260       CONTINUE
*           Insert zeros below diagonal of A for call to F06TXF.
            DO 300 I = 1, N
               DO 280 J = 1, I - 1
                  A(I,J) = ZERO
  280          CONTINUE
  300       CONTINUE
*           Premultiply R by Q.
            CALL F06TXF('L','B','B',N+1,N,1,N+1,C,S,A,LDA)
*           Now (A) should be the same as (  SAVA  ) .
*                                         (ALPHA*X')
            DO 340 I = 1, N
               DO 320 J = I, N
                  DIF = ABS(A(I,J)-SAVA(I,J))
                  IF (DIF.GT.TOL) THEN
                     WRITE (NOUT,99994) I, J
                     WRITE (NOUT,99997) N, ALPHA, INCX, LDA
                     WRITE (NOUT,99993) N + 1, N + 1, 1, N + 1, LDA
                     WRITE (NOUT,99992) A(I,J), SAVA(I,J)
                     PASS = .FALSE.
                     GO TO 400
                  END IF
  320          CONTINUE
               DIF = ABS(A(N+1,I)-ALPHA*SAVX((I-1)*INCX+1))
               IF (DIF.GT.TOL) THEN
                  WRITE (NOUT,99994) N + 1, I
                  WRITE (NOUT,99997) N, ALPHA, INCX, LDA
                  WRITE (NOUT,99993) N + 1, N + 1, 1, N + 1, LDA
                  WRITE (NOUT,99992) A(N+1,I), SAVX((I-1)*INCX+1)
                  PASS = .FALSE.
                  GO TO 400
               END IF
  340       CONTINUE
  360    CONTINUE
  380 CONTINUE
  400 IF (PASS) THEN
         WRITE (NOUT,99991)
      ELSE
         WRITE (NOUT,99990)
      END IF
      STOP
*
99999 FORMAT (' F06TQF Example Program Results',/1X)
99998 FORMAT (' Element A(',I3,',',I3,') was altered by the call:')
99997 FORMAT (' CALL F06TQF(',I3,', (',1P,D13.5,',',D13.5,') ,X,',I3,
     +       ',A,',I3,',C,S)')
99996 FORMAT (' although it should not have been referenced.')
99995 FORMAT (' Element X(',I3,') was altered by the call:')
99994 FORMAT (' Element A(',I3,',',I3,') was incorrectly computed by t',
     +       'he calls:')
99993 FORMAT (' CALL F06TXF(''L'',''B'',''B'',',I3,',',I3,',',I3,',',I3,
     +       ',C,S,A,',I3,')')
99992 FORMAT ('  as (',1P,D13.5,',',D13.5,') instead of (',D13.5,',',
     +       D13.5,').')
99991 FORMAT (' F06TQF Example Program ends OK')
99990 FORMAT (' F06TQF Example Program ends with ERRORS')
      END
