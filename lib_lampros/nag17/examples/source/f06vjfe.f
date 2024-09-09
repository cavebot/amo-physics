*     F06VJF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          LDB, MAXM
      PARAMETER        (LDB=17,MAXM=5)
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D+0)
*     .. Local Scalars ..
      DOUBLE PRECISION DIF1, DIF2, DUM, TOL
      INTEGER          I, IP, ISWAP, IT, J, K, KK, L, M, MM, N, SID, TRA
      LOGICAL          PASS
      CHARACTER*1      SIDE, TRANS
*     .. Local Arrays ..
      COMPLEX*16       A(LDB,LDB), B(LDB,LDB), C(LDB,LDB), D(LDB,LDB),
     +                 SAVB(LDB,LDB)
      DOUBLE PRECISION RPERM(LDB)
      INTEGER          EMS(MAXM), P(LDB,LDB), PERM(LDB), SAVP(LDB,LDB)
*     .. External Functions ..
      DOUBLE PRECISION G05CAF, X02AJF
      EXTERNAL         G05CAF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         F06VJF, F06VKF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, DCMPLX
*     .. Data statements ..
      DATA             EMS/0, 1, 2, 8, 17/
*     .. Executable Statements ..
      WRITE (NOUT,99999)
      PASS = .TRUE.
      TOL = 10*X02AJF()
*     Generate a random complex matrix in SAVB.
      DO 40 I = 1, LDB
         DO 20 J = 1, LDB
            SAVB(I,J) = DCMPLX(G05CAF(DUM),G05CAF(DUM))
   20    CONTINUE
   40 CONTINUE
      DO 560 MM = 1, MAXM
         M = EMS(MM)
         DO 540 KK = 1, MAXM
            K = EMS(KK)
            DO 520 N = 1, M
               DO 60 I = 1, N
                  PERM(I) = G05CAF(DUM)*M + 1
   60          CONTINUE
               DO 80 I = 1, N
                  IP = G05CAF(DUM)*N + 1
                  IT = PERM(I)
                  PERM(I) = PERM(IP)
                  PERM(IP) = IT
   80          CONTINUE
               DO 100 I = 1, N
                  RPERM(I) = PERM(I) + 0.01D0
  100          CONTINUE
               DO 140 I = 1, M
                  DO 120 J = 1, M
                     IF (I.EQ.J) THEN
                        SAVP(I,J) = 1
                     ELSE
                        SAVP(I,J) = 0
                     END IF
  120             CONTINUE
  140          CONTINUE
               DO 180 I = N, 1, -1
                  ISWAP = PERM(I)
                  DO 160 J = 1, M
                     IT = SAVP(I,J)
                     SAVP(I,J) = SAVP(ISWAP,J)
                     SAVP(ISWAP,J) = IT
  160             CONTINUE
  180          CONTINUE
               DO 500 SID = 1, 2
                  IF (SID.EQ.1) THEN
                     SIDE = 'L'
                  ELSE
                     SIDE = 'R'
                  END IF
                  DO 480 TRA = 1, 2
                     IF (TRA.EQ.1) THEN
                        TRANS = 'N'
                     ELSE
                        TRANS = 'T'
                     END IF
                     DO 220 I = 1, LDB
                        DO 200 J = 1, LDB
                           A(I,J) = SAVB(I,J)
                           B(I,J) = SAVB(I,J)
                           D(I,J) = SAVB(I,J)
  200                   CONTINUE
  220                CONTINUE
                     DO 260 I = 1, M
                        DO 240 J = 1, M
                           P(I,J) = SAVP(I,J)
                           C(I,J) = ZERO
  240                   CONTINUE
  260                CONTINUE
                     CALL F06VJF(SIDE,TRANS,N,PERM,K,B,LDB)
                     CALL F06VKF(SIDE,TRANS,N,RPERM,K,D,LDB)
                     IF (TRANS.EQ.'T') THEN
*                       Transpose P.
                        DO 300 I = 1, M
                           DO 280 J = I + 1, M
                              IT = P(I,J)
                              P(I,J) = P(J,I)
                              P(J,I) = IT
  280                      CONTINUE
  300                   CONTINUE
                     END IF
                     IF (SIDE.EQ.'L') THEN
*                       Premultiply A by P, result in C.
                        DO 360 I = 1, M
                           DO 340 J = 1, K
                              DO 320 L = 1, M
                                 IF (P(I,L).EQ.1) C(I,J) = A(L,J)
  320                         CONTINUE
  340                      CONTINUE
  360                   CONTINUE
                     ELSE
*                       Postmultiply A by P, result in C.
                        DO 420 I = 1, K
                           DO 400 J = 1, M
                              DO 380 L = 1, M
                                 IF (P(L,J).EQ.1) C(I,J) = A(I,L)
  380                         CONTINUE
  400                      CONTINUE
  420                   CONTINUE
                     END IF
*                    Now C should equal B and D. Compare them.
                     DO 460 I = 1, M
                        DO 440 J = 1, K
                           IF (SIDE.EQ.'L') THEN
                              DIF1 = ABS(B(I,J)-C(I,J))
                              DIF2 = ABS(D(I,J)-C(I,J))
                           ELSE
                              DIF1 = ABS(B(J,I)-C(J,I))
                              DIF2 = ABS(D(J,I)-C(J,I))
                           END IF
                           IF (DIF1.GT.TOL) THEN
                              WRITE (NOUT,99998) 'B', I, J
                              WRITE (NOUT,99997) 'F06VJF', SIDE, TRANS,
     +                          N, 'PERM', K, 'B', LDB, M
                              WRITE (NOUT,99996) B(I,J), C(I,J)
                              PASS = .FALSE.
                              GO TO 580
                           ELSE IF (DIF2.GT.TOL) THEN
                              WRITE (NOUT,99998) 'D', I, J
                              WRITE (NOUT,99997) 'F06VKF', SIDE, TRANS,
     +                          N, 'RPERM', K, 'D', LDB, M
                              WRITE (NOUT,99996) D(I,J), C(I,J)
                              PASS = .FALSE.
                              GO TO 580
                           END IF
  440                   CONTINUE
  460                CONTINUE
  480             CONTINUE
  500          CONTINUE
  520       CONTINUE
  540    CONTINUE
  560 CONTINUE
  580 IF (PASS) THEN
         WRITE (NOUT,99995)
      ELSE
         WRITE (NOUT,99994)
      END IF
      STOP
*
99999 FORMAT (' F06VJF Example Program Results',/1X)
99998 FORMAT (' Element ',A,'(',I3,',',I3,') was incorrectly computed ',
     +       'by the call:')
99997 FORMAT (' CALL ',A,'(''',A,''',''',A,''',',I3,',',A,',',I3,',',A,
     +       ',',I3,'),     with M =',I3)
99996 FORMAT ('  as (',1P,D13.5,',',D13.5,') instead of (',D13.5,',',
     +       D13.5,').')
99995 FORMAT (' F06VJF Example Program ends OK')
99994 FORMAT (' F06VJF Example Program ends with ERRORS')
      END
