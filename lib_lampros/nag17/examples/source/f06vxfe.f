*     F06VXF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      DOUBLE PRECISION ONE, RROGUE
      PARAMETER        (ONE=1.0D+0,RROGUE=-1.0D+10)
      INTEGER          LDA, NMAX
      PARAMETER        (LDA=17,NMAX=5)
*     .. Local Scalars ..
      COMPLEX*16       CROGUE
      DOUBLE PRECISION DUM, TOL
      INTEGER          DIR, I, J, K1, K2, KK1, KK2, M, MM, N, NN, PIV
      LOGICAL          PASS
      CHARACTER*1      DIRCT1, DIRCT2, PIVOT
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,LDA), SAVA(LDA,LDA)
      DOUBLE PRECISION CL(LDA), CR(LDA), SL(LDA), SR(LDA)
      INTEGER          ENS(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION G05CAF, X02AJF
      EXTERNAL         G05CAF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         F06VXF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, DCMPLX, SQRT
*     .. Data statements ..
      DATA             ENS/0, 1, 2, 8, 17/
*     .. Executable Statements ..
      WRITE (NOUT,99999)
      TOL = X02AJF()*100
      CROGUE = DCMPLX(RROGUE,RROGUE)
      PASS = .TRUE.
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
               DIRCT1 = 'F'
               DIRCT2 = 'B'
            ELSE
               DIRCT1 = 'B'
               DIRCT2 = 'F'
            END IF
            DO 240 MM = 1, NMAX
               M = ENS(MM)
               DO 220 NN = 1, NMAX
                  N = ENS(NN)
                  DO 200 KK1 = 1, NN
                     K1 = ENS(KK1)
                     DO 180 KK2 = 1, NN
                        K2 = ENS(KK2)
*                       Create two unitary matrices PL and PR, one for
*                       application from the left, one from the right.
                        DO 20 I = 1, M
                           SL(I) = (G05CAF(DUM))
                           CL(I) = SQRT(ONE-SL(I)*SL(I))
   20                   CONTINUE
                        DO 40 I = 1, N
                           SR(I) = G05CAF(DUM)
                           CR(I) = SQRT(ONE-SR(I)*SR(I))
   40                   CONTINUE
*                       Generate a random M x N matrix, A, and copy it.
                        DO 80 I = 1, M
                           DO 60 J = 1, N
                              A(I,J) = DCMPLX(G05CAF(DUM),G05CAF(DUM))
                              SAVA(I,J) = A(I,J)
   60                      CONTINUE
   80                   CONTINUE
*                       Hit A from the left with PL.
                        CALL F06VXF('L',PIVOT,DIRCT1,M,N,K1,K2,CL,SL,A,
     +                              LDA)
*                       Hit A from the right with conjg(PR').
                        CALL F06VXF('R',PIVOT,DIRCT1,M,N,K1,K2,CR,SR,A,
     +                              LDA)
*                       Take the transposes of PL and PR.
                        DO 100 I = 1, M
                           SL(I) = -SL(I)
  100                   CONTINUE
                        DO 120 I = 1, N
                           SR(I) = -SR(I)
  120                   CONTINUE
*                       Hit A from the left with conjg(PL').
                        CALL F06VXF('L',PIVOT,DIRCT2,M,N,K1,K2,CL,SL,A,
     +                              LDA)
*                       Hit A from the right with PR.
                        CALL F06VXF('R',PIVOT,DIRCT2,M,N,K1,K2,CR,SR,A,
     +                              LDA)
*                       Now A should be as it was originally.
*                       Check that all elements were computed correctly.
                        DO 160 I = 1, M
                           DO 140 J = 1, N
                              IF (ABS(A(I,J)-SAVA(I,J)).GT.TOL) THEN
                                 WRITE (NOUT,99997) I, J
                                 WRITE (NOUT,99996) 'L', PIVOT, DIRCT1,
     +                             M, N, K1, K2, 'CL', 'SL', LDA
                                 WRITE (NOUT,99996) 'R', PIVOT, DIRCT1,
     +                             M, N, K1, K2, 'CR', 'SR', LDA
                                 WRITE (NOUT,99998)
                                 WRITE (NOUT,99996) 'L', PIVOT, DIRCT2,
     +                             M, N, K1, K2, 'CL', 'SL', LDA
                                 WRITE (NOUT,99996) 'R', PIVOT, DIRCT2,
     +                             M, N, K1, K2, 'CR', 'SR', LDA
                                 WRITE (NOUT,99995) A(I,J), SAVA(I,J)
                                 PASS = .FALSE.
                                 GO TO 300
                              END IF
  140                      CONTINUE
  160                   CONTINUE
  180                CONTINUE
  200             CONTINUE
  220          CONTINUE
  240       CONTINUE
  260    CONTINUE
  280 CONTINUE
  300 IF (PASS) THEN
         WRITE (NOUT,99994)
      ELSE
         WRITE (NOUT,99993)
      END IF
      STOP
*
99999 FORMAT (' F06VXF Example Program Results',/1X)
99998 FORMAT (' PL = conjg(PL'')',/' PR = conjg(PR'')')
99997 FORMAT (' Element A(',I3,',',I3,') was incorrectly computed by t',
     +       'he sequence:')
99996 FORMAT (' CALL F06VXF(''',A,''',''',A,''',''',A,''',',I3,',',I3,
     +       ',',I3,',',I3,',',A,',',A,',A,',I3,')')
99995 FORMAT ('  as (',1P,D13.5,',',D13.5,') instead of (',D13.5,',',
     +       D13.5,').')
99994 FORMAT (' F06VXF Example Program ends OK')
99993 FORMAT (' F06VXF Example Program ends with ERRORS')
      END
