*     F06QHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D+0)
      INTEGER          LDA, MAXN
      PARAMETER        (LDA=17,MAXN=5)
*     .. Local Scalars ..
      DOUBLE PRECISION CONST, DIAG, DUM, TOL
      INTEGER          I, J, M, MM, N, NN
      LOGICAL          PASS
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,LDA), B(LDA,LDA), C(LDA,LDA)
      INTEGER          ENS(MAXN)
*     .. External Functions ..
      DOUBLE PRECISION G05CAF, X02AJF
      EXTERNAL         G05CAF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         CPYCHK, F06QFF, F06QHF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS
*     .. Data statements ..
      DATA             ENS/0, 1, 2, 8, 17/
*     .. Executable Statements ..
      WRITE (NOUT,99999)
      PASS = .TRUE.
      TOL = X02AJF()*10
      DO 160 MM = 1, MAXN
         M = ENS(MM)
         DO 140 NN = 1, MAXN
            N = ENS(NN)
            DO 40 I = 1, M
               DO 20 J = 1, N
                  A(I,J) = ZERO
                  B(I,J) = ZERO
                  C(I,J) = ZERO
   20          CONTINUE
   40       CONTINUE
            CONST = G05CAF(DUM)
            DIAG = G05CAF(DUM)
            CALL F06QHF('L',M,N,CONST,DIAG,A,LDA)
            CALL F06QFF('L',M,N,A,LDA,C,LDA)
            CALL CPYCHK(M,N,A,C,LDA,PASS,NOUT,'L','A','C')
            IF ( .NOT. PASS) GO TO 180
            CALL F06QHF('G',M,N,CONST,DIAG,B,LDA)
            CALL F06QFF('G',M,N,B,LDA,C,LDA)
            CALL CPYCHK(M,N,B,C,LDA,PASS,NOUT,'G','B','C')
            IF ( .NOT. PASS) GO TO 180
            CALL F06QHF('U',M,N,ZERO,DIAG,B,LDA)
            CALL F06QFF('U',M,N,B,LDA,C,LDA)
            CALL CPYCHK(M,N,B,C,LDA,PASS,NOUT,'U','B','C')
            IF ( .NOT. PASS) GO TO 180
*           Now lower triangles of A and B should be equal.
            DO 80 I = 1, M
               DO 60 J = 1, N
                  IF (ABS(A(I,J)-B(I,J)).GT.TOL) THEN
                     WRITE (NOUT,99998)
                     WRITE (NOUT,99997) 'L', M, N, CONST, DIAG, 'A', LDA
                     WRITE (NOUT,99997) 'G', M, N, CONST, DIAG, 'B', LDA
                     WRITE (NOUT,99997) 'U', M, N, ZERO, DIAG, 'B', LDA
                     WRITE (NOUT,99996) I, J, A(I,J), B(I,J)
                     PASS = .FALSE.
                     GO TO 180
                  END IF
   60          CONTINUE
   80       CONTINUE
            CALL F06QHF('G',M,N,CONST,DIAG,A,LDA)
            CALL F06QHF('L',M,N,CONST,DIAG,B,LDA)
            CALL F06QHF('U',M,N,CONST,DIAG,B,LDA)
            DO 120 I = 1, M
               DO 100 J = 1, N
                  IF (ABS(A(I,J)-B(I,J)).GT.TOL) THEN
                     WRITE (NOUT,99998)
                     WRITE (NOUT,99997) 'G', M, N, CONST, DIAG, 'A', LDA
                     WRITE (NOUT,99997) 'L', M, N, CONST, DIAG, 'B', LDA
                     WRITE (NOUT,99997) 'U', M, N, CONST, DIAG, 'B', LDA
                     WRITE (NOUT,99996) I, J, A(I,J), B(I,J)
                     PASS = .FALSE.
                     GO TO 180
                  END IF
  100          CONTINUE
  120       CONTINUE
  140    CONTINUE
  160 CONTINUE
  180 IF (PASS) THEN
         WRITE (NOUT,99995)
      ELSE
         WRITE (NOUT,99994)
      END IF
      STOP
*
99999 FORMAT (' F06QHF Example Program Results',/1X)
99998 FORMAT (' The sequence:')
99997 FORMAT (' CALL F06QHF(''',A,''',',I3,',',I3,',',1P,D12.5,',',
     +       D12.5,',',A,',',I3,')')
99996 FORMAT (' returned elements A(I,J) and B(I,J), for I =',I3,' and',
     +       ' J =',I3,',',/'  as ',1P,D12.5,' and ',D12.5,' respectiv',
     +       'ely.',/'  These elements should be equal.')
99995 FORMAT (' F06QHF Example Program ends OK')
99994 FORMAT (' F06QHF Example Program ends with ERRORS')
      END
*
      SUBROUTINE CPYCHK(M,N,A,B,LDA,PASS,NOUT,MAT,MAT1,MAT2)
*     .. Scalar Arguments ..
      INTEGER           LDA, M, N, NOUT
      LOGICAL           PASS
      CHARACTER*1       MAT, MAT1, MAT2
*     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,LDA), B(LDA,LDA)
*     .. Local Scalars ..
      INTEGER           I, J
*     .. Executable Statements ..
      DO 40 I = 1, M
         DO 20 J = 1, N
            IF (A(I,J).NE.B(I,J)) THEN
               PASS = .FALSE.
               WRITE (NOUT,99999)
               WRITE (NOUT,99998) MAT, M, N, MAT1, LDA, MAT2, LDA
               WRITE (NOUT,99997) MAT2, I, J, B(I,J), A(I,J)
               RETURN
            END IF
   20    CONTINUE
   40 CONTINUE
      RETURN
*
99999 FORMAT (' The call:')
99998 FORMAT (' CALL F06QFF(''',A,''',',I3,',',I3,',',A,',',I3,',',A,
     +       ',',I3,')')
99997 FORMAT (' returned ',A,'(',I3,',',I3,') as ',1P,D12.5,' instead ',
     +       'of ',D12.5,' .')
      END
