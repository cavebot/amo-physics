*     E02ACF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, M1
      PARAMETER        (N=21,M1=6)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      DOUBLE PRECISION P1
      PARAMETER        (P1=0.1D0)
*     .. Local Scalars ..
      DOUBLE PRECISION REF, S, T, Z
      INTEGER          I, J
*     .. Local Arrays ..
      DOUBLE PRECISION A(M1), X(N), Y(N)
*     .. External Subroutines ..
      EXTERNAL         E02ACF
*     .. Intrinsic Functions ..
      INTRINSIC        EXP, DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02ACF Example Program Results'
      DO 20 I = 1, N
         X(I) = DBLE(I-1)/DBLE(N-1)
         Y(I) = EXP(X(I))
   20 CONTINUE
*
      CALL E02ACF(X,Y,N,A,M1,REF)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   Polynomial coefficients'
      WRITE (NOUT,99998) (A(I),I=1,M1)
      WRITE (NOUT,*)
      WRITE (NOUT,99997) '   Reference deviation = ', REF
      WRITE (NOUT,*)
      WRITE (NOUT,*) '  X     exp(X)    Fit     Residual'
      DO 60 J = 1, 11
         Z = DBLE(J-1)*P1
         S = A(M1)
         DO 40 I = M1 - 1, 1, -1
            S = S*Z + A(I)
   40    CONTINUE
         T = EXP(Z)
         WRITE (NOUT,99999) Z, S, T, S - T
   60 CONTINUE
      STOP
*
99999 FORMAT (1X,F5.2,2F9.4,D11.2)
99998 FORMAT (6X,D12.4)
99997 FORMAT (1X,A,D10.2)
      END
