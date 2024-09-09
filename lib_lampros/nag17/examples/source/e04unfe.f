*     E04UNF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, NCLMAX, NCNMAX
      PARAMETER        (MMAX=50,NMAX=10,NCLMAX=10,NCNMAX=10)
      INTEGER          LDA, LDCJ, LDFJ, LDR
      PARAMETER        (LDA=NCLMAX,LDCJ=NCNMAX,LDFJ=MMAX,LDR=NMAX)
      INTEGER          LIWORK, LWORK
      PARAMETER        (LIWORK=100,LWORK=1000)
*     .. Local Scalars ..
      DOUBLE PRECISION OBJF
      INTEGER          I, IFAIL, ITER, J, M, N, NCLIN, NCNLN
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), BL(NMAX+NCLMAX+NCNMAX),
     +                 BU(NMAX+NCLMAX+NCNMAX), C(NCNMAX),
     +                 CJAC(LDCJ,NMAX), CLAMDA(NMAX+NCLMAX+NCNMAX),
     +                 F(MMAX), FJAC(LDFJ,NMAX), R(LDR,NMAX), USER(1),
     +                 WORK(LWORK), X(NMAX), Y(MMAX)
      INTEGER          ISTATE(NMAX+NCLMAX+NCNMAX), IUSER(1),
     +                 IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         CONFUN, E04UNF, OBJFUN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04UNF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      READ (NIN,*) NCLIN, NCNLN
      IF (M.LE.MMAX .AND. N.LE.NMAX .AND. NCLIN.LE.NCLMAX .AND.
     +    NCNLN.LE.NCNMAX) THEN
*
*        Read A, Y, BL, BU and X from data file
*
         IF (NCLIN.GT.0) READ (NIN,*) ((A(I,J),J=1,N),I=1,NCLIN)
         READ (NIN,*) (Y(I),I=1,M)
         READ (NIN,*) (BL(I),I=1,N+NCLIN+NCNLN)
         READ (NIN,*) (BU(I),I=1,N+NCLIN+NCNLN)
         READ (NIN,*) (X(I),I=1,N)
*
*        Solve the problem
*
         IFAIL = -1
*
         CALL E04UNF(M,N,NCLIN,NCNLN,LDA,LDCJ,LDFJ,LDR,A,BL,BU,Y,CONFUN,
     +               OBJFUN,ITER,ISTATE,C,CJAC,F,FJAC,CLAMDA,OBJF,R,X,
     +               IWORK,LIWORK,WORK,LWORK,IUSER,USER,IFAIL)
*
      END IF
      STOP
      END
      SUBROUTINE OBJFUN(MODE,M,N,LDFJ,X,F,FJAC,NSTATE,IUSER,USER)
*     Routine to evaluate the subfunctions and their 1st derivatives.
*     .. Parameters ..
      DOUBLE PRECISION  PT49, ONE, EIGHT
      PARAMETER         (PT49=0.49D0,ONE=1.0D0,EIGHT=8.0D0)
*     .. Scalar Arguments ..
      INTEGER           LDFJ, M, MODE, N, NSTATE
*     .. Array Arguments ..
      DOUBLE PRECISION  F(*), FJAC(LDFJ,*), USER(*), X(N)
      INTEGER           IUSER(*)
*     .. Local Scalars ..
      DOUBLE PRECISION  AI, TEMP, X1, X2
      INTEGER           I
      LOGICAL           MODE02, MODE12
*     .. Local Arrays ..
      DOUBLE PRECISION  A(44)
*     .. Intrinsic Functions ..
      INTRINSIC         EXP
*     .. Data statements ..
      DATA              A/8.0D0, 8.0D0, 10.0D0, 10.0D0, 10.0D0, 10.0D0,
     +                  12.0D0, 12.0D0, 12.0D0, 12.0D0, 14.0D0, 14.0D0,
     +                  14.0D0, 16.0D0, 16.0D0, 16.0D0, 18.0D0, 18.0D0,
     +                  20.0D0, 20.0D0, 20.0D0, 22.0D0, 22.0D0, 22.0D0,
     +                  24.0D0, 24.0D0, 24.0D0, 26.0D0, 26.0D0, 26.0D0,
     +                  28.0D0, 28.0D0, 30.0D0, 30.0D0, 30.0D0, 32.0D0,
     +                  32.0D0, 34.0D0, 36.0D0, 36.0D0, 38.0D0, 38.0D0,
     +                  40.0D0, 42.0D0/
*     .. Executable Statements ..
      X1 = X(1)
      X2 = X(2)
      MODE02 = MODE .EQ. 0 .OR. MODE .EQ. 2
      MODE12 = MODE .EQ. 1 .OR. MODE .EQ. 2
      DO 20 I = 1, M
         AI = A(I)
         TEMP = EXP(-X2*(AI-EIGHT))
         IF (MODE02) F(I) = X1 + (PT49-X1)*TEMP
         IF (MODE12) THEN
            FJAC(I,1) = ONE - TEMP
            FJAC(I,2) = -(PT49-X1)*(AI-EIGHT)*TEMP
         END IF
   20 CONTINUE
*
      RETURN
      END
*
      SUBROUTINE CONFUN(MODE,NCNLN,N,LDCJ,NEEDC,X,C,CJAC,NSTATE,IUSER,
     +                  USER)
*     Routine to evaluate the nonlinear constraint and its 1st
*     derivatives.
*     .. Parameters ..
      DOUBLE PRECISION  ZERO, PT09, PT49
      PARAMETER         (ZERO=0.0D0,PT09=0.09D0,PT49=0.49D0)
*     .. Scalar Arguments ..
      INTEGER           LDCJ, MODE, N, NCNLN, NSTATE
*     .. Array Arguments ..
      DOUBLE PRECISION  C(*), CJAC(LDCJ,*), USER(*), X(N)
      INTEGER           IUSER(*), NEEDC(*)
*     .. Local Scalars ..
      INTEGER           I, J
*     .. Executable Statements ..
      IF (NSTATE.EQ.1) THEN
*        First call to CONFUN.  Set all Jacobian elements to zero.
*        Note that this will only work when 'Derivative Level = 3'
*        (the default; see Section 11.2).
         DO 40 J = 1, N
            DO 20 I = 1, NCNLN
               CJAC(I,J) = ZERO
   20       CONTINUE
   40    CONTINUE
      END IF
*
      IF (NEEDC(1).GT.0) THEN
         IF (MODE.EQ.0 .OR. MODE.EQ.2) C(1) = -PT09 - X(1)*X(2) +
     +       PT49*X(2)
         IF (MODE.EQ.1 .OR. MODE.EQ.2) THEN
            CJAC(1,1) = -X(2)
            CJAC(1,2) = -X(1) + PT49
         END IF
      END IF
*
      RETURN
      END
