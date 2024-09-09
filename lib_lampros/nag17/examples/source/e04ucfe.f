*     E04UCF Example Program Text
*     Mark 16 Release. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NCLMAX, NCNMAX
      PARAMETER        (NMAX=10,NCLMAX=10,NCNMAX=10)
      INTEGER          LDA, LDCJ, LDR
      PARAMETER        (LDA=NCLMAX,LDCJ=NCNMAX,LDR=NMAX)
      INTEGER          LIWORK, LWORK
      PARAMETER        (LIWORK=100,LWORK=1000)
*     .. Local Scalars ..
      DOUBLE PRECISION OBJF
      INTEGER          I, IFAIL, ITER, J, N, NCLIN, NCNLN
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), BL(NMAX+NCLMAX+NCNMAX),
     +                 BU(NMAX+NCLMAX+NCNMAX), C(NCNMAX),
     +                 CJAC(LDCJ,NMAX), CLAMDA(NMAX+NCLMAX+NCNMAX),
     +                 OBJGRD(NMAX), R(LDR,NMAX), USER(1), WORK(LWORK),
     +                 X(NMAX)
      INTEGER          ISTATE(NMAX+NCLMAX+NCNMAX), IUSER(1),
     +                 IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         CONFUN, E04UCF, OBJFUN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04UCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NCLIN, NCNLN
      IF (N.LE.NMAX .AND. NCLIN.LE.NCLMAX .AND. NCNLN.LE.NCNMAX) THEN
*
*        Read A, BL, BU and X from data file
*
         IF (NCLIN.GT.0) READ (NIN,*) ((A(I,J),J=1,N),I=1,NCLIN)
         READ (NIN,*) (BL(I),I=1,N+NCLIN+NCNLN)
         READ (NIN,*) (BU(I),I=1,N+NCLIN+NCNLN)
         READ (NIN,*) (X(I),I=1,N)
*
*        Solve the problem
*
         IFAIL = -1
*
         CALL E04UCF(N,NCLIN,NCNLN,LDA,LDCJ,LDR,A,BL,BU,CONFUN,OBJFUN,
     +               ITER,ISTATE,C,CJAC,CLAMDA,OBJF,OBJGRD,R,X,IWORK,
     +               LIWORK,WORK,LWORK,IUSER,USER,IFAIL)
*
      END IF
      STOP
      END
      SUBROUTINE OBJFUN(MODE,N,X,OBJF,OBJGRD,NSTATE,IUSER,USER)
*     Routine to evaluate objective function and its 1st derivatives.
*     .. Parameters ..
      DOUBLE PRECISION  ONE, TWO
      PARAMETER         (ONE=1.0D0,TWO=2.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  OBJF
      INTEGER           MODE, N, NSTATE
*     .. Array Arguments ..
      DOUBLE PRECISION  OBJGRD(N), USER(*), X(N)
      INTEGER           IUSER(*)
*     .. Executable Statements ..
      IF (MODE.EQ.0 .OR. MODE.EQ.2) OBJF = X(1)*X(4)*(X(1)+X(2)+X(3)) +
     +    X(3)
*
      IF (MODE.EQ.1 .OR. MODE.EQ.2) THEN
         OBJGRD(1) = X(4)*(TWO*X(1)+X(2)+X(3))
         OBJGRD(2) = X(1)*X(4)
         OBJGRD(3) = X(1)*X(4) + ONE
         OBJGRD(4) = X(1)*(X(1)+X(2)+X(3))
      END IF
*
      RETURN
      END
*
      SUBROUTINE CONFUN(MODE,NCNLN,N,LDCJ,NEEDC,X,C,CJAC,NSTATE,IUSER,
     +                  USER)
*     Routine to evaluate the nonlinear constraints and their 1st
*     derivatives.
*     .. Parameters ..
      DOUBLE PRECISION  ZERO, TWO
      PARAMETER         (ZERO=0.0D0,TWO=2.0D0)
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
         IF (MODE.EQ.0 .OR. MODE.EQ.2) C(1) = X(1)**2 + X(2)**2 + X(3)
     +       **2 + X(4)**2
         IF (MODE.EQ.1 .OR. MODE.EQ.2) THEN
            CJAC(1,1) = TWO*X(1)
            CJAC(1,2) = TWO*X(2)
            CJAC(1,3) = TWO*X(3)
            CJAC(1,4) = TWO*X(4)
         END IF
      END IF
*
      IF (NEEDC(2).GT.0) THEN
         IF (MODE.EQ.0 .OR. MODE.EQ.2) C(2) = X(1)*X(2)*X(3)*X(4)
         IF (MODE.EQ.1 .OR. MODE.EQ.2) THEN
            CJAC(2,1) = X(2)*X(3)*X(4)
            CJAC(2,2) = X(1)*X(3)*X(4)
            CJAC(2,3) = X(1)*X(2)*X(4)
            CJAC(2,4) = X(1)*X(2)*X(3)
         END IF
      END IF
*
      RETURN
      END
