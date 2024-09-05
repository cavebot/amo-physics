*     E04NFF Example Program Text
*     Mark 16 Release. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NCMAX
      PARAMETER        (NMAX=10,NCMAX=10)
      INTEGER          LDA, LDH
      PARAMETER        (LDA=NMAX,LDH=NMAX)
      INTEGER          LIWORK, LWORK
      PARAMETER        (LIWORK=1000,LWORK=10000)
*     .. Local Scalars ..
      DOUBLE PRECISION OBJ
      INTEGER          I, IFAIL, ITER, J, N, NCLIN
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), AX(NCMAX), BL(NMAX+NCMAX),
     +                 BU(NMAX+NCMAX), CLAMDA(NMAX+NCMAX), CVEC(NMAX),
     +                 H(LDH,NMAX), WORK(LWORK), X(NMAX)
      INTEGER          ISTATE(NMAX+NCMAX), IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         E04NFF, E04NFU
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04NFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NCLIN
      IF (N.LE.NMAX .AND. NCLIN.LE.NCMAX) THEN
*
*        Read CVEC, A, BL, BU, X and H from data file
*
         READ (NIN,*) (CVEC(I),I=1,N)
         READ (NIN,*) ((A(I,J),J=1,N),I=1,NCLIN)
         READ (NIN,*) (BL(I),I=1,N+NCLIN)
         READ (NIN,*) (BU(I),I=1,N+NCLIN)
         READ (NIN,*) (X(I),I=1,N)
         READ (NIN,*) ((H(I,J),J=1,N),I=1,N)
*
*        Solve the problem
*
         IFAIL = -1
*
         CALL E04NFF(N,NCLIN,A,LDA,BL,BU,CVEC,H,LDH,E04NFU,ISTATE,X,
     +               ITER,OBJ,AX,CLAMDA,IWORK,LIWORK,WORK,LWORK,IFAIL)
*
      END IF
      STOP
      END
