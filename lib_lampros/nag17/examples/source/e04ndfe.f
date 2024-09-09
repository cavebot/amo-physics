*     E04NDF Example Program Text
*     Mark 16 Release. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, NCMAX
      PARAMETER        (MMAX=10,NMAX=10,NCMAX=10)
      INTEGER          LDC, LDA
      PARAMETER        (LDC=NCMAX,LDA=MMAX)
      INTEGER          LIWORK, LWORK
      PARAMETER        (LIWORK=100,LWORK=1000)
*     .. Local Scalars ..
      DOUBLE PRECISION OBJ
      INTEGER          I, IFAIL, INFORM, ITER, J, M, N, NCLIN
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(MMAX), BL(NMAX+NCMAX),
     +                 BU(NMAX+NCMAX), C(LDC,NMAX), CLAMDA(NMAX+NCMAX),
     +                 CVEC(NMAX), WORK(LWORK), X(NMAX)
      INTEGER          ISTATE(NMAX+NCMAX), IWORK(LIWORK), KX(NMAX)
*     .. External Subroutines ..
      EXTERNAL         E04NCF, E04NDF, E04NEF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04NDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N, NCLIN
      IF (M.LE.MMAX .AND. N.LE.NMAX .AND. NCLIN.LE.NCMAX) THEN
*
*        Read CVEC, A, C, BL, BU and X from data file
*
         READ (NIN,*) (CVEC(I),I=1,N)
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
         READ (NIN,*) ((C(I,J),J=1,N),I=1,NCLIN)
         READ (NIN,*) (BL(I),I=1,N+NCLIN)
         READ (NIN,*)
         READ (NIN,*) (BU(I),I=1,N+NCLIN)
         READ (NIN,*)
         READ (NIN,*) (X(I),I=1,N)
*
*        Set two options using E04NEF
*
         CALL E04NEF(' Infinite Bound Size = 1.0D+25 ')
*
         CALL E04NEF(' Problem Type = QP2 ')
*
*        Set the unit number for advisory messages to NOUT
*
         CALL X04ABF(1,NOUT)
*
*        Read the options file for the remaining options
*
         CALL E04NDF(NIN,INFORM)
*
         IF (INFORM.NE.0) THEN
            WRITE (NOUT,99999) 'E04NDF terminated with INFORM = ',
     +        INFORM
            STOP
         END IF
*
*        Solve the problem
*
         IFAIL = -1
*
         CALL E04NCF(M,N,NCLIN,LDC,LDA,C,BL,BU,CVEC,ISTATE,KX,X,A,B,
     +               ITER,OBJ,CLAMDA,IWORK,LIWORK,WORK,LWORK,IFAIL)
*
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
      END
