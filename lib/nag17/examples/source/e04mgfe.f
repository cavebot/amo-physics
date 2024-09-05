*     E04MGF Example Program Text
*     Mark 16 Release. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NCMAX
      PARAMETER        (NMAX=10,NCMAX=10)
      INTEGER          LDA
      PARAMETER        (LDA=NMAX)
      INTEGER          LIWORK, LWORK
      PARAMETER        (LIWORK=1000,LWORK=10000)
*     .. Local Scalars ..
      DOUBLE PRECISION OBJ
      INTEGER          I, IFAIL, INFORM, ITER, J, N, NCLIN
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), AX(NCMAX), BL(NMAX+NCMAX),
     +                 BU(NMAX+NCMAX), CLAMDA(NMAX+NCMAX), CVEC(NMAX),
     +                 WORK(LWORK), X(NMAX)
      INTEGER          ISTATE(NMAX+NCMAX), IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         E04MFF, E04MGF, E04MHF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04MGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NCLIN
      IF (N.LE.NMAX .AND. NCLIN.LE.NCMAX) THEN
*
*        Read CVEC, A, BL, BU and X from data file
*
         READ (NIN,*) (CVEC(I),I=1,N)
         READ (NIN,*) ((A(I,J),J=1,N),I=1,NCLIN)
         READ (NIN,*) (BL(I),I=1,N+NCLIN)
         READ (NIN,*) (BU(I),I=1,N+NCLIN)
         READ (NIN,*) (X(I),I=1,N)
*
*        Set three options using E04MHF
*
         CALL E04MHF(' Print Level = 1 ')
*
         CALL E04MHF(' Check Frequency = 10 ')
*
         CALL E04MHF(' Infinite Bound Size = 1.0D+25 ')
*
*        Set the unit number for advisory messages to NOUT
*
         CALL X04ABF(1,NOUT)
*
*        Read the options file for the remaining options
*
         CALL E04MGF(NIN,INFORM)
*
         IF (INFORM.NE.0) THEN
            WRITE (NOUT,99999) 'E04MGF terminated with INFORM = ',
     +        INFORM
            STOP
         END IF
*
*        Solve the problem
*
         IFAIL = -1
*
         CALL E04MFF(N,NCLIN,A,LDA,BL,BU,CVEC,ISTATE,X,ITER,OBJ,AX,
     +               CLAMDA,IWORK,LIWORK,WORK,LWORK,IFAIL)
*
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
      END
