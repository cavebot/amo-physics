*     H02BBF Example Program Text
*     Mark 16 Revised. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=10,MMAX=10)
      INTEGER          LDA
      PARAMETER        (LDA=MMAX)
      INTEGER          LIWORK, LRWORK
      PARAMETER        (LIWORK=1000,LRWORK=1000)
*     .. Local Scalars ..
      DOUBLE PRECISION BIGBND, OBJMIP, TOLFES, TOLIV
      INTEGER          I, IFAIL, INTFST, ITMAX, J, M, MAXDPT, MAXNOD,
     +                 MSGLVL, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), BL(MMAX+NMAX), BU(MMAX+NMAX),
     +                 CVEC(NMAX), RWORK(LRWORK), X(NMAX)
      INTEGER          INTVAR(NMAX), IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         H02BBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'H02BBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M
      IF (N.LE.NMAX .AND. M.LE.MMAX) THEN
*
*        Read ITMAX, MSGLVL, MAXNOD, INTFST, MAXDPT, TOLFES, TOLIV,
*        CVEC, A, BIGBND, BL, BU, INTVAR and X from data file
*
         READ (NIN,*) ITMAX, MSGLVL
         READ (NIN,*) MAXNOD
         READ (NIN,*) INTFST, MAXDPT
         READ (NIN,*) TOLFES, TOLIV
         READ (NIN,*) (CVEC(I),I=1,N)
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
         READ (NIN,*) BIGBND
         READ (NIN,*) (BL(I),I=1,N+M)
         READ (NIN,*) (BU(I),I=1,N+M)
         READ (NIN,*) (INTVAR(I),I=1,N)
         READ (NIN,*) (X(I),I=1,N)
*
*        Solve the IP problem
*
         IFAIL = -1
*
         CALL H02BBF(ITMAX,MSGLVL,N,M,A,LDA,BL,BU,INTVAR,CVEC,MAXNOD,
     +               INTFST,MAXDPT,TOLIV,TOLFES,BIGBND,X,OBJMIP,IWORK,
     +               LIWORK,RWORK,LRWORK,IFAIL)
*
      END IF
      STOP
      END
