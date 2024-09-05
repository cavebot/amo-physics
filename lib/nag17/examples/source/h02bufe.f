*     H02BUF Example Program Text
*     Mark 16 Release. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MAXN, MAXM
      PARAMETER        (MAXN=50,MAXM=50)
      INTEGER          LDA
      PARAMETER        (LDA=MAXM)
      DOUBLE PRECISION XBUDEF, XBLDEF
      PARAMETER        (XBUDEF=1.0D+20,XBLDEF=0.0D0)
      INTEGER          LIWORK
      PARAMETER        (LIWORK=2*MAXN+3)
      INTEGER          LWORK
      PARAMETER        (LWORK=2*(MAXM+1)**2+7*MAXN+5*MAXM)
      CHARACTER*3      OPTIM
      PARAMETER        (OPTIM='MIN')
*     .. Local Scalars ..
      DOUBLE PRECISION OBJVAL
      INTEGER          IFAIL, INFILE, ITER, M, N
      LOGICAL          MPSLST
      CHARACTER*8      KBLANK, NMBND, NMOBJ, NMPROB, NMRHS, NMRNG
*     .. Local Arrays ..
      DOUBLE PRECISION A(MAXM,MAXN), AX(MAXM), BL(MAXN+MAXM),
     +                 BU(MAXN+MAXM), CLAMDA(MAXN+MAXM), CVEC(MAXN),
     +                 WORK(LWORK), X(MAXN)
      INTEGER          INTVAR(MAXN), ISTATE(MAXN+MAXM), IWORK(LIWORK)
      CHARACTER*8      CRNAME(MAXN+MAXM)
*     .. External Subroutines ..
      EXTERNAL         E04MFF, E04MHF, H02BUF, H02BVF
*     .. Data statements ..
      DATA             KBLANK/'        '/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'H02BUF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*
*     Initialize parameters
*
      INFILE = NIN
      NMPROB = KBLANK
      NMOBJ = KBLANK
      NMRHS = KBLANK
      NMRNG = KBLANK
      NMBND = KBLANK
      MPSLST = .FALSE.
*
      IFAIL = 0
*
*     Convert the MPSX data file for use by E04MFF
*
      CALL H02BUF(INFILE,MAXN,MAXM,OPTIM,XBLDEF,XBUDEF,NMOBJ,NMRHS,
     +            NMRNG,NMBND,MPSLST,N,M,A,BL,BU,CVEC,X,INTVAR,CRNAME,
     +            NMPROB,ISTATE,IFAIL)
*
*     Solve the problem
*
      IFAIL = -1
*
      CALL E04MHF('Print Level = 5')
*
      CALL E04MFF(N,M,A,LDA,BL,BU,CVEC,ISTATE,X,ITER,OBJVAL,AX,CLAMDA,
     +            IWORK,LIWORK,WORK,LWORK,IFAIL)
*
      IF (IFAIL.EQ.0 .OR. IFAIL.EQ.1 .OR. IFAIL.EQ.3) THEN
*
*        Print solution (using MPSX names)
*
         IFAIL = 0
*
         CALL H02BVF(N,M,A,LDA,BL,BU,X,CLAMDA,ISTATE,CRNAME,IFAIL)
*
      ELSE
         WRITE (NOUT,99999) 'E04MFF terminated with IFAIL = ', IFAIL
      END IF
*
      STOP
*
99999 FORMAT (1X,A,I3)
      END
