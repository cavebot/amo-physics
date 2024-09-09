*     H02BFF Example Program Text
*     Mark 16 Release. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MAXN, MAXM
      PARAMETER        (MAXN=50,MAXM=50)
      DOUBLE PRECISION XBLDEF, XBUDEF
      PARAMETER        (XBLDEF=0.0D0,XBUDEF=1.0D+20)
      INTEGER          MAXDPT
      PARAMETER        (MAXDPT=3*MAXN/2)
      INTEGER          MSGLVL
      PARAMETER        (MSGLVL=5)
      INTEGER          LIWORK
      PARAMETER        (LIWORK=(25+MAXN+MAXM)*MAXDPT+2*MAXM+7*MAXN+4)
      INTEGER          LRWORK
      PARAMETER        (LRWORK=MAXDPT*(MAXN+2)
     +                 +2*MAXN**2+MAXM*MAXN+18*MAXN+15*MAXM)
      CHARACTER*3      OPTIM
      PARAMETER        (OPTIM='MIN')
*     .. Local Scalars ..
      INTEGER          IFAIL, INFILE, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION RWORK(LRWORK), X(MAXN)
      INTEGER          IWORK(LIWORK)
      CHARACTER*8      CRNAME(MAXN+MAXM)
*     .. External Subroutines ..
      EXTERNAL         H02BFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'H02BFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*
*     Solve the problem
*
      INFILE = NIN
      IFAIL = 0
*
      CALL H02BFF(INFILE,MAXN,MAXM,OPTIM,XBLDEF,XBUDEF,MAXDPT,MSGLVL,N,
     +            M,X,CRNAME,IWORK,LIWORK,RWORK,LRWORK,IFAIL)
*
      STOP
      END
