*     G11SAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          IPMAX, ICM, ISMAX, NRE, LW, NRX
      PARAMETER        (IPMAX=10,ICM=2*IPMAX,ISMAX=1024,NRE=IPMAX,
     +                 LW=4*IPMAX*(IPMAX+16),NRX=ISMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION CGETOL, CHI, RLOGL, SIGLEV
      INTEGER          I, IDF, IFAIL, IP, IPRINT, IS, ISHOW, J, MAXIT,
     +                 N, NITER
      LOGICAL          CHISQR, GPROB
*     .. Local Arrays ..
      DOUBLE PRECISION A(IPMAX), ALPHA(IPMAX), C(IPMAX),
     +                 CM(ICM,2*IPMAX), EXF(ISMAX), EXPP(NRE,IPMAX),
     +                 G(2*IPMAX), OBS(NRE,IPMAX), PIGAM(IPMAX), W(LW),
     +                 XL(ISMAX), Y(ISMAX)
      INTEGER          IOB(ISMAX), IRL(ISMAX)
      LOGICAL          X(NRX,IPMAX)
*     .. External Subroutines ..
      EXTERNAL         G11SAF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G11SAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) IP, N, IS
      CALL X04ABF(1,NOUT)
      IF (IP.GT.0 .AND. IP.LE.IPMAX .AND. IS.GE.0 .AND. IS.LE.ISMAX)
     +    THEN
         DO 20 I = 1, IS
            READ (NIN,*) IRL(I), (X(I,J),J=1,IP)
   20    CONTINUE
         GPROB = .FALSE.
         DO 40 I = 1, IP
            A(I) = 0.5D0
            C(I) = 0.0D0
   40    CONTINUE
*        ** Set IPRINT > 0 to obtain intermediate output **
         IPRINT = -1
         CGETOL = 0.0001D0
         MAXIT = 1000
         CHISQR = .TRUE.
         ISHOW = 7
         IFAIL = -1
*
         CALL G11SAF(IP,N,GPROB,IS,X,NRX,IRL,A,C,IPRINT,CGETOL,MAXIT,
     +               CHISQR,ISHOW,NITER,ALPHA,PIGAM,CM,ICM,G,EXPP,NRE,
     +               OBS,EXF,Y,XL,IOB,RLOGL,CHI,IDF,SIGLEV,W,LW,IFAIL)
*
      END IF
      STOP
      END
