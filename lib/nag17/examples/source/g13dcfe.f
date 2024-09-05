*     G13DCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          KMAX, IK, IPMAX, IQMAX, NMAX, NPARMX, ICM, LWORK,
     +                 LIW
      PARAMETER        (KMAX=3,IK=KMAX,IPMAX=3,IQMAX=3,NMAX=100,
     +                 NPARMX=(IPMAX+IQMAX)*KMAX*KMAX+KMAX,ICM=NPARMX,
     +                 LWORK=2000,LIW=100)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION CGETOL, RLOGL
      INTEGER          I, IFAIL, IP, IPRINT, IQ, ISHOW, J, K, MAXCAL, N,
     +                 NITER, NPAR
      LOGICAL          EXACT, MEAN
*     .. Local Arrays ..
      DOUBLE PRECISION CM(ICM,NPARMX), G(NPARMX), PAR(NPARMX),
     +                 QQ(IK,KMAX), V(IK,NMAX), W(IK,NMAX), WORK(LWORK)
      INTEGER          IW(LIW)
      LOGICAL          PARHLD(NPARMX)
*     .. External Subroutines ..
      EXTERNAL         G13DCF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13DCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) K, IP, IQ, N, MEAN
      CALL X04ABF(1,NOUT)
      WRITE (NOUT,*)
      IF (K.GT.0 .AND. K.LE.KMAX .AND. IP.GE.0 .AND. IP.LE.IPMAX .AND.
     +    IQ.GE.0 .AND. IQ.LE.IQMAX) THEN
         NPAR = (IP+IQ)*K*K
         IF (MEAN) NPAR = NPAR + K
         IF ((N.LE.NMAX) .AND. (NPAR.LE.NPARMX)) THEN
            DO 20 I = 1, NPAR
               PAR(I) = 0.0D0
               PARHLD(I) = .FALSE.
   20       CONTINUE
*
*           Set all elements of Q to zero to use covariance matrix
*           between the K time series as the initial estimate of the
*           covariance matrix
*
            DO 60 J = 1, K
               DO 40 I = J, K
                  QQ(I,J) = 0.0D0
   40          CONTINUE
   60       CONTINUE
            DO 80 I = 1, K
               READ (NIN,*) (W(I,J),J=1,N)
   80       CONTINUE
            PARHLD(3) = .TRUE.
            EXACT = .TRUE.
*           ** Set IPRINT .GT. 0 to obtain intermediate output
            IPRINT = -1
            CGETOL = 0.0001D0
            MAXCAL = 40*NPAR*(NPAR+5)
            ISHOW = 2
            IFAIL = -1
*
            CALL G13DCF(K,N,IP,IQ,MEAN,PAR,NPAR,QQ,IK,W,PARHLD,EXACT,
     +                  IPRINT,CGETOL,MAXCAL,ISHOW,NITER,RLOGL,V,G,CM,
     +                  ICM,WORK,LWORK,IW,LIW,IFAIL)
*
         END IF
      END IF
      STOP
      END
