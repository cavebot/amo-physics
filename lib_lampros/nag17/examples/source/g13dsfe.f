*     G13DSF Example Program Text
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          KMAX, IK, IPMAX, IQMAX, NMAX, NPARMX, LWORK, LIW,
     +                 MMAX, IRCM, ICM
      PARAMETER        (KMAX=3,IK=KMAX,IPMAX=3,IQMAX=3,NMAX=100,
     +                 NPARMX=(IPMAX+IQMAX)*KMAX*KMAX+KMAX,LWORK=2000,
     +                 LIW=100,MMAX=20,IRCM=MMAX*KMAX*KMAX,ICM=NPARMX)
*     .. Local Scalars ..
      DOUBLE PRECISION CGETOL, CHI, RLOGL, SIGLEV
      INTEGER          I, IDF, IFAIL, IP, IPRINT, IQ, ISHOW, J, K, M,
     +                 MAXCAL, N, NITER, NPAR
      LOGICAL          EXACT, MEAN
*     .. Local Arrays ..
      DOUBLE PRECISION CM(ICM,NPARMX), G(NPARMX), PAR(NPARMX),
     +                 QQ(IK,KMAX), R(IK,IK,MMAX), R0(IK,KMAX),
     +                 RCM(IRCM,MMAX*KMAX*KMAX), V(IK,NMAX), W(IK,NMAX),
     +                 WORK(LWORK)
      INTEGER          IW(LIW)
      LOGICAL          PARHLD(NPARMX)
*     .. External Subroutines ..
      EXTERNAL         G13DCF, G13DSF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13DSF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) K, N
*
      CALL X04ABF(1,NOUT)
*
      IF (K.GT.0 .AND. K.LE.KMAX .AND. N.GE.3 .AND. N.LE.NMAX) THEN
         DO 20 I = 1, K
            READ (NIN,*) (W(I,J),J=1,N)
   20    CONTINUE
         READ (NIN,*) IP, IQ, MEAN, M
         IF (IP.GE.0 .AND. IP.LE.IPMAX .AND. IQ.GE.0 .AND. IQ.LE.IQMAX)
     +       THEN
            NPAR = (IP+IQ)*K*K
            IF (MEAN) NPAR = NPAR + K
            IF (NPAR.LE.NPARMX) THEN
               DO 40 I = 1, NPAR
                  PAR(I) = 0.0D0
                  PARHLD(I) = .FALSE.
   40          CONTINUE
               DO 80 J = 1, K
                  DO 60 I = J, K
                     QQ(I,J) = 0.0D0
   60             CONTINUE
   80          CONTINUE
               PARHLD(3) = .TRUE.
               EXACT = .TRUE.
*              ** Set IPRINT > 0 to obtain intermediate output **
               IPRINT = -1
               CGETOL = 0.0001D0
               MAXCAL = 40*NPAR*(NPAR+5)
               ISHOW = 2
               IFAIL = 1
*
               CALL G13DCF(K,N,IP,IQ,MEAN,PAR,NPAR,QQ,IK,W,PARHLD,EXACT,
     +                     IPRINT,CGETOL,MAXCAL,ISHOW,NITER,RLOGL,V,G,
     +                     CM,ICM,WORK,LWORK,IW,LIW,IFAIL)
*
               WRITE (NOUT,*)
               IF (IFAIL.NE.0) THEN
                  WRITE (NOUT,99999) 'G13DCF fails. IFAIL =', IFAIL
                  WRITE (NOUT,*)
               END IF
               IF ((IFAIL.EQ.0 .OR. IFAIL.GE.4) .AND. M.LE.MMAX) THEN
                  WRITE (NOUT,*) 'Output from G13DSF'
                  WRITE (NOUT,*)
                  ISHOW = 1
                  IFAIL = -1
*
                  CALL G13DSF(K,N,V,IK,IP,IQ,M,PAR,PARHLD,QQ,ISHOW,R0,R,
     +                        RCM,IRCM,CHI,IDF,SIGLEV,IW,LIW,WORK,LWORK,
     +                        IFAIL)
*
                  IF (IFAIL.NE.0) WRITE (NOUT,99999)
     +                'G13DSF fails. IFAIL =', IFAIL
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
      END
