*     G13ASF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NPARMX, NPPCMX, NXMAX, IRES, LIWORK, LWORK, MMAX,
     +                 ICM, IRCM
      PARAMETER        (NPARMX=10,NPPCMX=NPARMX+1,NXMAX=50,IRES=550,
     +                 LIWORK=10,LWORK=500,MMAX=10,ICM=NPARMX+1,
     +                 IRCM=MMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION C, CHI, S, SIGLEV
      INTEGER          I, IDF, IFAIL, ISHOW, ITC, KFC, KPIV, M, NDF,
     +                 NIT, NPAR, NPPC, NRES, NST, NX
*     .. Local Arrays ..
      DOUBLE PRECISION CM(ICM,ICM), PAR(NPARMX), R(MMAX),
     +                 RCM(IRCM,MMAX), RES(IRES), SD(NPPCMX), ST(NXMAX),
     +                 WORK(LWORK), X(NXMAX)
      INTEGER          ISF(4), IWORK(LIWORK), MR(7)
*     .. External Subroutines ..
      EXTERNAL         G13AFF, G13ASF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13ASF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NX
      IF (NX.GT.0 .AND. NX.LE.NXMAX) THEN
         READ (NIN,*) (X(I),I=1,NX)
         READ (NIN,*) (MR(I),I=1,7)
         NPAR = MR(1) + MR(3) + MR(4) + MR(6)
         IF (NPAR.GT.0 .AND. NPAR.LE.NPARMX) THEN
            DO 20 I = 1, NPAR
               PAR(I) = 0.0D0
   20       CONTINUE
            KFC = 1
            C = 0.0D0
            NPPC = NPAR + KFC
*           * Set KPIV to 1 to obtain monitoring information *
            KPIV = 0
            NIT = 50
            IFAIL = 1
*
            CALL G13AFF(MR,PAR,NPAR,C,KFC,X,NX,S,NDF,SD,NPPC,CM,ICM,ST,
     +                  NST,KPIV,NIT,ITC,ISF,RES,IRES,NRES,IFAIL)
*
            IF (IFAIL.NE.0) WRITE (NOUT,99999) 'G13AFF fails. IFAIL =',
     +          IFAIL
*
            IF (IFAIL.EQ.0 .OR. IFAIL.EQ.9) THEN
               CALL X04ABF(1,NOUT)
               M = 10
               ISHOW = 1
               IFAIL = -1
*
               CALL G13ASF(NRES,RES,MR,M,PAR,NPAR,ISHOW,R,RCM,IRCM,CHI,
     +                     IDF,SIGLEV,IWORK,LIWORK,WORK,LWORK,IFAIL)
*
               IF (IFAIL.NE.0) WRITE (NOUT,99999)
     +             'G13ASF fails. IFAIL =', IFAIL
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I2)
      END
