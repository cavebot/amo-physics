*     G13DJF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          KMAX, IK, IPMAX, IQMAX, NMAX, NPARMX, LWORK,
     +                 LIWORK, ICM, MMAX, IDMAXL, LREF
      PARAMETER        (KMAX=3,IK=KMAX,IPMAX=3,IQMAX=3,NMAX=100,
     +                 NPARMX=(IPMAX+IQMAX)*KMAX*KMAX+KMAX,LWORK=2000,
     +                 LIWORK=100,ICM=NPARMX,MMAX=10,IDMAXL=2,
     +                 LREF=MMAX*KMAX*(KMAX+2))
*     .. Local Scalars ..
      DOUBLE PRECISION CGETOL, RLOGL
      INTEGER          I, I2, IDMAX, IDMIN, IFAIL, IP, IPRINT, IQ,
     +                 ISHOW, J, K, L, L2, LMAX, LOOP, MAXCAL, N, ND,
     +                 NITER, NPAR
      LOGICAL          EXACT, MEANL
      CHARACTER        MEAN
*     .. Local Arrays ..
      DOUBLE PRECISION CM(ICM,NPARMX), DELTA(IK,IDMAXL), G(NPARMX),
     +                 PAR(NPARMX), PREDZ(IK,MMAX), QQ(IK,KMAX),
     +                 REF(LREF), SEFZ(IK,MMAX), V(IK,NMAX), W(IK,NMAX),
     +                 WORK(LWORK), Z(IK,NMAX)
      INTEGER          ID(KMAX), IWORK(LIWORK)
      LOGICAL          PARHLD(NPARMX)
      CHARACTER        TR(KMAX)
*     .. External Subroutines ..
      EXTERNAL         G13DCF, G13DJF, G13DLF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN, MOD
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13DJF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) K, N, IP, IQ, MEAN, LMAX
      NPAR = (IP+IQ)*K*K
      MEANL = .FALSE.
      IF (MEAN.EQ.'M' .OR. MEAN.EQ.'m') THEN
         NPAR = NPAR + K
         MEANL = .TRUE.
      END IF
      IF (K.GT.0 .AND. K.LE.KMAX .AND. N.GE.1 .AND. N.LE.NMAX .AND.
     +    NPAR.GE.1 .AND. NPAR.LE.NPARMX .AND. LMAX.GE.1 .AND. LMAX.LE.
     +    MMAX) THEN
         READ (NIN,*) (ID(I),I=1,K)
         IDMIN = 0
         IDMAX = 0
         DO 20 I = 1, K
            IDMIN = MIN(ID(I),IDMIN)
            IDMAX = MAX(ID(I),IDMAX)
   20    CONTINUE
*
         IF (IDMIN.GE.0 .AND. IDMAX.LE.IDMAXL) THEN
            DO 40 I = 1, K
               READ (NIN,*) (Z(I,J),J=1,N)
   40       CONTINUE
            READ (NIN,*) (TR(I),I=1,K)
*
            IF (IDMAX.GT.0) THEN
               DO 60 I = 1, K
                  READ (NIN,*) (DELTA(I,J),J=1,ID(I))
   60          CONTINUE
            END IF
            IFAIL = 0
*
            CALL G13DLF(K,N,Z,IK,TR,ID,DELTA,W,ND,WORK,IFAIL)
*
            DO 80 I = 1, NPAR
               PAR(I) = 0.0D0
               PARHLD(I) = .FALSE.
   80       CONTINUE
            DO 120 I = 1, K
               DO 100 J = 1, I
                  QQ(I,J) = 0.0D0
  100          CONTINUE
  120       CONTINUE
            PARHLD(3) = .TRUE.
            EXACT = .TRUE.
*           ** Set IPRINT .lt. 0 for no monitoring
            IPRINT = -1
            CGETOL = 0.0001D0
            MAXCAL = 40*NPAR*(NPAR+5)
*           ** Set ISHOW = 0 for no printing of results from G13DCF
            ISHOW = 0
            IFAIL = 1
*
            CALL G13DCF(K,ND,IP,IQ,MEANL,PAR,NPAR,QQ,IK,W,PARHLD,EXACT,
     +                  IPRINT,CGETOL,MAXCAL,ISHOW,NITER,RLOGL,V,G,CM,
     +                  ICM,WORK,LWORK,IWORK,LIWORK,IFAIL)
*
            IF (IFAIL.EQ.0 .OR. IFAIL.GE.4) THEN
               IFAIL = 0
*
               CALL G13DJF(K,N,Z,IK,TR,ID,DELTA,IP,IQ,MEAN,PAR,NPAR,QQ,
     +                     V,LMAX,PREDZ,SEFZ,REF,LREF,WORK,LWORK,IWORK,
     +                     LIWORK,IFAIL)
*
               WRITE (NOUT,*)
               WRITE (NOUT,*) ' FORECAST SUMMARY TABLE'
               WRITE (NOUT,*) ' ----------------------'
               WRITE (NOUT,*)
               WRITE (NOUT,99998) ' Forecast origin is set at t = ',
     +           N
               WRITE (NOUT,*)
               LOOP = LMAX/5
               IF (MOD(LMAX,5).NE.0) LOOP = LOOP + 1
               DO 160 J = 1, LOOP
                  I2 = (J-1)*5
                  L2 = MIN(I2+5,LMAX)
                  WRITE (NOUT,99997) 'Lead Time ', (I,I=I2+1,L2)
                  WRITE (NOUT,*)
                  I = 1
                  WRITE (NOUT,99996) 'Series ', I,
     +              ' : Forecast    ', (PREDZ(1,L),L=I2+1,L2)
                  WRITE (NOUT,99995) ' : Standard Error ',
     +              (SEFZ(1,L),L=I2+1,L2)
                  DO 140 I = 2, K
                     WRITE (NOUT,99996) 'Series ', I,
     +                 ' : Forecast    ', (PREDZ(I,L),L=I2+1,L2)
                     WRITE (NOUT,99995) ' : Standard Error ',
     +                 (SEFZ(I,L),L=I2+1,L2)
  140             CONTINUE
                  WRITE (NOUT,*)
  160          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I2,A)
99998 FORMAT (1X,A,I4)
99997 FORMAT (1X,A,12X,5I10)
99996 FORMAT (1X,A,I2,A,5F10.2)
99995 FORMAT (10X,A,4(F7.2,3X),F7.2)
      END
