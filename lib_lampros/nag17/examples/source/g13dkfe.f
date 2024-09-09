*     G13DKF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          KMAX, IK, IPMAX, IQMAX, NMAX, NPARMX, ICM, LWORK,
     +                 LIWORK, IDMAXL, LLMAX, LREF
      PARAMETER        (KMAX=3,IK=KMAX,IPMAX=3,IQMAX=3,NMAX=100,
     +                 NPARMX=(IPMAX+IQMAX)*KMAX*KMAX+KMAX,ICM=NPARMX,
     +                 LWORK=2000,LIWORK=100,IDMAXL=2,LLMAX=10,
     +                 LREF=(LLMAX-1)*KMAX*KMAX+2*KMAX*LLMAX+KMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION CGETOL, RLOGL
      INTEGER          I, IDMAX, IDMIN, IFAIL, IP, IPRINT, IQ, ISHOW, J,
     +                 K, LMAX, LPAR, M, MAXCAL, MLAST, N, ND, NITER
      LOGICAL          EXACT, MEANL
      CHARACTER        MEAN
*     .. Local Arrays ..
      DOUBLE PRECISION CM(ICM,NPARMX), DELTA(IK,IDMAXL), G(NPARMX),
     +                 PAR(NPARMX), PREDZ(IK,LLMAX), QQ(IK,KMAX),
     +                 REF(LREF), SEFZ(IK,LLMAX), V(IK,NMAX),
     +                 W(IK,NMAX), WORK(LWORK), Z(IK,NMAX)
      INTEGER          ID(KMAX), IWORK(LIWORK)
      LOGICAL          PARHLD(NPARMX)
      CHARACTER        TR(KMAX)
*     .. External Subroutines ..
      EXTERNAL         FPRINT, G13DCF, G13DJF, G13DKF, G13DLF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13DKF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) K, N, IP, IQ, MEAN, LMAX
      MEANL = .FALSE.
      LPAR = (IP+IQ)*K*K
      IF (MEAN.EQ.'M' .OR. MEAN.EQ.'m') THEN
         LPAR = LPAR + K
         MEANL = .TRUE.
      END IF
      IF (K.GT.0 .AND. K.LE.KMAX .AND. N.GE.1 .AND. N.LE.NMAX .AND.
     +    LPAR.GE.1 .AND. LPAR.LE.NPARMX .AND. LMAX.GE.1 .AND. LMAX.LE.
     +    LLMAX) THEN
         READ (NIN,*) (ID(I),I=1,K)
         IDMAX = 0
         IDMIN = 0
         DO 20 I = 1, K
            IDMIN = MIN(ID(I),IDMIN)
            IDMAX = MAX(ID(I),IDMAX)
   20    CONTINUE
         IF (IDMIN.GE.0 .AND. IDMAX.LE.IDMAXL) THEN
            DO 40 I = 1, K
               READ (NIN,*) (Z(I,J),J=1,N)
   40       CONTINUE
            READ (NIN,*) (TR(I),I=1,K)
            IF (IDMAX.GT.0) THEN
               DO 60 I = 1, K
                  READ (NIN,*) (DELTA(I,J),J=1,ID(I))
   60          CONTINUE
            END IF
            IFAIL = 0
*
            CALL G13DLF(K,N,Z,IK,TR,ID,DELTA,W,ND,WORK,IFAIL)
*
            DO 80 I = 1, LPAR
               PAR(I) = 0.0D0
               PARHLD(I) = .FALSE.
   80       CONTINUE
            DO 120 J = 1, K
               DO 100 I = J, K
                  QQ(I,J) = 0.0D0
  100          CONTINUE
  120       CONTINUE
            PARHLD(3) = .TRUE.
            EXACT = .TRUE.
*           ** Set IPRINT.gt.0 for no intermediate monitoring
            IPRINT = -1
            CGETOL = 0.0001D0
            MAXCAL = 40*LPAR*(LPAR+5)
*           ** Set ISHOW.eq.0 for no results from G13DCF
            ISHOW = 0
            IFAIL = 1
*
            CALL G13DCF(K,ND,IP,IQ,MEANL,PAR,LPAR,QQ,IK,W,PARHLD,EXACT,
     +                  IPRINT,CGETOL,MAXCAL,ISHOW,NITER,RLOGL,V,G,CM,
     +                  ICM,WORK,LWORK,IWORK,LIWORK,IFAIL)
*
            IF (IFAIL.EQ.0 .OR. IFAIL.GE.4) THEN
               IFAIL = 0
*
               CALL G13DJF(K,N,Z,IK,TR,ID,DELTA,IP,IQ,MEAN,PAR,LPAR,QQ,
     +                     V,LMAX,PREDZ,SEFZ,REF,LREF,WORK,LWORK,IWORK,
     +                     LIWORK,IFAIL)
*
               CALL FPRINT(K,N,LMAX,PREDZ,SEFZ,IK,NOUT)
               M = 1
               MLAST = 0
               Z(1,1) = 8.1D0
               Z(2,1) = 10.2D0
               IFAIL = 0
*
               CALL G13DKF(K,LMAX,M,MLAST,Z,IK,REF,LREF,V,PREDZ,SEFZ,
     +                     WORK,IFAIL)
*
               CALL FPRINT(K,N+MLAST,LMAX,PREDZ,SEFZ,IK,NOUT)
               M = 1
*              Leave MLAST unchanged from last call
               Z(1,1) = 8.5D0
               Z(2,1) = 10.0D0
               IFAIL = 0
*
               CALL G13DKF(K,LMAX,M,MLAST,Z,IK,REF,LREF,V,PREDZ,SEFZ,
     +                     WORK,IFAIL)
*
               CALL FPRINT(K,N+MLAST,LMAX,PREDZ,SEFZ,IK,NOUT)
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I2,A)
      END
*
      SUBROUTINE FPRINT(K,NM,LMAX,PREDZ,SEFZ,IK,NOUT)
*     .. Scalar Arguments ..
      INTEGER           IK, K, LMAX, NM, NOUT
*     .. Array Arguments ..
      DOUBLE PRECISION  PREDZ(IK,LMAX), SEFZ(IK,LMAX)
*     .. Local Scalars ..
      INTEGER           I, I2, J, L, L2, LOOP
*     .. Intrinsic Functions ..
      INTRINSIC         MIN, MOD
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' FORECAST SUMMARY TABLE'
      WRITE (NOUT,*) ' ----------------------'
      WRITE (NOUT,*)
      WRITE (NOUT,99999) ' Forecast origin is set at t = ', NM
      WRITE (NOUT,*)
      LOOP = LMAX/5
      IF (MOD(LMAX,5).NE.0) LOOP = LOOP + 1
      DO 40 J = 1, LOOP
         I2 = (J-1)*5
         L2 = MIN(I2+5,LMAX)
         WRITE (NOUT,99998) 'Lead Time ', (I,I=I2+1,L2)
         WRITE (NOUT,*)
         I = 1
         WRITE (NOUT,99997) 'Series ', I, ' : Forecast    ',
     +     (PREDZ(1,L),L=I2+1,L2)
         WRITE (NOUT,99996) ' : Standard Error ',
     +     (SEFZ(1,L),L=I2+1,L2)
         DO 20 I = 2, K
            WRITE (NOUT,99997) 'Series ', I, ' : Forecast    ',
     +        (PREDZ(I,L),L=I2+1,L2)
            WRITE (NOUT,99996) ' : Standard Error ',
     +        (SEFZ(I,L),L=I2+1,L2)
   20    CONTINUE
         WRITE (NOUT,*)
   40 CONTINUE
      RETURN
*
99999 FORMAT (1X,A,I4)
99998 FORMAT (1X,A,12X,5I10)
99997 FORMAT (1X,A,I2,A,5F10.2)
99996 FORMAT (10X,A,4(F7.2,3X),F7.2)
      END
