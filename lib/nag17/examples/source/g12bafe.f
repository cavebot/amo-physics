*     G12BAF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*
*     .. Parameters ..
      INTEGER          NMAX, NDMAX, MMAX, SMAX, NIN, NOUT
      PARAMETER        (NMAX=42,NDMAX=42,MMAX=2,SMAX=1,NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DEV, TOL
      INTEGER          I, IDF, IFAIL, IP, IPRINT, IRANK, J, LDZ, M,
     +                 MAXIT, N, ND, NS
*     .. Local Arrays ..
      DOUBLE PRECISION B(MMAX), COV(MMAX*(MMAX+1)/2), OMEGA(NMAX),
     +                 RES(NMAX), SC(MMAX), SE(MMAX), SUR(NDMAX,SMAX),
     +                 T(NMAX), TP(NDMAX), V(NMAX,MMAX+7),
     +                 WK(MMAX*(MMAX+9)/2+NMAX), Y(NMAX), Z(NMAX,MMAX)
      INTEGER          IC(NMAX), ISI(NMAX), ISZ(MMAX), IWK(2*NMAX)
*     .. External Subroutines ..
      EXTERNAL         G02GCF, G12BAF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE, LOG, MAX
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G12BAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M, NS, MAXIT, IPRINT
      IF (N.LE.NMAX .AND. M.LE.MMAX) THEN
         IF (NS.GT.0) THEN
            DO 20 I = 1, N
               READ (NIN,*) T(I), (Z(I,J),J=1,M), IC(I), ISI(I)
   20       CONTINUE
         ELSE
            DO 40 I = 1, N
               READ (NIN,*) T(I), (Z(I,J),J=1,M), IC(I)
   40       CONTINUE
         END IF
         READ (NIN,*) (ISZ(I),I=1,M), IP
         LDZ = NMAX
         TOL = 0.00005D0
         DO 60 I = 1, N
            Y(I) = 1.0D0 - DBLE(IC(I))
            V(I,7) = LOG(T(I))
   60    CONTINUE
         IFAIL = -1
         CALL G02GCF('L','M','Y','U',N,Z,LDZ,M,ISZ,IP+1,Y,RES,0.0D0,DEV,
     +               IDF,B,IRANK,SE,COV,V,NMAX,TOL,MAXIT,0,0.0D0,WK,
     +               IFAIL)
         DO 80 I = 1, IP
            B(I) = B(I+1)
   80    CONTINUE
         IF (IRANK.NE.IP+1) THEN
            WRITE (NOUT,*) ' WARNING: covariates not of full rank'
         END IF
         IFAIL = 0
*
         CALL G12BAF('No-offset',N,M,NS,Z,LDZ,ISZ,IP,T,IC,OMEGA,ISI,DEV,
     +               B,SE,SC,COV,RES,ND,TP,SUR,NDMAX,TOL,MAXIT,IPRINT,
     +               WK,IWK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Parameter      Estimate',
     +     '       Standard Error'
         WRITE (NOUT,*)
         DO 100 I = 1, IP
            WRITE (NOUT,99999) I, B(I), SE(I)
  100    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,99998) ' Deviance = ', DEV
         WRITE (NOUT,*)
         WRITE (NOUT,*) '    Time     Survivor Function'
         WRITE (NOUT,*)
         NS = MAX(NS,1)
         DO 120 I = 1, ND
            WRITE (NOUT,99997) TP(I), (SUR(I,J),J=1,NS)
  120    CONTINUE
      END IF
      STOP
*
99999 FORMAT (I6,2(10X,F8.4))
99998 FORMAT (A,D13.4)
99997 FORMAT (F10.0,3(5X,F8.4))
      END
