*     G08RBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NSMAX, NX, NMXMAX, NSMMAX, IPMAX, NPVAR, LWORK
      PARAMETER        (NSMAX=5,NX=100,NMXMAX=NX,NSMMAX=NX,IPMAX=6,
     +                 NPVAR=IPMAX+1,LWORK=NMXMAX*(IPMAX+1))
*     .. Local Scalars ..
      DOUBLE PRECISION GAMMA, TOL
      INTEGER          I, IFAIL, IP, J, NMAX, NS, NSUM
*     .. Local Arrays ..
      DOUBLE PRECISION ETA(NMXMAX), PAREST(4*IPMAX+1),
     +                 PARVAR(NPVAR,IPMAX), VAPVEC(NMXMAX*(NMXMAX+1)/2),
     +                 WORK(LWORK), X(NX,IPMAX), Y(NSMMAX), ZIN(NMXMAX)
      INTEGER          ICEN(NSMMAX), IRANK(NMXMAX), IWA(4*NMXMAX),
     +                 NV(NSMAX)
*     .. External Subroutines ..
      EXTERNAL         G08RBF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08RBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Read number of samples, number of parameters to be fitted,
*     distribution power parameter and tolerance criterion for ties.
      READ (NIN,*) NS, IP, GAMMA, TOL
      WRITE (NOUT,*)
      IF (NS.GT.0 .AND. NS.LE.NSMAX .AND. IP.GT.0 .AND. IP.LE.IPMAX)
     +    THEN
         WRITE (NOUT,99999) 'Number of samples =', NS
         WRITE (NOUT,99999) 'Number of parameters fitted =', IP
         WRITE (NOUT,99998) 'Distribution power parameter =', GAMMA
         WRITE (NOUT,99998) 'Tolerance for ties =', TOL
*        Read the number of observations in each sample
         READ (NIN,*) (NV(I),I=1,NS)
         NMAX = 0
         NSUM = 0
         DO 20 I = 1, NS
            NSUM = NSUM + NV(I)
            NMAX = MAX(NMAX,NV(I))
   20    CONTINUE
         IF (NMAX.GT.0 .AND. NMAX.LE.NMXMAX .AND. NSUM.GT.0 .AND.
     +       NSUM.LE.NSMMAX) THEN
*           Read in observations, design matrix and censoring variable
            READ (NIN,*) (Y(I),(X(I,J),J=1,IP),ICEN(I),I=1,NSUM)
            IFAIL = 0
*
            CALL G08RBF(NS,NV,NSUM,Y,IP,X,NX,ICEN,GAMMA,NMAX,TOL,PARVAR,
     +                  NPVAR,IRANK,ZIN,ETA,VAPVEC,PAREST,WORK,LWORK,
     +                  IWA,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Score statistic'
            WRITE (NOUT,99997) (PAREST(I),I=1,IP)
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Covariance matrix of score statistic'
            DO 40 J = 1, IP
               WRITE (NOUT,99997) (PARVAR(I,J),I=1,J)
   40       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Parameter estimates'
            WRITE (NOUT,99997) (PAREST(IP+I),I=1,IP)
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Covariance matrix of parameter estimates'
            DO 60 I = 1, IP
               WRITE (NOUT,99997) (PARVAR(I+1,J),J=1,I)
   60       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,99996) 'Chi-squared statistic =',
     +        PAREST(2*IP+1), ' with', IP, ' d.f.'
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Standard errors of estimates and'
            WRITE (NOUT,*) 'approximate z-statistics'
            WRITE (NOUT,99995) (PAREST(2*IP+1+I),PAREST(3*IP+1+I),I=1,
     +        IP)
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,A,F10.5)
99997 FORMAT (1X,F9.3)
99996 FORMAT (1X,A,F9.3,A,I2,A)
99995 FORMAT (1X,F9.3,F14.3)
      END
