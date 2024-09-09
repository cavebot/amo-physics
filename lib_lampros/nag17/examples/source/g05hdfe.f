*     G05HDF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          KMAX, IK, IPMAX, IQMAX, LPARMX, NMAX, LREF,
     +                 LIWORK
      PARAMETER        (KMAX=3,IK=KMAX,IPMAX=2,IQMAX=2,
     +                 LPARMX=(IPMAX+IQMAX)*KMAX*KMAX+KMAX,NMAX=100,
     +                 LREF=554,LIWORK=10)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, IP, IQ, J, K, N, NPAR
      CHARACTER        MEAN
*     .. Local Arrays ..
      DOUBLE PRECISION PAR(LPARMX), QQ(IK,KMAX), REF(LREF), W(IK,NMAX)
      INTEGER          IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05HDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05HDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) K, IP, IQ, N, MEAN
*
      IF (K.GT.0 .AND. K.LE.KMAX .AND. IP.GE.0 .AND. IP.LE.IPMAX .AND.
     +    IQ.GE.0 .AND. IQ.LE.IQMAX) THEN
         NPAR = (IP+IQ)*K*K
         IF (MEAN.EQ.'M' .OR. MEAN.EQ.'m') NPAR = NPAR + K
         IF (N.GT.0 .AND. N.LE.NMAX) THEN
            READ (NIN,*) (PAR(I),I=1,NPAR)
            DO 20 I = 1, K
               READ (NIN,*) (QQ(I,J),J=1,I)
   20       CONTINUE
*
            CALL G05CBF(0)
*
            IFAIL = 0
*
            CALL G05HDF('Start',K,IP,IQ,MEAN,PAR,NPAR,QQ,IK,N,W,REF,
     +                  LREF,IWORK,LIWORK,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,*) ' Realisation Number 1'
*
            DO 40 I = 1, K
               WRITE (NOUT,99999) '  Series number ', I
               WRITE (NOUT,*) '  -------------'
               WRITE (NOUT,*)
               WRITE (NOUT,99998) (W(I,J),J=1,N)
   40       CONTINUE
*
            IFAIL = 0
*
            CALL G05HDF('Restart',K,IP,IQ,MEAN,PAR,NPAR,QQ,IK,N,W,REF,
     +                  LREF,IWORK,LIWORK,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,*)
            WRITE (NOUT,*) ' Realisation Number 2'
*
            DO 60 I = 1, K
               WRITE (NOUT,99999) '  Series number ', I
               WRITE (NOUT,*) '  -------------'
               WRITE (NOUT,*)
               WRITE (NOUT,99998) (W(I,J),J=1,N)
   60       CONTINUE
*
         END IF
      END IF
      STOP
*
99999 FORMAT (/1X,A,I3)
99998 FORMAT (8(2X,F8.3))
      END
