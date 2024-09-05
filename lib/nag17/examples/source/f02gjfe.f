*     F02GJF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IAR, IAI, IBR, IBI, IVR, IVI
      PARAMETER        (NMAX=4,IAR=NMAX,IAI=NMAX,IBR=NMAX,IBI=NMAX,
     +                 IVR=NMAX,IVI=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS1
      INTEGER          I, IFAIL, J, N
      LOGICAL          MATV
*     .. Local Arrays ..
      DOUBLE PRECISION AI(IAI,NMAX), ALFI(NMAX), ALFR(NMAX),
     +                 AR(IAR,NMAX), BETA(NMAX), BI(IBI,NMAX),
     +                 BR(IBR,NMAX), VI(IVI,NMAX), VR(IVR,NMAX)
      INTEGER          ITER(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F02GJF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02GJF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((AR(I,J),AI(I,J),J=1,N),I=1,N)
         READ (NIN,*) ((BR(I,J),BI(I,J),J=1,N),I=1,N)
         EPS1 = X02AJF()
         MATV = .TRUE.
         IFAIL = 1
*
         CALL F02GJF(N,AR,IAR,AI,IAI,BR,IBR,BI,IBI,EPS1,ALFR,ALFI,BETA,
     +               MATV,VR,IVR,VI,IVI,ITER,IFAIL)
*
         WRITE (NOUT,*)
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'Error in F02GJF. IFAIL =', IFAIL
         ELSE
            DO 20 I = 1, N
               ALFR(I) = ALFR(I)/BETA(I)
               ALFI(I) = ALFI(I)/BETA(I)
   20       CONTINUE
            WRITE (NOUT,*) 'Eigenvalues'
            WRITE (NOUT,99998) (' (',ALFR(I),',',ALFI(I),')',I=1,N)
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Eigenvectors'
            DO 40 I = 1, N
               WRITE (NOUT,99998) (' (',VR(I,J),',',VI(I,J),')',J=1,N)
   40       CONTINUE
         END IF
      ELSE
         WRITE (NOUT,99999) 'N is out of range: N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,4(A,F7.3,A,F7.3,A))
      END
