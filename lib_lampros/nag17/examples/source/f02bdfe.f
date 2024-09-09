*     F02BDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MMAX, IAI, IAR, IBI, IBR, IVI, IVR
      PARAMETER        (NMAX=4,MMAX=4,IAI=NMAX,IAR=NMAX,IBI=NMAX,
     +                 IBR=NMAX,IVI=NMAX,IVR=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RLB, RUB
      INTEGER          I, IFAIL, J, M, MM, N
*     .. Local Arrays ..
      DOUBLE PRECISION AI(IAI,NMAX), AR(IAR,NMAX), BI(IBI,NMAX),
     +                 BR(IBR,NMAX), RI(NMAX), RR(NMAX), U(NMAX),
     +                 V(NMAX), VI(IVI,MMAX), VR(IVR,MMAX)
      INTEGER          INTGER(NMAX)
      LOGICAL          C(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02BDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02BDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX .OR. M.LT.1 .OR. M.GT.MMAX) THEN
         WRITE (NOUT,99999) 'N or M out of range: N = ', N, '  M = ', M
         STOP
      END IF
      READ (NIN,*) RLB, RUB, ((AR(I,J),AI(I,J),J=1,N),I=1,N)
      IFAIL = 1
*
      CALL F02BDF(AR,IAR,AI,IAI,N,RLB,RUB,M,MM,RR,RI,VR,IVR,VI,IVI,
     +            INTGER,C,BR,IBR,BI,IBI,U,V,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Error in F02BDF. IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99998) (' (',RR(I),',',RI(I),')',I=1,MM)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvectors'
         DO 20 I = 1, N
            WRITE (NOUT,99998) (' (',VR(I,J),',',VI(I,J),')',J=1,MM)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,4(A,F7.3,A,F7.3,A))
      END
