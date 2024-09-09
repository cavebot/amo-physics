*     F02AKF Example Program Text
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NMAX, IAR, IAI, IVR, IVI
      PARAMETER        (NMAX=4,IAR=NMAX,IAI=NMAX,IVR=NMAX,IVI=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION AI(IAI,NMAX), AR(IAR,NMAX), VI(IVI,NMAX),
     +                 VR(IVR,NMAX), WI(NMAX), WR(NMAX)
      INTEGER          INTGER(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02AKF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02AKF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LT.1 .OR. N.GT.NMAX) THEN
         WRITE (NOUT,99999) 'N is out of range: N = ', N
         STOP
      END IF
      READ (NIN,*) ((AR(I,J),AI(I,J),J=1,N),I=1,N)
      IFAIL = 1
*
      CALL F02AKF(AR,IAR,AI,IAI,N,WR,WI,VR,IVR,VI,IVI,INTGER,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Error in F02AKF. IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99998) (' (',WR(I),',',WI(I),')',I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvectors'
         DO 20 I = 1, N
            WRITE (NOUT,99998) (' (',VR(I,J),',',VI(I,J),')',J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,4(A,F7.3,A,F7.3,A))
      END
