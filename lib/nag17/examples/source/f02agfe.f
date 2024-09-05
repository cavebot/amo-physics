*     F02AGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA, IVR, IVI
      PARAMETER        (NMAX=4,IA=NMAX,IVR=NMAX,IVI=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), RI(NMAX), RR(NMAX), VI(IVI,NMAX),
     +                 VR(IVR,NMAX)
      INTEGER          INTGER(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02AGF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02AGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX) THEN
         WRITE (NOUT,99999) 'N is out of range: N = ', N
         STOP
      END IF
      READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
      IFAIL = 1
*
      CALL F02AGF(A,IA,N,RR,RI,VR,IVR,VI,IVI,INTGER,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Error in F02AGF. IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99998) (' (',RR(I),',',RI(I),')',I=1,N)
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
