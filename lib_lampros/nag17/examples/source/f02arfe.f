*     F02ARF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IHR, IHI, IVR, IVI
      PARAMETER        (NMAX=4,IHR=NMAX,IHI=NMAX,IVR=NMAX,IVI=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, L, N
*     .. Local Arrays ..
      DOUBLE PRECISION HI(IHI,NMAX), HR(IHR,NMAX), VI(IVI,NMAX),
     +                 VR(IVR,NMAX), WI(NMAX), WR(NMAX)
      INTEGER          INTGER(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01AMF, F02ARF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02ARF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX) THEN
         WRITE (NOUT,99999) 'N is out of range: N = ', N
         STOP
      END IF
      READ (NIN,*) ((HR(I,J),HI(I,J),J=1,N),I=1,N)
      K = 1
      L = N
*
*     Reduce to upper Hessenberg form
      CALL F01AMF(N,K,L,HR,IHR,HI,IHI,INTGER)
*
      IFAIL = 1
*     Eigenvalues and eigenvectors of original matrix
      CALL F02ARF(N,K,L,X02AJF(),INTGER,HR,IHR,HI,IHI,WR,WI,VR,IVR,VI,
     +            IVI,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Error in F02ARF. IFAIL =', IFAIL
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
