*     F01AWF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IAI, IAR, IZR, IZI
      PARAMETER        (NMAX=4,IAI=NMAX,IAR=NMAX,IZR=NMAX,IZI=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS
      INTEGER          I, IB, IFAIL, J, K, L, N
*     .. Local Arrays ..
      DOUBLE PRECISION AI(IAI,NMAX), AR(IAR,NMAX), D(NMAX), WI(NMAX),
     +                 WR(NMAX), ZI(IZI,NMAX), ZR(IZR,NMAX)
      INTEGER          INTGER(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      INTEGER          X02BHF
      EXTERNAL         X02AJF, X02BHF
*     .. External Subroutines ..
      EXTERNAL         F01AMF, F01AVF, F01AWF, F02ARF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01AWF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((AR(I,J),AI(I,J),J=1,N),I=1,N)
         IB = X02BHF()
*
*        Balance complex matrix
         CALL F01AVF(N,IB,AR,IAR,AI,IAI,K,L,D)
*        Reduce to upper Hessenberg form
         CALL F01AMF(N,K,L,AR,IAR,AI,IAI,INTGER)
*
         EPS = X02AJF()
         IFAIL = 0
*
*        Calculate eigenvalues and eigenvectors
         CALL F02ARF(N,K,L,EPS,INTGER,AR,IAR,AI,IAI,WR,WI,ZR,IZR,ZI,IZI,
     +               IFAIL)
*
*        Eigenvectors of original matrix from those of balanced matrix
         CALL F01AWF(N,K,L,N,D,ZR,IZR,ZI,IZI)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99999) (' (',WR(I),',',WI(I),')',I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvectors'
         DO 20 I = 1, N
            WRITE (NOUT,99999) (' (',ZR(I,J),',',ZI(I,J),')',J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,4(A,F7.3,A,F7.3,A))
      END
