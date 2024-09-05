*     F01AUF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA, IZ
      PARAMETER        (NMAX=8,IA=NMAX,IZ=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS
      INTEGER          I, IB, IFAIL, J, K, L, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), D(NMAX), WI(NMAX), WR(NMAX),
     +                 Z(IZ,NMAX)
      INTEGER          INTGER(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      INTEGER          X02BHF
      EXTERNAL         X02AJF, X02BHF
*     .. External Subroutines ..
      EXTERNAL         F01AKF, F01APF, F01ATF, F01AUF, F02AQF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01AUF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
         IB = X02BHF()
*
*        Balance matrix
         CALL F01ATF(N,IB,A,IA,K,L,D)
*        Reduce to Hessenberg form
         CALL F01AKF(N,K,L,A,IA,INTGER)
*        Accumulate transformations
         CALL F01APF(N,K,L,INTGER,A,IA,Z,IZ)
*
         IFAIL = 0
         EPS = X02AJF()
*
*        Calculate eigenvalues and eigenvectors
         CALL F02AQF(N,K,L,EPS,A,IA,Z,IZ,WR,WI,INTGER,IFAIL)
*
         M = N
*
*        Eigenvectors of original matrix from those of balanced matrix
         CALL F01AUF(N,K,L,M,D,Z,IZ)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99999) (' (',WR(I),',',WI(I),')',I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvector array'
         DO 20 I = 1, N
            WRITE (NOUT,99998) (Z(I,J),J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,F7.3,A,F7.3,A)
99998 FORMAT (1X,8F9.4)
      END
