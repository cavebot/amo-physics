*     F01AFF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA, IB, M2MAX, IZ
      PARAMETER        (NMAX=8,IA=NMAX,IB=NMAX,M2MAX=NMAX,IZ=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M1, M2, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), B(IB,NMAX), DL(NMAX), E(NMAX),
     +                 R(NMAX), Z(IZ,M2MAX)
*     .. External Subroutines ..
      EXTERNAL         F01AEF, F01AFF, F02ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01AFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),(B(I,J),J=1,N),I=1,N)
         IFAIL = 0
*
*        Reduce A*X = LAMBDA*B*X to P*Z = LAMBDA*Z
         CALL F01AEF(N,A,IA,B,IB,DL,IFAIL)
*
*        Eigenvalues and eigenvectors of P*Z = LAMBDA*Z
         CALL F02ABF(A,IA,N,R,Z,IZ,E,IFAIL)
*
         M1 = 1
         M2 = N
*
*        Eigenvectors of A*X = LAMBDA*B*X from those of P*Z = LAMBDA*Z
         CALL F01AFF(N,M1,M2,B,IB,DL,Z,IZ)
*
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99999) (R(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvectors'
         DO 20 I = 1, N
            WRITE (NOUT,99999) (Z(I,J),J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
      END
