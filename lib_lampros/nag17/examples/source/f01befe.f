*     F01BEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA, IB, IV
      PARAMETER        (NMAX=8,IA=NMAX,IB=NMAX,IV=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M1, M2, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), B(IB,NMAX), DL(NMAX), E(NMAX),
     +                 R(NMAX), V(IV,NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01BDF, F01BEF, F02ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01BEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),(B(I,J),J=1,N),I=1,N)
         IFAIL = 0
*
*        Reduce A*B*X = LAMBDA*X to Q*Y = LAMBDA*Y
         CALL F01BDF(N,A,IA,B,IB,DL,IFAIL)
*
*        Eigenvalues and eigenvectors of real symmetric matrix
         CALL F02ABF(A,IA,N,R,V,IV,E,IFAIL)
*
         M1 = 1
         M2 = N
*
*        Eigenvectors of A*B*X = LAMBDA*X from those of Q*Y = LAMBDA*Y
         CALL F01BEF(N,M1,M2,B,IB,DL,V,IV)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99999) (R(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvectors'
         DO 20 I = 1, N
            WRITE (NOUT,99999) (V(I,J),J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,8F9.3)
      END
