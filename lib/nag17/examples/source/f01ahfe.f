*     F01AHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA, MMAX, IZ
      PARAMETER        (NMAX=8,IA=NMAX,MMAX=5,IZ=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ALB, EPS, EPS1, UB
      INTEGER          I, IFAIL, J, M, M1, M2, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), D(NMAX), E(NMAX), E2(NMAX), R(MMAX),
     +                 X(NMAX,7), Z(IZ,MMAX)
      INTEGER          ICOUNT(MMAX)
      LOGICAL          C(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01AGF, F01AHF, F02BEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01AHF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M
      IF (N.GT.0 .AND. N.LE.NMAX .AND. M.GT.0 .AND. M.LE.MMAX) THEN
         READ (NIN,*) ALB, UB
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
*
*        Householder reduction to tridiagonal form
         CALL F01AGF(N,0.0D0,A,IA,D,E,E2)
*
         EPS = X02AJF()
         EPS1 = 0.0D0
         IFAIL = 0
*
*        Eigenvalues and eigenvectors of tridiagonal matrix
         CALL F02BEF(N,D,ALB,UB,EPS,EPS1,E,E2,M,M2,R,Z,IZ,ICOUNT,X,C,
     +               IFAIL)
*
         M1 = 1
*
*        Eigenvectors of symmetric matrix from those of tridiagonal
*        matrix
         CALL F01AHF(N,M1,M2,A,IA,E,Z,IZ)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99999) (R(I),I=M1,M2)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvectors'
         DO 20 I = 1, N
            WRITE (NOUT,99999) (Z(I,J),J=M1,M2)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
      END
