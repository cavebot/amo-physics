*     F02BEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA, MMAX, IV
      PARAMETER        (NMAX=8,IA=NMAX,MMAX=8,IV=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS, EPS1, RLB, RUB
      INTEGER          I, IFAIL, J, M, MM, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), B(NMAX), BETA(NMAX), CC(NMAX),
     +                 ROOT(MMAX), V(IV,MMAX), X(NMAX,7)
      INTEGER          ICOUNT(MMAX)
      LOGICAL          C(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01AGF, F01AHF, F02BEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02BEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX .OR. M.LT.1 .OR. M.GT.MMAX) THEN
         WRITE (NOUT,99999) 'N or M out of range: N = ', N, '  M = ', M
         STOP
      END IF
      READ (NIN,*) RLB, RUB, ((A(I,J),J=1,N),I=1,N)
*
*     Householder reduction to tridiagonal form
      CALL F01AGF(N,0.0D0,A,IA,CC,B,BETA)
*
      EPS = X02AJF()
      EPS1 = 0.0D0
      IFAIL = 1
*
*     Selected eigenvalues and eigenvectors of tridiagonal matrix
      CALL F02BEF(N,CC,RLB,RUB,EPS,EPS1,B,BETA,M,MM,ROOT,V,IV,ICOUNT,X,
     +            C,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Error in F02BEF. IFAIL =', IFAIL
      ELSE
*
*        Selected eigenvectors of original matrix
         CALL F01AHF(N,1,MM,A,IA,B,V,IV)
*
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99998) (ROOT(I),I=1,MM)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvectors'
         DO 20 I = 1, N
            WRITE (NOUT,99998) (V(I,J),J=1,MM)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'No. of iterations (machine dependent)'
         WRITE (NOUT,99997) (ICOUNT(I),I=1,MM)
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,8F9.4)
99997 FORMAT (1X,5I8)
      END
