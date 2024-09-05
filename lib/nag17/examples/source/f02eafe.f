*     F02EAF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA, LDZ, LWORK
      PARAMETER        (NMAX=8,LDA=NMAX,LDZ=NMAX,LWORK=64*NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), WI(NMAX), WORK(LWORK), WR(NMAX),
     +                 Z(LDZ,NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02EAF, X04CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02EAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
*
*        Compute Schur factorization of A
*
         IFAIL = 0
*
         CALL F02EAF('Vectors',N,A,LDA,WR,WI,Z,LDZ,WORK,LWORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99999) (' (',WR(I),',',WI(I),')',I=1,N)
         WRITE (NOUT,*)
*
         CALL X04CAF('General',' ',N,N,A,LDA,'Schur form',IFAIL)
*
         WRITE (NOUT,*)
*
         CALL X04CAF('General',' ',N,N,Z,LDZ,'Schur vectors',IFAIL)
*
      END IF
      STOP
*
99999 FORMAT (1X,A,F8.4,A,F8.4,A)
      END
