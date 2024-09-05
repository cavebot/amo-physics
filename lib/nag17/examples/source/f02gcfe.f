*     F02GCF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX, LDA, LDV, LWORK
      PARAMETER        (NMAX=8,MMAX=3,LDA=NMAX,LDV=NMAX,LWORK=64*NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION WL, WU
      INTEGER          I, IFAIL, J, M, N
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), V(LDV,NMAX), W(NMAX), WORK(LWORK)
      DOUBLE PRECISION RWORK(2*NMAX)
      INTEGER          IWORK(NMAX)
      LOGICAL          BWORK(NMAX)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         F02GCF, X04DBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02GCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, WL, WU
      IF (N.LE.NMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
*
*        Compute selected eigenvalues and eigenvectors of A
*
         IFAIL = 0
*
         CALL F02GCF('Moduli',N,A,LDA,WL,WU,MMAX,M,W,V,LDV,WORK,LWORK,
     +               RWORK,IWORK,BWORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99999) (W(I),I=1,M)
         WRITE (NOUT,*)
*
         CALL X04DBF('General',' ',N,M,V,LDV,'Bracketed','F7.4',
     +               'Eigenvectors','Integer',RLABS,'Integer',CLABS,80,
     +               0,IFAIL)
*
      END IF
      STOP
*
99999 FORMAT ((3X,4(' (',F7.4,',',F7.4,')',:)))
      END
