*     F02EBF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA, LDV, LDVI, LDVR, LWORK
      PARAMETER        (NMAX=8,LDA=NMAX,LDV=NMAX,LDVI=NMAX,LDVR=NMAX,
     +                 LWORK=64*NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N
*     .. Local Arrays ..
      COMPLEX*16       V(LDV,NMAX)
      DOUBLE PRECISION A(LDA,NMAX), VI(LDVI,NMAX), VR(LDVR,NMAX),
     +                 WI(NMAX), WORK(LWORK), WR(NMAX)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         F02EBF, X04DBF
*     .. Intrinsic Functions ..
      INTRINSIC        DCMPLX
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02EBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
*
*        Compute eigenvalues and eigenvectors of A
*
         IFAIL = 0
*
         CALL F02EBF('Vectors',N,A,LDA,WR,WI,VR,LDVR,VI,LDVI,WORK,LWORK,
     +               IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99999) (' (',WR(I),',',WI(I),')',I=1,N)
         WRITE (NOUT,*)
         DO 40 J = 1, N
            DO 20 I = 1, N
               V(I,J) = DCMPLX(VR(I,J),VI(I,J))
   20       CONTINUE
   40    CONTINUE
*
         CALL X04DBF('General',' ',N,N,V,LDV,'Bracketed','F7.4',
     +               'Eigenvectors','Integer',RLABS,'Integer',CLABS,80,
     +               0,IFAIL)
*
      END IF
      STOP
*
99999 FORMAT ((3X,4(A,F7.4,A,F7.4,A,:)))
      END
