*     F02ECF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX, LDA, LDV, LDVI, LDVR, LWORK
      PARAMETER        (NMAX=8,MMAX=3,LDA=NMAX,LDV=NMAX,LDVI=NMAX,
     +                 LDVR=NMAX,LWORK=64*NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION WL, WU
      INTEGER          I, IFAIL, J, M, N
*     .. Local Arrays ..
      COMPLEX*16       V(LDV,NMAX)
      DOUBLE PRECISION A(LDA,NMAX), VI(LDVI,MMAX), VR(LDVR,MMAX),
     +                 WI(NMAX), WORK(LWORK), WR(NMAX)
      INTEGER          IWORK(NMAX)
      LOGICAL          BWORK(NMAX)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         F02ECF, X04DBF
*     .. Intrinsic Functions ..
      INTRINSIC        DCMPLX
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02ECF Example Program Results'
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
         CALL F02ECF('Moduli',N,A,LDA,WL,WU,MMAX,M,WR,WI,VR,LDVR,VI,
     +               LDVI,WORK,LWORK,IWORK,BWORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99999) (' (',WR(I),',',WI(I),')',I=1,M)
         WRITE (NOUT,*)
         DO 40 J = 1, M
            DO 20 I = 1, N
               V(I,J) = DCMPLX(VR(I,J),VI(I,J))
   20       CONTINUE
   40    CONTINUE
*
         CALL X04DBF('General',' ',N,M,V,LDV,'Bracketed','F7.4',
     +               'Eigenvectors','Integer',RLABS,'Integer',CLABS,80,
     +               0,IFAIL)
*
      END IF
      STOP
*
99999 FORMAT ((3X,4(A,F7.4,A,F7.4,A,:)))
      END
