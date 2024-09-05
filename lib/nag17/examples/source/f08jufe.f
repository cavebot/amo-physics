*     F08JUF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA, LWORK, LDZ
      PARAMETER        (NMAX=8,LDA=NMAX,LWORK=64*NMAX,LDZ=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, N
      CHARACTER        UPLO
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), TAU(NMAX), WORK(LWORK), Z(LDZ,NMAX)
      DOUBLE PRECISION D(NMAX), E(NMAX), RWORK(4*NMAX-4)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         F06TFF, X04DBF, ZHETRD, ZPTEQR, ZUNGTR
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F08JUF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) UPLO
         IF (UPLO.EQ.'U') THEN
            READ (NIN,*) ((A(I,J),J=I,N),I=1,N)
         ELSE IF (UPLO.EQ.'L') THEN
            READ (NIN,*) ((A(I,J),J=1,I),I=1,N)
         END IF
*
*        Reduce A to tridiagonal form T = (Q**H)*A*Q
*
         CALL ZHETRD(UPLO,N,A,LDA,D,E,TAU,WORK,LWORK,INFO)
*
*        Copy A into Z
*
         CALL F06TFF(UPLO,N,N,A,LDA,Z,LDZ)
*
*        Form Q explicitly, storing the result in Z
*
         CALL ZUNGTR(UPLO,N,Z,LDZ,TAU,WORK,LWORK,INFO)
*
*        Calculate all the eigenvalues and eigenvectors of A
*
         CALL ZPTEQR('V',N,D,E,Z,LDZ,RWORK,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.GT.0) THEN
            WRITE (NOUT,*) 'Failure to converge.'
         ELSE
*
*           Print eigenvalues and eigenvectors
*
            WRITE (NOUT,*) 'Eigenvalues'
            WRITE (NOUT,99999) (D(I),I=1,N)
            WRITE (NOUT,*)
            IFAIL = 0
*
            CALL X04DBF('General',' ',N,N,Z,LDZ,'Bracketed','F7.4',
     +                  'Eigenvectors','Integer',RLABS,'Integer',CLABS,
     +                  80,0,IFAIL)
*
         END IF
      END IF
      STOP
*
99999 FORMAT (8X,4(F7.4,11X,:))
      END
