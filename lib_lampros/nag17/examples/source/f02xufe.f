*     F02XUF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NCOLB
      PARAMETER        (NMAX=10,NCOLB=1)
      INTEGER          LDA, LDB, LDQ
      PARAMETER        (LDA=NMAX,LDB=NMAX,LDQ=NMAX)
      INTEGER          LRWORK
      PARAMETER        (LRWORK=5*(NMAX-1))
      INTEGER          LCWORK
      PARAMETER        (LCWORK=NMAX-1)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N
      LOGICAL          WANTP, WANTQ
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), B(LDB), CWORK(LCWORK), Q(LDQ,NMAX)
      DOUBLE PRECISION RWORK(LRWORK), SV(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02XUF
*     .. Intrinsic Functions ..
      INTRINSIC        DCONJG
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02XUF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GT.NMAX) THEN
         WRITE (NOUT,*) 'N is out of range.'
         WRITE (NOUT,99999) 'N = ', N
      ELSE
         READ (NIN,*) ((A(I,J),J=I,N),I=1,N)
         READ (NIN,*) (B(I),I=1,N)
*        Find the SVD of A.
         WANTQ = .TRUE.
         WANTP = .TRUE.
         IFAIL = 0
*
         CALL F02XUF(N,A,LDA,NCOLB,B,LDB,WANTQ,Q,LDQ,SV,WANTP,RWORK,
     +               CWORK,IFAIL)
*
         WRITE (NOUT,*) 'Singular value decomposition of A'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Singular values'
         WRITE (NOUT,99998) (SV(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Left-hand singular vectors, by column'
         DO 20 I = 1, N
            WRITE (NOUT,99997) (Q(I,J),J=1,N)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Right-hand singular vectors, by column'
         DO 40 I = 1, N
            WRITE (NOUT,99997) (DCONJG(A(J,I)),J=1,N)
   40    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Vector conjg( Q'' )*B'
         WRITE (NOUT,99997) (B(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,3F9.4)
99997 FORMAT (1X,3('(',F7.4,',',F8.4,') ',:))
      END
