*     F02WUF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NCOLB, LDA, LDB, LDQ, LWORK
      PARAMETER        (NMAX=10,NCOLB=1,LDA=NMAX,LDB=NMAX,LDQ=NMAX,
     +                 LWORK=5*(NMAX-1))
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N
      LOGICAL          WANTP, WANTQ
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB), Q(LDQ,NMAX), SV(NMAX),
     +                 WORK(LWORK)
*     .. External Subroutines ..
      EXTERNAL         F02WUF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02WUF Example Program Results'
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
         WANTQ = .TRUE.
         WANTP = .TRUE.
         IFAIL = 0
*
*        Find the SVD of A
         CALL F02WUF(N,A,LDA,NCOLB,B,LDB,WANTQ,Q,LDQ,SV,WANTP,WORK,
     +               IFAIL)
*
         WRITE (NOUT,*) 'Singular value decomposition of A'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Singular values'
         WRITE (NOUT,99998) (SV(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Left-hand singular vectors, by column'
         DO 20 I = 1, N
            WRITE (NOUT,99998) (Q(I,J),J=1,N)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Right-hand singular vectors, by column'
         DO 40 I = 1, N
            WRITE (NOUT,99998) (A(J,I),J=1,N)
   40    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Vector Q''*B'
         WRITE (NOUT,99998) (B(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (3(1X,F8.4))
      END
