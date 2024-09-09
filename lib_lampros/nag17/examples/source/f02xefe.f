*     F02XEF Example Program Text
*     Mark 17 Revised.  NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. External Subroutines ..
      EXTERNAL         EX1, EX2
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02XEF Example Program Results'
      CALL EX1
      CALL EX2
      STOP
      END
*
      SUBROUTINE EX1
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, NCOLB
      PARAMETER        (MMAX=5,NMAX=3,NCOLB=1)
      INTEGER          LDA, LDB, LDPH
      PARAMETER        (LDA=MMAX,LDB=MMAX,LDPH=NMAX)
      INTEGER          LRWORK
      PARAMETER        (LRWORK=5*(NMAX-1))
      INTEGER          LCWORK
      PARAMETER        (LCWORK=NMAX**2+NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N
      LOGICAL          WANTP, WANTQ
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), B(LDB), CWORK(LCWORK), DUMMY(1),
     +                 PH(LDPH,NMAX)
      DOUBLE PRECISION RWORK(LRWORK), SV(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02XEF
*     .. Intrinsic Functions ..
      INTRINSIC        DCONJG
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Example 1'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*)
      READ (NIN,*)
      READ (NIN,*) M, N
      WRITE (NOUT,*)
      IF ((M.GT.MMAX) .OR. (N.GT.NMAX)) THEN
         WRITE (NOUT,*) 'M or N is out of range.'
         WRITE (NOUT,99999) 'M = ', M, '   N = ', N
      ELSE
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
         READ (NIN,*) (B(I),I=1,M)
*        Find the SVD of A.
         WANTQ = .TRUE.
         WANTP = .TRUE.
         IFAIL = 0
*
         CALL F02XEF(M,N,A,LDA,NCOLB,B,LDB,WANTQ,DUMMY,1,SV,WANTP,PH,
     +               LDPH,RWORK,CWORK,IFAIL)
*
         WRITE (NOUT,*) 'Singular value decomposition of A'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Singular values'
         WRITE (NOUT,99998) (SV(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Left-hand singular vectors, by column'
         DO 20 I = 1, M
            WRITE (NOUT,99997) (A(I,J),J=1,N)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Right-hand singular vectors, by column'
         DO 40 I = 1, N
            WRITE (NOUT,99997) (DCONJG(PH(J,I)),J=1,N)
   40    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Vector conjg( Q'' )*B'
         WRITE (NOUT,99997) (B(I),I=1,M)
      END IF
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,5F9.4)
99997 FORMAT ((1X,3('(',F7.4,',',F8.4,') ',:)))
      END
*
      SUBROUTINE EX2
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=3,NMAX=5)
      INTEGER          LDA, LDQ
      PARAMETER        (LDA=MMAX,LDQ=MMAX)
      INTEGER          LRWORK
      PARAMETER        (LRWORK=5*(MMAX-1))
      INTEGER          LCWORK
      PARAMETER        (LCWORK=MMAX**2+2*MMAX-1)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N, NCOLB
      LOGICAL          WANTP, WANTQ
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), CWORK(LCWORK), DUMMY(1), Q(LDQ,MMAX)
      DOUBLE PRECISION RWORK(LRWORK), SV(MMAX)
*     .. External Subroutines ..
      EXTERNAL         F02XEF
*     .. Intrinsic Functions ..
      INTRINSIC        DCONJG
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Example 2'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*)
      READ (NIN,*) M, N
      WRITE (NOUT,*)
      IF ((M.GT.MMAX) .OR. (N.GT.NMAX)) THEN
         WRITE (NOUT,*) 'M or N is out of range.'
         WRITE (NOUT,99999) 'M = ', M, '   N = ', N
      ELSE
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
*        Find the SVD of A.
         WANTQ = .TRUE.
         WANTP = .TRUE.
         NCOLB = 0
         IFAIL = 0
*
         CALL F02XEF(M,N,A,LDA,NCOLB,DUMMY,1,WANTQ,Q,LDQ,SV,WANTP,DUMMY,
     +               1,RWORK,CWORK,IFAIL)
*
         WRITE (NOUT,*) 'Singular value decomposition of A'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Singular values'
         WRITE (NOUT,99998) (SV(I),I=1,M)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Left-hand singular vectors, by column'
         DO 20 I = 1, M
            WRITE (NOUT,99997) (Q(I,J),J=1,M)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Right-hand singular vectors, by column'
         DO 40 I = 1, N
            WRITE (NOUT,99997) (DCONJG(A(J,I)),J=1,M)
   40    CONTINUE
      END IF
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,5F9.4)
99997 FORMAT (1X,3('(',F7.4,',',F8.4,') ',:))
      END
