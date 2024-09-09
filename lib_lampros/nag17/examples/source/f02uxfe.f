*     F02UXF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA
      PARAMETER        (NMAX=10,LDA=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N, NCOLY
      LOGICAL          WANTQ
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), CWORK(NMAX), DUMMY(1)
      DOUBLE PRECISION D(NMAX), E(NMAX), RWORK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02UWF, F02UXF
*     .. Intrinsic Functions ..
      INTRINSIC        DCONJG
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02UXF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GT.NMAX) THEN
         WRITE (NOUT,*) 'N is out of range.'
         WRITE (NOUT,99999) 'N = ', N
      ELSE
         READ (NIN,*) ((A(I,J),J=I,N),I=1,N)
         NCOLY = 0
         WANTQ = .FALSE.
         IFAIL = 0
*
*        Reduce A to bidiagonal form
         CALL F02UWF(N,A,LDA,D,E,NCOLY,DUMMY,1,WANTQ,DUMMY,1,CWORK,
     +               IFAIL)
*
         NCOLY = 0
         IFAIL = 0
*
*        Form the n by n unitary matrix conjg(P')
         CALL F02UXF(N,A,LDA,NCOLY,DUMMY,1,RWORK,CWORK,IFAIL)
*
         WRITE (NOUT,*) 'Matrix  P'
         DO 20 I = 1, N
            WRITE (NOUT,99998) (DCONJG(A(J,I)),J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (3(' (',F7.4,',',F8.4,')',:))
      END
