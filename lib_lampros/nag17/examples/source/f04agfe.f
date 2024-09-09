*     F04AGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IR, IA, IB, IX
      PARAMETER        (NMAX=8,IR=1,IA=NMAX,IB=NMAX,IX=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION D1
      INTEGER          I, ID, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), B(IB,IR), P(NMAX), X(IX,IR)
*     .. External Subroutines ..
      EXTERNAL         F03AEF, F04AGF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04AGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N), ((B(I,J),J=1,IR),I=1,N)
         IFAIL = 1
*
*        Cholesky decomposition
         CALL F03AEF(N,A,IA,P,D1,ID,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'Error in F03AEF. IFAIL =', IFAIL
         ELSE
*
*           Approximate solution of linear equations
            CALL F04AGF(N,IR,A,IA,P,B,IB,X,IX)
*
            WRITE (NOUT,*) ' Solution'
            DO 20 I = 1, N
               WRITE (NOUT,99998) (X(I,J),J=1,IR)
   20       CONTINUE
         END IF
      ELSE
         WRITE (NOUT,99999) 'N is out of range: N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,8F9.4)
      END
