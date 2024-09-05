*     F04AYF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IR, IA, IB
      PARAMETER        (NMAX=8,IR=1,IA=NMAX,IB=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DP
      INTEGER          I, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), B(IB,IR), P(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01BTF, F04AYF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04AYF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
         IFAIL = 1
*
*        Crout decomposition
         CALL F01BTF(N,A,IA,P,DP,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99998) 'Error in F01BTF. IFAIL =', IFAIL
         ELSE
            READ (NIN,*) ((B(I,J),J=1,IR),I=1,N)
*
*           Approximate solution of linear equations
            CALL F04AYF(N,IR,A,IA,P,B,IB,IFAIL)
*
            WRITE (NOUT,*) ' Solution'
            DO 20 I = 1, N
               WRITE (NOUT,99999) (B(I,J),J=1,IR)
   20       CONTINUE
         END IF
      ELSE
         WRITE (NOUT,99998) 'N is out of range: N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
99998 FORMAT (1X,A,I5)
      END
