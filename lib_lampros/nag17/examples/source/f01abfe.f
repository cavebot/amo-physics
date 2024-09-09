*     F01ABF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA, IB
      PARAMETER        (NMAX=8,IA=NMAX+1,IB=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), B(IB,NMAX), Z(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01ABF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
         IFAIL = 1
*
         CALL F01ABF(A,IA,N,B,IB,Z,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'Error in F01ABF. IFAIL =', IFAIL
         ELSE
            WRITE (NOUT,*) 'Lower triangle of inverse'
            DO 20 I = 1, N
               WRITE (NOUT,99998) (B(I,J),J=1,I)
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
