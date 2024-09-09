*     F01ADF Example Program Text
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NMAX, IA
      PARAMETER        (NMAX=8,IA=NMAX+1)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01ADF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01ADF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GE.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
         IFAIL = 0
*
         CALL F01ADF(N,A,IA,IFAIL)
*
         WRITE (NOUT,*) 'Lower triangle of inverse'
         DO 20 I = 1, N
            WRITE (NOUT,99998) (A(I+1,J),J=1,I)
   20    CONTINUE
      ELSE
         WRITE (NOUT,99999) 'N is out of range: N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,8F9.4)
      END
