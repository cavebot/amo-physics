*     F01BTF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA
      PARAMETER        (NMAX=8,IA=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DP
      INTEGER          I, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), P(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01BTF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01BTF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
         IFAIL = 0
*
         CALL F01BTF(N,A,IA,P,DP,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Decomposition as stored in array A'
         DO 20 I = 1, N
            WRITE (NOUT,99999) (A(I,J),J=1,N)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Pivotal rows'
         WRITE (NOUT,99998) (P(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
99998 FORMAT (1X,4F5.0)
      END
