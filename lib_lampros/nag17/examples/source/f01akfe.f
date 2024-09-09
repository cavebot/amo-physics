*     F01AKF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA
      PARAMETER        (NMAX=8,IA=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, J, K, L, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX)
      INTEGER          INTGER(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01AKF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01AKF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
         K = 1
         L = N
*
         CALL F01AKF(N,K,L,A,IA,INTGER)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Array A'
         DO 20 I = 1, N
            WRITE (NOUT,99999) (A(I,J),J=1,N)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Interchanges'
         WRITE (NOUT,99998) (INTGER(I),I=K+1,L)
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
99998 FORMAT (1X,8I4)
      END
