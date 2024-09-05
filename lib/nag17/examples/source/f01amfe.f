*     F01AMF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IAI, IAR
      PARAMETER        (NMAX=4,IAI=NMAX,IAR=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, J, K, L, N
*     .. Local Arrays ..
      DOUBLE PRECISION AI(IAI,NMAX), AR(IAR,NMAX)
      INTEGER          INTGER(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01AMF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01AMF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((AR(I,J),AI(I,J),J=1,N),I=1,N)
         K = 1
         L = N
*
         CALL F01AMF(N,K,L,AR,IAR,AI,IAI,INTGER)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Arrays AR and AI'
         DO 20 I = 1, N
            WRITE (NOUT,99999) (' (',AR(I,J),',',AI(I,J),')',J=1,N)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Interchanges'
         WRITE (NOUT,99998) (INTGER(I),I=K+1,L-1)
      END IF
      STOP
*
99999 FORMAT (1X,4(A,F7.3,A,F7.3,A))
99998 FORMAT (1X,4I4)
      END
