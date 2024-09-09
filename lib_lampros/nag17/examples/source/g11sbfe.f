*     G11SBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NRX, IPMAX
      PARAMETER        (NRX=100,IPMAX=5)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, IP, IS, J, N
*     .. Local Arrays ..
      INTEGER          IRL(NRX)
      LOGICAL          X(NRX,IPMAX)
*     .. External Subroutines ..
      EXTERNAL         G11SBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G11SBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, IP
      IF (N.GT.0 .AND. N.LE.NRX .AND. IP.GT.0 .AND. IP.LE.IPMAX) THEN
         DO 20 I = 1, N
            READ (NIN,*) (X(I,J),J=1,IP)
   20    CONTINUE
         IFAIL = 0
*
         CALL G11SBF(IP,N,IS,X,NRX,IRL,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Frequency     Score pattern'
         WRITE (NOUT,*)
         DO 40 I = 1, IS
            WRITE (NOUT,99999) IRL(I), (X(I,J),J=1,IP)
   40    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,I5,12X,5L2)
      END
