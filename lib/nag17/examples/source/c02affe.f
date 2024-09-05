*     C02AFF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MAXDEG
      PARAMETER        (MAXDEG=100)
      LOGICAL          SCALE
      PARAMETER        (SCALE=.TRUE.)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(2,0:MAXDEG), W(4*MAXDEG+4), Z(2,MAXDEG)
*     .. External Subroutines ..
      EXTERNAL         C02AFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C02AFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.MAXDEG) THEN
         READ (NIN,*) (A(1,I),A(2,I),I=0,N)
         IFAIL = 0
*
         CALL C02AFF(A,N,SCALE,Z,W,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Degree of polynomial = ', N
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Roots of polynomial'
         WRITE (NOUT,*)
         DO 20 I = 1, N
            WRITE (NOUT,99998) 'z = ', Z(1,I), Z(2,I), '*i'
   20    CONTINUE
      ELSE
         WRITE (NOUT,*) 'N is out of range'
      END IF
      STOP
*
99999 FORMAT (1X,A,I4)
99998 FORMAT (1X,A,1P,D12.4,SP,D14.4,A)
      END
