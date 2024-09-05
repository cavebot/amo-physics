*     C02AGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D0)
      INTEGER          MAXDEG
      PARAMETER        (MAXDEG=100)
      LOGICAL          SCALE
      PARAMETER        (SCALE=.TRUE.)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, N, NROOT
*     .. Local Arrays ..
      DOUBLE PRECISION A(0:MAXDEG), W(2*MAXDEG+2), Z(2,MAXDEG)
*     .. External Subroutines ..
      EXTERNAL         C02AGF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C02AGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.MAXDEG) THEN
         READ (NIN,*) (A(I),I=0,N)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Degree of polynomial = ', N
         IFAIL = 0
*
         CALL C02AGF(A,N,SCALE,Z,W,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Roots of polynomial'
         WRITE (NOUT,*)
         NROOT = 1
   20    IF (NROOT.LE.N) THEN
            IF (Z(2,NROOT).EQ.ZERO) THEN
               WRITE (NOUT,99998) 'Z = ', Z(1,NROOT)
               NROOT = NROOT + 1
            ELSE
               WRITE (NOUT,99998) 'Z = ', Z(1,NROOT), ' +/- ',
     +           ABS(Z(2,NROOT)), '*i'
               NROOT = NROOT + 2
            END IF
            GO TO 20
         END IF
      ELSE
         WRITE (NOUT,*) 'N is out of range'
      END IF
      STOP
*
99999 FORMAT (1X,A,I4)
99998 FORMAT (1X,A,1P,D12.4,A,1P,D12.4,A)
      END
