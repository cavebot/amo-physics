*     C06FKF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=64)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION WORK(NMAX), XA(NMAX), XB(NMAX), YA(NMAX),
     +                 YB(NMAX)
*     .. External Subroutines ..
      EXTERNAL         C06FKF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C06FKF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=80) N
      WRITE (NOUT,*)
      IF (N.GT.1 .AND. N.LE.NMAX) THEN
         DO 40 J = 1, N
            READ (NIN,*) XA(J), YA(J)
            XB(J) = XA(J)
            YB(J) = YA(J)
   40    CONTINUE
         IFAIL = 0
*
         CALL C06FKF(1,XA,YA,N,WORK,IFAIL)
         CALL C06FKF(2,XB,YB,N,WORK,IFAIL)
*
         WRITE (NOUT,*) '        Convolution  Correlation'
         WRITE (NOUT,*)
         DO 60 J = 1, N
            WRITE (NOUT,99999) J - 1, XA(J), XB(J)
   60    CONTINUE
         GO TO 20
      ELSE
         WRITE (NOUT,*) 'Invalid value of N'
      END IF
   80 STOP
*
99999 FORMAT (1X,I5,2F13.5)
      END
