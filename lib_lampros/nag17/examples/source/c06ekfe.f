*     C06EKF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=64)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION XA(0:NMAX-1), XB(0:NMAX-1), YA(0:NMAX-1),
     +                 YB(0:NMAX-1)
*     .. External Subroutines ..
      EXTERNAL         C06EKF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C06EKF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=80) N
      IF (N.GT.1 .AND. N.LE.NMAX) THEN
         DO 40 J = 0, N - 1
            READ (NIN,*) XA(J), YA(J)
            XB(J) = XA(J)
            YB(J) = YA(J)
   40    CONTINUE
         IFAIL = 0
*
         CALL C06EKF(1,XA,YA,N,IFAIL)
         CALL C06EKF(2,XB,YB,N,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) '        Convolution  Correlation'
         WRITE (NOUT,*)
         DO 60 J = 0, N - 1
            WRITE (NOUT,99999) J, XA(J), XB(J)
   60    CONTINUE
         GO TO 20
      ELSE
         WRITE (NOUT,*) 'Invalid value of N'
      END IF
   80 STOP
*
99999 FORMAT (1X,I5,2F13.5)
      END
