*     E01BGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=21,NMAX=50)
*     .. Local Scalars ..
      DOUBLE PRECISION STEP
      INTEGER          I, IFAIL, M, N, R
*     .. Local Arrays ..
      DOUBLE PRECISION D(NMAX), F(NMAX), PD(MMAX), PF(MMAX), PX(MMAX),
     +                 X(NMAX)
*     .. External Subroutines ..
      EXTERNAL         E01BGF
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E01BGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         DO 20 R = 1, N
            READ (NIN,*) X(R), F(R), D(R)
   20    CONTINUE
         READ (NIN,*) M
         IF (M.GT.0 .AND. M.LE.MMAX) THEN
*           Compute M equally spaced points from X(1) to X(N).
            STEP = (X(N)-X(1))/(M-1)
            DO 40 I = 1, M
               PX(I) = MIN(X(1)+(I-1)*STEP,X(N))
   40       CONTINUE
            IFAIL = 0
*
            CALL E01BGF(N,X,F,D,M,PX,PF,PD,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        '                  Interpolated   Interpolated'
            WRITE (NOUT,*)
     +        '       Abscissa          Value     Derivative'
            DO 60 I = 1, M
               WRITE (NOUT,99999) PX(I), PF(I), PD(I)
   60       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,2F15.4,1P,D15.3)
      END
