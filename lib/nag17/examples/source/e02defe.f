*     E02DEF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, PXMAX, PYMAX
      PARAMETER        (MMAX=20,PXMAX=MMAX,PYMAX=PXMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, M, PX, PY
*     .. Local Arrays ..
      DOUBLE PRECISION C((PXMAX-4)*(PYMAX-4)), FF(MMAX), LAMDA(PXMAX),
     +                 MU(PYMAX), WRK(PYMAX-7), X(MMAX), Y(MMAX)
      INTEGER          IWRK(PYMAX-7)
*     .. External Subroutines ..
      EXTERNAL         E02DEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02DEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Read PX and PY, the number of knots in the X and Y directions.
      READ (NIN,*) PX, PY
      IF (PX.LE.PXMAX .AND. PY.LE.PYMAX) THEN
*        Read the knots LAMDA(1) .. LAMDA(PX) and MU(1) .. MU(PY).
         READ (NIN,*) (LAMDA(I),I=1,PX)
         READ (NIN,*) (MU(I),I=1,PY)
*        Read C, the bicubic spline coefficients.
         READ (NIN,*) (C(I),I=1,(PX-4)*(PY-4))
*        Read M, the number of spline evaluation points.
         READ (NIN,*) M
         IF (M.LE.MMAX) THEN
*           Read the X and Y co-ordinates of the evaluation points.
            DO 20 I = 1, M
               READ (NIN,*) X(I), Y(I)
   20       CONTINUE
            IFAIL = 0
*
*           Evaluate the spline at the M points.
            CALL E02DEF(M,PX,PY,X,Y,LAMDA,MU,C,FF,WRK,IWRK,IFAIL)
*
*           Print the results.
            WRITE (NOUT,*)
            WRITE (NOUT,*) '      I       X(I)       Y(I)      FF(I)'
            WRITE (NOUT,99999) (I,X(I),Y(I),FF(I),I=1,M)
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,I7,3F11.3)
      END
