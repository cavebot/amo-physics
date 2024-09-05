*     E02DFF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MXMAX, MYMAX, PXMAX, PYMAX
      PARAMETER        (MXMAX=20,MYMAX=MXMAX,PXMAX=MXMAX,PYMAX=PXMAX)
      INTEGER          LIWRK, LWRK
      PARAMETER        (LIWRK=MXMAX+PXMAX-4,LWRK=4*MXMAX+PXMAX+8)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, MX, MY, PX, PY
*     .. Local Arrays ..
      DOUBLE PRECISION C((PXMAX-4)*(PYMAX-4)), FF(MXMAX*MYMAX),
     +                 LAMDA(PXMAX), MU(PYMAX), WRK(LWRK), X(MXMAX),
     +                 Y(MYMAX)
      INTEGER          IWRK(LIWRK)
      CHARACTER*10     CLABS(MYMAX), RLABS(MXMAX)
*     .. External Subroutines ..
      EXTERNAL         E02DFF, X04CBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02DFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
*     Read PX and PY, the number of knots in the X and Y directions.
      READ (NIN,*) PX, PY
      IF (PX.LE.PXMAX .AND. PY.LE.PYMAX) THEN
*        Read the knots LAMDA(1) .. LAMDA(PX) and MU(1) .. MU(PY).
         READ (NIN,*) (LAMDA(I),I=1,PX)
         READ (NIN,*) (MU(I),I=1,PY)
*        Read C, the bicubic spline coefficients.
         READ (NIN,*) (C(I),I=1,(PX-4)*(PY-4))
*        Read MX and MY, the number of grid points in the X and Y
*        directions respectively.
         READ (NIN,*) MX, MY
         IF (MX.LE.MXMAX .AND. MY.LE.MYMAX) THEN
*           Read the X and Y co-ordinates defining the evaluation grid.
            READ (NIN,*) (X(I),I=1,MX)
            READ (NIN,*) (Y(I),I=1,MY)
            IFAIL = 0
*
*           Evaluate the spline at the MX by MY points.
            CALL E02DFF(MX,MY,PX,PY,X,Y,LAMDA,MU,C,FF,WRK,LWRK,IWRK,
     +                  LIWRK,IFAIL)
*
*           Generate column and row labels to print the results with.
            DO 20 I = 1, MX
               WRITE (CLABS(I),99999) X(I)
   20       CONTINUE
            DO 40 I = 1, MY
               WRITE (RLABS(I),99999) Y(I)
   40       CONTINUE
*
*           Print the result array.
            CALL X04CBF('G','X',MY,MX,FF,MY,'F8.3',
     +                'Spline evaluated on X-Y grid (X across, Y down):'
     +                  ,'Character',RLABS,'Character',CLABS,80,0,IFAIL)
*
         END IF
      END IF
      STOP
*
99999 FORMAT (F5.1)
      END
