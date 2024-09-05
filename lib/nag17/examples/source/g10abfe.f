*     G10ABF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDC
      PARAMETER        (NMAX=43,LDC=NMAX-1)
*     .. Local Scalars ..
      DOUBLE PRECISION DF, RHO, RSS
      INTEGER          I, IFAIL, N, NORD
      CHARACTER        MODE, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION C(LDC,3), H(NMAX), RES(NMAX), WK(9*NMAX+14),
     +                 WT(NMAX), WWT(NMAX), X(NMAX), XORD(NMAX),
     +                 Y(NMAX), YHAT(NMAX), YORD(NMAX)
      INTEGER          IWRK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G10ABF, G10ZAF
*     .. Executable Statements ..
      WRITE (NOUT,*) ' G10ABF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) MODE, WEIGHT
         READ (NIN,*) RHO
         IF (WEIGHT.EQ.'U' .OR. WEIGHT.EQ.'u') THEN
            READ (NIN,*) (X(I),Y(I),I=1,N)
         ELSE
            READ (NIN,*) (X(I),Y(I),WT(I),I=1,N)
         END IF
         IFAIL = 0
*
*        Sort data into increasing X and
*        remove tied observations and weight accordingly
*
         IFAIL = 0
*
         CALL G10ZAF(WEIGHT,N,X,Y,WT,NORD,XORD,YORD,WWT,RSS,IWRK,IFAIL)
*
*        Fit cubic spline
*
         CALL G10ABF(MODE,'W',NORD,XORD,YORD,WWT,RHO,YHAT,C,LDC,RSS,DF,
     +               RES,H,WK,IFAIL)
*
*        Print results
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' RHO  = ', RHO
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' Residual sum of squares  = ', RSS
         WRITE (NOUT,99999) ' Degrees of freedom       = ', DF
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Ordered input data     Output results'
         WRITE (NOUT,*)
         WRITE (NOUT,*) '    X       Y           Fitted Values'
         WRITE (NOUT,*)
         DO 20 I = 1, NORD
            WRITE (NOUT,99998) XORD(I), YORD(I), YHAT(I)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (A,F10.3)
99998 FORMAT (1X,2F8.4,8X,F8.4)
      END
