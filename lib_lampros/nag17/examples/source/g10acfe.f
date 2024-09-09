*     G10ACF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDC
      PARAMETER        (NMAX=50,LDC=49)
*     .. Local Scalars ..
      DOUBLE PRECISION CRIT, DF, RHO, RSS, TOL, U
      INTEGER          I, IFAIL, J, MAXCAL, N, NORD
      CHARACTER        METHOD, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION C(LDC,3), H(NMAX), RES(NMAX), WK(7*(NMAX+2)),
     +                 WT(NMAX), WWT(NMAX), X(NMAX), XORD(NMAX),
     +                 Y(NMAX), YHAT(NMAX), YORD(NMAX)
      INTEGER          IRANK(NMAX), IWRK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G10ACF, G10ZAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G10ACF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
         READ (NIN,*) METHOD, WEIGHT
         IF (WEIGHT.EQ.'U' .OR. WEIGHT.EQ.'u') THEN
            READ (NIN,*) (X(I),Y(I),I=1,N)
         ELSE
            READ (NIN,*) (X(I),Y(I),WT(I),I=1,N)
         END IF
         READ (NIN,*) U, TOL, MAXCAL, CRIT
         IFAIL = 0
*
         IFAIL = 0
*
*        Sort data, removing ties and weighting accordingly
*
         CALL G10ZAF(WEIGHT,N,X,Y,WT,NORD,XORD,YORD,WWT,RSS,IWRK,IFAIL)
*
*        Fit cubic spline
*
         CALL G10ACF(METHOD,'W',NORD,XORD,YORD,WWT,YHAT,C,LDC,RSS,DF,
     +               RES,H,CRIT,RHO,U,TOL,MAXCAL,WK,IFAIL)
*
*        Print results
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) RSS
         WRITE (NOUT,99998) DF
         WRITE (NOUT,99997) RHO
         WRITE (NOUT,99996)
         DO 20 I = 1, NORD
            WRITE (NOUT,99995) I, XORD(I), YORD(I), YHAT(I), H(I)
   20    CONTINUE
      ENDIF
      STOP
*
99999 FORMAT (' Residual sum of squares = ',F10.2)
99998 FORMAT (' Degrees of freedom = ',F10.2)
99997 FORMAT (' RHO = ',F10.2)
99996 FORMAT (/'     Input data',16X,'Output results',
     + /'   I    X       Y   ',9X,'YHAT      H')
99995 FORMAT (I4,2F8.3,6X,2F8.3)
      END
