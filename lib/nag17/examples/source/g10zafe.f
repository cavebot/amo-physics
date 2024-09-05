*     G10ZAF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=100)
*     .. Local Scalars ..
      DOUBLE PRECISION RSS
      INTEGER          I, IFAIL, N, NORD
      CHARACTER        WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION WT(NMAX), WTORD(NMAX), X(NMAX), XORD(NMAX),
     +                 Y(NMAX), YORD(NMAX)
      INTEGER          IWRK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G10ZAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G10ZAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
         READ (NIN,*) WEIGHT
         DO 20 I = 1, N
            READ (NIN,*) X(I), Y(I)
   20    CONTINUE
         IFAIL = 0
*
         CALL G10ZAF(WEIGHT,N,X,Y,WT,NORD,XORD,YORD,WTORD,RSS,IWRK,
     +               IFAIL)
*
*        Print results
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) NORD
         WRITE (NOUT,99998) RSS
         WRITE (NOUT,*)
         WRITE (NOUT,99997)
         DO 40 I = 1, NORD
            WRITE (NOUT,99996) XORD(I), YORD(I), WTORD(I)
   40    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,'Number of distinct observations = ',I6)
99998 FORMAT (1X,'Residual sum of squares = ',F13.5)
99997 FORMAT (13X,'X                 Y                 WT')
99996 FORMAT (5X,F13.5,5X,F13.5,5X,F13.5)
      END
