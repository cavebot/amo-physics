*     E02ZAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, MAXPX, MAXPY, NADMAX, NPOINT
      PARAMETER        (MMAX=20,MAXPX=12,MAXPY=12,NADMAX=(MAXPX-7)
     +                 *(MAXPY-7),NPOINT=MMAX+NADMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IADRES, IFAIL, M, NADRES, PX, PY
*     .. Local Arrays ..
      DOUBLE PRECISION LAMDA(MAXPX), MU(MAXPY), X(MMAX), Y(MMAX)
      INTEGER          ADRES(NADMAX), POINT(NPOINT)
*     .. External Subroutines ..
      EXTERNAL         E02ZAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02ZAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*) M
      IF (M.GT.0 .AND. M.LE.MMAX) THEN
         READ (NIN,*) PX, PY
         IF (PX.LE.MAXPX .AND. PY.LE.MAXPY) THEN
            NADRES = (PX-7)*(PY-7)
*           Read data points and intercepts of panel sides
            READ (NIN,*) (X(I),Y(I),I=1,M)
            IF (PX.GT.8) READ (NIN,*) (LAMDA(I),I=5,PX-4)
            IF (PY.GT.8) READ (NIN,*) (MU(I),I=5,PY-4)
*           Sort points into panel order
            IFAIL = 0
*
            CALL E02ZAF(PX,PY,LAMDA,MU,M,X,Y,POINT,NPOINT,ADRES,NADRES,
     +                  IFAIL)
*
*           Output points in panel order
            DO 60 I = 1, NADRES
               WRITE (NOUT,*)
               WRITE (NOUT,99999) 'Panel', I
               IADRES = M + I
   40          IADRES = POINT(IADRES)
               IF (IADRES.GT.0) THEN
                  WRITE (NOUT,99998) X(IADRES), Y(IADRES)
                  GO TO 40
               END IF
   60       CONTINUE
            GO TO 20
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I4)
99998 FORMAT (1X,2F7.2)
      END
