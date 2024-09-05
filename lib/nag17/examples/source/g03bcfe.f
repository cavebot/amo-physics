*     G03BCF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=3,MMAX=2)
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, RSS
      INTEGER          I, IFAIL, J, M, N
      CHARACTER        SCALE, STAND
*     .. Local Arrays ..
      DOUBLE PRECISION R(MMAX,MMAX), RES(NMAX), WK(MMAX*MMAX+7*MMAX),
     +                 X(NMAX,MMAX), Y(NMAX,MMAX), YHAT(NMAX,MMAX)
*     .. External Subroutines ..
      EXTERNAL         G03BCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G03BCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M, STAND, SCALE
      IF (N.LE.NMAX .AND. M.LE.MMAX) THEN
         DO 20 I = 1, N
            READ (NIN,*) (X(I,J),J=1,M)
   20    CONTINUE
         DO 40 I = 1, N
            READ (NIN,*) (Y(I,J),J=1,M)
   40    CONTINUE
         IFAIL = 0
*
         CALL G03BCF(STAND,SCALE,N,M,X,NMAX,Y,NMAX,YHAT,R,MMAX,ALPHA,
     +               RSS,RES,WK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) '         Rotation Matrix'
         WRITE (NOUT,*)
         DO 60 I = 1, M
            WRITE (NOUT,99999) (R(I,J),J=1,M)
   60    CONTINUE
         IF (SCALE.EQ.'S' .OR. SCALE.EQ.'s') THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99998) ' Scale factor = ', ALPHA
         END IF
         WRITE (NOUT,*)
         WRITE (NOUT,*) '        Target Matrix'
         WRITE (NOUT,*)
         DO 80 I = 1, N
            WRITE (NOUT,99999) (Y(I,J),J=1,M)
   80    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) '        Fitted Matrix'
         WRITE (NOUT,*)
         DO 100 I = 1, N
            WRITE (NOUT,99999) (YHAT(I,J),J=1,M)
  100    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'RSS = ', RSS
      END IF
      STOP
*
99999 FORMAT (6(2X,F7.3))
99998 FORMAT (1X,A,F10.3)
      END
