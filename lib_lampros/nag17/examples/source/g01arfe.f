*     G01ARF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDP
      PARAMETER        (NMAX=100,LDP=100)
*     .. Local Scalars ..
      DOUBLE PRECISION UNIT, UNIT1, UNIT2
      INTEGER          I, IFAIL, J, LINES, N, NSTEPX, NSTEPY
*     .. Local Arrays ..
      DOUBLE PRECISION SORTY(NMAX), Y(NMAX)
      INTEGER          IWORK(NMAX)
      CHARACTER        PLOT(LDP,132)
*     .. External Subroutines ..
      EXTERNAL         G01ARF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01ARF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Set advisory message unit for plot output to NOUT
      CALL X04ABF(1,NOUT)
   20 READ (NIN,*,END=60) UNIT, NSTEPX, NSTEPY
      READ (NIN,*) N
      READ (NIN,*) (Y(I),I=1,N)
      UNIT1 = UNIT
      UNIT2 = UNIT
      IF (N.LE.NMAX) THEN
         IFAIL = 0
         WRITE (NOUT,*)
*
         CALL G01ARF('Fences','Print',N,Y,NSTEPX,NSTEPY,UNIT1,PLOT,LDP,
     +               LINES,SORTY,IWORK,IFAIL)
*
         IFAIL = 0
         WRITE (NOUT,*)
*
         CALL G01ARF('Extremes','Noprint',N,Y,NSTEPX,NSTEPY,UNIT2,PLOT,
     +               LDP,LINES,SORTY,IWORK,IFAIL)
*
         DO 40 I = 1, LINES
            WRITE (NOUT,99997) (PLOT(I,J),J=1,NSTEPX)
   40    CONTINUE
      ELSE
         WRITE (NOUT,99998) 'N is too large: N = ', N
         GO TO 20
      END IF
   60 STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,A,I8)
99997 FORMAT (1X,132A)
      END
