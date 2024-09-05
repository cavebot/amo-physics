*     F04EAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=100)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, N
*     .. Local Arrays ..
      DOUBLE PRECISION B(NMAX), D(NMAX), DL(NMAX), DU(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F04EAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04EAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX) THEN
         WRITE (NOUT,99999) 'N is out of range: N = ', N
      ELSE
         READ (NIN,*) (D(I),I=1,N)
         IF (N.GT.1) THEN
            READ (NIN,*) (DU(I),I=2,N)
            READ (NIN,*) (DL(I),I=2,N)
         END IF
         READ (NIN,*) (B(I),I=1,N)
         IFAIL = 1
*
         CALL F04EAF(N,D,DU,DL,B,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'F04EAF fails. IFAIL =', IFAIL
         ELSE
            WRITE (NOUT,*) 'Solution vector'
            WRITE (NOUT,99998) (B(I),I=1,N)
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,5F9.3)
      END
