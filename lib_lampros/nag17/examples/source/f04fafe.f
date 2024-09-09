*     F04FAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=100)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, JOB, N
*     .. Local Arrays ..
      DOUBLE PRECISION B(NMAX), D(NMAX), E(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F04FAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04FAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX) THEN
         WRITE (NOUT,99999) 'N is out of range: N = ', N
      ELSE
         READ (NIN,*) (D(I),I=1,N)
         IF (N.GT.1) READ (NIN,*) (E(I),I=2,N)
         READ (NIN,*) (B(I),I=1,N)
         JOB = 0
         IFAIL = 1
*
         CALL F04FAF(JOB,N,D,E,B,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'F04FAF fails. IFAIL =', IFAIL
         ELSE
            WRITE (NOUT,*) ' First solution vector'
            WRITE (NOUT,99998) (B(I),I=1,N)
         END IF
*
         READ (NIN,*) (B(I),I=1,N)
         JOB = 1
         IFAIL = 1
*
         CALL F04FAF(JOB,N,D,E,B,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'F04FAF fails. IFAIL =', IFAIL
         ELSE
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Second solution vector'
            WRITE (NOUT,99998) (B(I),I=1,N)
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,5F9.3)
      END
