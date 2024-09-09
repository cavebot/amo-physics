*     F01LEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=50)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION LAMBDA, TOL
      INTEGER          I, IFAIL, JOB, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(NMAX), B(NMAX), C(NMAX), D(NMAX), Y(NMAX)
      INTEGER          IN(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01LEF, F04LEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01LEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX) THEN
         WRITE (NOUT,99999) 'N is out of range. N = ', N
      ELSE
         READ (NIN,*) (A(I),I=1,N)
         READ (NIN,*) (B(I),I=2,N)
         READ (NIN,*) (C(I),I=2,N)
         TOL = 0.00005D0
         LAMBDA = 0.0D0
         IFAIL = 1
*
         CALL F01LEF(N,A,LAMBDA,B,C,TOL,D,IN,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'F01LEF fails. IFAIL =', IFAIL
         ELSE
            IF (IN(N).NE.0) THEN
               WRITE (NOUT,*) 'Matrix is singular or nearly singular'
               WRITE (NOUT,99998) 'Diagonal element', IN(N), 'is small'
            ELSE
               WRITE (NOUT,*) 'Details of factorization'
               WRITE (NOUT,*)
               WRITE (NOUT,*) ' Main diagonal of U'
               WRITE (NOUT,99997) (A(I),I=1,N)
               WRITE (NOUT,*)
               WRITE (NOUT,*) ' First super-diagonal of U'
               WRITE (NOUT,99997) (B(I),I=2,N)
               WRITE (NOUT,*)
               WRITE (NOUT,*) ' Second super-diagonal of U'
               WRITE (NOUT,99997) (D(I),I=3,N)
               WRITE (NOUT,*)
               WRITE (NOUT,*) ' Multipliers'
               WRITE (NOUT,99997) (C(I),I=2,N)
               WRITE (NOUT,*)
               WRITE (NOUT,*) ' Vector of interchanges'
               WRITE (NOUT,99996) (IN(I-1),I=2,N)
*
               READ (NIN,*) (Y(I),I=1,N)
               JOB = 1
               IFAIL = 1
*
               CALL F04LEF(JOB,N,A,B,C,D,IN,Y,TOL,IFAIL)
*
               WRITE (NOUT,*)
               IF (IFAIL.NE.0) THEN
                  WRITE (NOUT,99999) 'F04LEF fails. IFAIL =', IFAIL
               ELSE
                  WRITE (NOUT,*) ' Solution vector'
                  WRITE (NOUT,99997) (Y(I),I=1,N)
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,A,I4,A)
99997 FORMAT (1X,8F9.4)
99996 FORMAT (1X,5I9)
      END
