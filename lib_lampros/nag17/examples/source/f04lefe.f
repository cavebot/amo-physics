*     F04LEF Example Program Text
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
      DOUBLE PRECISION A(NMAX), B(NMAX), C(NMAX), D(NMAX), Y(NMAX),
     +                 Z(NMAX)
      INTEGER          IN(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01LEF, F04LEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04LEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX) THEN
         WRITE (NOUT,99999) 'N is out of range. N = ', N
      ELSE
         READ (NIN,*) (A(I),I=1,N)
         IF (N.GT.1) THEN
            READ (NIN,*) (B(I),I=2,N)
            READ (NIN,*) (C(I),I=2,N)
         END IF
         TOL = 5.0D-5
         LAMBDA = 0.0D0
         IFAIL = 1
*
         CALL F01LEF(N,A,LAMBDA,B,C,TOL,D,IN,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'F01LEF fails. IFAIL =', IFAIL
         ELSE
            IF (IN(N).NE.0) THEN
               WRITE (NOUT,*) 'Matrix is singular or nearly singular'
               WRITE (NOUT,99998) 'Diagonal element', IN(N), 'is small'
            ELSE
               READ (NIN,*) (Y(I),I=1,N)
               DO 20 I = 1, N
                  Z(I) = Y(I)
   20          CONTINUE
               JOB = 1
               IFAIL = 1
*
               CALL F04LEF(JOB,N,A,B,C,D,IN,Y,TOL,IFAIL)
*
               IF (IFAIL.NE.0) THEN
                  WRITE (NOUT,*)
                  WRITE (NOUT,99999) 'F04LEF fails. IFAIL =', IFAIL
               ELSE
                  WRITE (NOUT,*)
                  WRITE (NOUT,*) 'Solution vector for T*X = Y'
                  WRITE (NOUT,99997) (Y(I),I=1,N)
               END IF
               JOB = 2
               IFAIL = 1
*
               CALL F04LEF(JOB,N,A,B,C,D,IN,Z,TOL,IFAIL)
*
               IF (IFAIL.NE.0) THEN
                  WRITE (NOUT,*)
                  WRITE (NOUT,99999) 'F04LEF fails. IFAIL =', IFAIL
               ELSE
                  WRITE (NOUT,*)
                  WRITE (NOUT,*)
     +              'Solution vector for transpose(T)*X = Y'
                  WRITE (NOUT,99997) (Z(I),I=1,N)
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,A,I4,A)
99997 FORMAT (1X,5F9.3)
      END
