*     G01AAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=30)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION S2, S3, S4, WTSUM, XBAR, XMAX, XMIN
      INTEGER          I, IFAIL, IWT, J, N, NPROB
*     .. Local Arrays ..
      DOUBLE PRECISION WT(NMAX), X(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G01AAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01AAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NPROB
      DO 20 J = 1, NPROB
         READ (NIN,*) N, IWT
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Problem ', J
         WRITE (NOUT,99999) 'Number of cases ', N
         IF (N.GE.1 .AND. N.LE.NMAX) THEN
            READ (NIN,*) (X(I),I=1,N)
            WRITE (NOUT,*) 'Data as input -'
            WRITE (NOUT,99998) (X(I),I=1,N)
            IF (IWT.EQ.1) THEN
               WRITE (NOUT,*) 'Weights as input -'
               READ (NIN,*) (WT(I),I=1,N)
               WRITE (NOUT,99998) (WT(I),I=1,N)
            END IF
            IFAIL = 1
*
            CALL G01AAF(N,X,IWT,WT,XBAR,S2,S3,S4,XMIN,XMAX,WTSUM,IFAIL)
*
            WRITE (NOUT,*)
            IF (IFAIL.EQ.0) THEN
               WRITE (NOUT,*) 'Successful call of G01AAF'
               WRITE (NOUT,99999) 'No. of valid cases    ', IWT
               WRITE (NOUT,99997) 'Mean          ', XBAR
               WRITE (NOUT,99997) 'Std devn      ', S2
               WRITE (NOUT,99997) 'Skewness      ', S3
               WRITE (NOUT,99997) 'Kurtosis      ', S4
               WRITE (NOUT,99997) 'Minimum       ', XMIN
               WRITE (NOUT,99997) 'Maximum       ', XMAX
               WRITE (NOUT,99997) 'Sum of weights', WTSUM
            ELSE
               WRITE (NOUT,*) 'Unsuccessful call of G01AAF'
               WRITE (NOUT,99999) 'IFAIL =', IFAIL
               IF (IFAIL.EQ.2) THEN
                  WRITE (NOUT,99999) 'No. of valid cases', IWT
                  WRITE (NOUT,99997) 'Mean          ', XBAR
                  WRITE (NOUT,99997) 'Minimum       ', XMIN
                  WRITE (NOUT,99997) 'Maximum       ', XMAX
                  WRITE (NOUT,99997) 'Sum of weights', WTSUM
                  WRITE (NOUT,*) 'Std devn and coeffts of skewness'
                  WRITE (NOUT,*) 'and kurtosis not defined'
               END IF
            END IF
         ELSE
            STOP
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,5F12.1)
99997 FORMAT (1X,A,F13.1)
      END
