*     G01ADF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          KMAX
      PARAMETER        (KMAX=50)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION S2, S3, S4, XMEAN
      INTEGER          I, IFAIL, J, K, KMIN1, N, NPROB
*     .. Local Arrays ..
      DOUBLE PRECISION X(KMAX)
      INTEGER          IFREQ(KMAX)
*     .. External Subroutines ..
      EXTERNAL         G01ADF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01ADF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NPROB
      DO 20 J = 1, NPROB
         READ (NIN,*) KMIN1
         K = KMIN1 + 1
         IF (K.GE.2 .AND. K.LE.KMAX) THEN
            READ (NIN,*) (X(I),IFREQ(I),I=1,KMIN1), X(K)
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Problem ', J
            WRITE (NOUT,99999) 'Number of classes ', KMIN1
            IFAIL = 1
*
            CALL G01ADF(K,X,IFREQ,XMEAN,S2,S3,S4,N,IFAIL)
*
            WRITE (NOUT,*)
            IF (IFAIL.EQ.0) THEN
               WRITE (NOUT,*) 'Successful call of G01ADF'
               WRITE (NOUT,*)
               WRITE (NOUT,*) '          Class          Frequency'
               WRITE (NOUT,*)
               WRITE (NOUT,99998) (X(I),X(I+1),IFREQ(I),I=1,KMIN1)
               WRITE (NOUT,*)
               WRITE (NOUT,99997) ' Mean ', XMEAN
               WRITE (NOUT,99996) ' Std devn', S2
               WRITE (NOUT,99996) ' Skewness', S3
               WRITE (NOUT,99996) ' Kurtosis', S4
               WRITE (NOUT,99995) ' Number of cases', N
            ELSE
               WRITE (NOUT,99999)
     +           'Unsuccessful call of G01ADF. IFAIL = ', IFAIL
            END IF
         ELSE
            STOP
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I4)
99998 FORMAT (1X,2F10.2,I12)
99997 FORMAT (1X,A,F16.4)
99996 FORMAT (1X,A,F13.4)
99995 FORMAT (1X,A,I8)
      END
