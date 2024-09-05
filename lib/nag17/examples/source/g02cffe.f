*     G02CFF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, ISSP, ICORR
      PARAMETER        (N=3,ISSP=N,ICORR=N)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION CORR(ICORR,N), SSP(ISSP,N), STD(N), XM(N)
      INTEGER          IORDER(N), KW(N)
*     .. External Subroutines ..
      EXTERNAL         G02CFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02CFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) (XM(I),I=1,N), (STD(I),I=1,N),
     +  ((SSP(I,J),J=1,N),I=1,N), ((CORR(I,J),J=1,N),I=1,N)
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Original vector XM   :   ', (XM(I),I=1,N)
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Original vector STD  :   ', (STD(I),I=1,N)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Original matrix SSP  :'
      WRITE (NOUT,99998) ((SSP(I,J),J=1,N),I=1,N)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Original matrix CORR :'
      WRITE (NOUT,99998) ((CORR(I,J),J=1,N),I=1,N)
      WRITE (NOUT,*)
      IORDER(1) = 1
      IORDER(2) = 3
      IORDER(3) = 2
      IFAIL = 1
*
      CALL G02CFF(N,IORDER,XM,STD,SSP,ISSP,CORR,ICORR,KW,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99997) 'Routine fails, IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,99996) 'New vector XM   :   ', (XM(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,99996) 'New vector STD  :   ', (STD(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'New matrix SSP  :'
         WRITE (NOUT,99995) ((SSP(I,J),J=1,N),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'New matrix CORR :'
         WRITE (NOUT,99995) ((CORR(I,J),J=1,N),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,A,3F10.4)
99998 FORMAT (1X,3F10.4)
99997 FORMAT (1X,A,I2)
99996 FORMAT (1X,A,3F10.4)
99995 FORMAT (1X,3F10.4)
      END
