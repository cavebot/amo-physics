*     G02CEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, N2, ISSP, ICORR, ISSPX, ICORRX
      PARAMETER        (N=4,N2=3,ISSP=N,ICORR=N,ISSPX=N2,ICORRX=N2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION CORR(ICORR,N), CORRX(ICORRX,N2), SSP(ISSP,N),
     +                 SSPX(ISSPX,N2), STD(N), STDX(N2), XM(N), XMX(N2)
      INTEGER          IORDER(N2)
*     .. External Subroutines ..
      EXTERNAL         G02CEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02CEF Example Program Results'
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
      IORDER(1) = 4
      IORDER(2) = 1
      IORDER(3) = 2
      IFAIL = 1
*
      CALL G02CEF(N,XM,STD,SSP,ISSP,CORR,ICORR,N2,IORDER,XMX,STDX,SSPX,
     +            ISSPX,CORRX,ICORRX,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99997) 'Routine fails, IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,99996) 'New vector XMX   :   ', (XMX(I),I=1,N2)
         WRITE (NOUT,*)
         WRITE (NOUT,99996) 'New vector STDX  :   ', (STDX(I),I=1,N2)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'New matrix SSPX  :'
         WRITE (NOUT,99995) ((SSPX(I,J),J=1,N2),I=1,N2)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'New matrix CORRX :'
         WRITE (NOUT,99995) ((CORRX(I,J),J=1,N2),I=1,N2)
      END IF
      STOP
*
99999 FORMAT (1X,A,4F10.4)
99998 FORMAT (1X,4F10.4)
99997 FORMAT (1X,A,I2)
99996 FORMAT (1X,A,3F10.4)
99995 FORMAT (1X,3F10.4)
      END
