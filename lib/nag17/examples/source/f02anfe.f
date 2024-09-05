*     F02ANF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IHR, IHI
      PARAMETER        (NMAX=4,IHR=NMAX,IHI=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, L, N
*     .. Local Arrays ..
      DOUBLE PRECISION HI(IHI,NMAX), HR(IHR,NMAX), WI(NMAX), WR(NMAX)
      INTEGER          INTGER(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01AMF, F02ANF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02ANF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX) THEN
         WRITE (NOUT,99999) 'N is out of range: N = ', N
         STOP
      END IF
      READ (NIN,*) ((HR(I,J),HI(I,J),J=1,N),I=1,N)
      K = 1
      L = N
*
*     Reduce to upper Hessenberg form
      CALL F01AMF(N,K,L,HR,IHR,HI,IHI,INTGER)
*
      IFAIL = 1
*     Eigenvalues of Hessenberg form
      CALL F02ANF(N,X02AJF(),HR,IHR,HI,IHI,WR,WI,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Error in F02ANF. IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99998) (' (',WR(I),',',WI(I),')',I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,A,F7.3,A,F7.3,A)
      END
