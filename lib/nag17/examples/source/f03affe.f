*     F03AFF Example Program Text
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NMAX, IA
      DOUBLE PRECISION TWO
      PARAMETER        (NMAX=8,IA=NMAX,TWO=2.0D0)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION D1
      INTEGER          I, ID, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), P(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F03AFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F03AFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GE.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
         IFAIL = 0
*
         CALL F03AFF(N,X02AJF(),A,IA,D1,ID,P,IFAIL)
*
         WRITE (NOUT,*) 'Array A after factorization'
         DO 20 I = 1, N
            WRITE (NOUT,99998) (A(I,J),J=1,N)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Array P'
         WRITE (NOUT,99998) (P(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,99997) 'D1 = ', D1, ' ID = ', ID
         D1 = D1*TWO**ID
         WRITE (NOUT,*)
         WRITE (NOUT,99997) 'Value of determinant = ', D1
      ELSE
         WRITE (NOUT,99999) 'N is out of range: N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,8F9.4)
99997 FORMAT (1X,A,F9.4,A,I2)
      END
