*     F04AMF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX, IR, IA, IX, IB, IQR
      PARAMETER        (MMAX=8,NMAX=MMAX,IR=1,IA=MMAX,IX=NMAX,IB=MMAX,
     +                 IQR=MMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS
      INTEGER          I, IFAIL, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), ALPHA(NMAX), B(IB,IR), E(NMAX),
     +                 QR(IQR,NMAX), R(MMAX), X(IX,IR), Y(NMAX), Z(NMAX)
      INTEGER          IPIV(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F04AMF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04AMF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      WRITE (NOUT,*)
      IF (M.GT.0 .AND. M.LE.MMAX .AND. N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),(B(I,J),J=1,IR),I=1,M)
         EPS = X02AJF()
         IFAIL = 1
*
         CALL F04AMF(A,IA,X,IX,B,IB,M,N,IR,EPS,QR,IQR,ALPHA,E,Y,Z,R,
     +               IPIV,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99998) 'Error in F04AMF. IFAIL =', IFAIL
         ELSE
            WRITE (NOUT,*) ' Solution'
            DO 20 I = 1, N
               WRITE (NOUT,99999) (X(I,J),J=1,IR)
   20       CONTINUE
         END IF
      ELSE
         WRITE (NOUT,99998) 'M or N is out of range: M =', M, ' N =', N
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
99998 FORMAT (1X,A,I5,A,I5)
      END
