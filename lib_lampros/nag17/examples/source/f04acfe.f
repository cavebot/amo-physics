*     F04ACF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, M1MAX, IA, IB, IC, IRL
      PARAMETER        (NMAX=10,M1MAX=5,IA=NMAX,IB=NMAX,IC=NMAX,
     +                 IRL=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, IR, J, M, M1, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,M1MAX), B(IB,1), C(IC,1), RL(IRL,M1MAX)
*     .. External Subroutines ..
      EXTERNAL         F04ACF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04ACF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M1
      WRITE (NOUT,*)
      IR = 1
      IF (N.GT.0 .AND. N.LE.NMAX .AND. M1.GT.0 .AND. M1.LE.M1MAX) THEN
         READ (NIN,*) ((A(I,J),J=1,M1),B(I,1),I=1,N)
         M = M1 - 1
         IFAIL = 1
*
         CALL F04ACF(A,IA,B,IB,N,M,IR,C,IC,RL,IRL,M1,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'Error in F04ACF. IFAIL =', IFAIL
         ELSE
            WRITE (NOUT,*) ' Solution'
            WRITE (NOUT,99998) (C(I,1),I=1,N)
         END IF
      ELSE
         WRITE (NOUT,99999) 'N or M1 is out of range: N = ', N,
     +     '  M1 = ', M1
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,F9.4)
      END
