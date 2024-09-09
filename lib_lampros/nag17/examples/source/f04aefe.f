*     F04AEF Example Program Text
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NMAX, IA, IB, IC, IAA, IBB
      PARAMETER        (NMAX=8,IA=NMAX,IB=NMAX,IC=NMAX,IAA=NMAX,
     +                 IBB=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), AA(IAA,NMAX), B(IB,1), BB(IBB,1),
     +                 C(IC,1), WKSPCE(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F04AEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04AEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      M = 1
      IF (N.GE.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N), (B(I,1),I=1,N)
         IFAIL = 0
*
         CALL F04AEF(A,IA,B,IB,N,M,C,IC,WKSPCE,AA,IAA,BB,IBB,IFAIL)
*
         WRITE (NOUT,*) ' Solution'
         WRITE (NOUT,99998) (C(I,1),I=1,N)
      ELSE
         WRITE (NOUT,99999) 'N is out of range: N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,F9.4)
      END
