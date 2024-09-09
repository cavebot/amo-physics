*     G05EAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, IC, NR
      PARAMETER        (N=2,IC=N,NR=(N+1)*(N+2)/2)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION A(N), C(IC,N), R(NR), Z(N)
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05EAF, G05EZF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05EAF Example Program Results'
      WRITE (NOUT,*)
      A(1) = 1.0D0
      A(2) = 2.0D0
      C(1,1) = 2.0D0
      C(2,2) = 3.0D0
      C(1,2) = 1.0D0
      C(2,1) = 1.0D0
      CALL G05CBF(0)
      IFAIL = 0
*
      CALL G05EAF(A,N,C,IC,0.01D0,R,NR,IFAIL)
*
      DO 20 I = 1, 5
         IFAIL = 0
         CALL G05EZF(Z,N,R,NR,IFAIL)
         WRITE (NOUT,99999) (Z(J),J=1,N)
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,2F10.4)
      END
