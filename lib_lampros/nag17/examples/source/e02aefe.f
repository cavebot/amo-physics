*     E02AEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, NP1MAX
      PARAMETER        (NMAX=199,NP1MAX=NMAX+1)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION P, XCAP
      INTEGER          I, IFAIL, M, N, R
*     .. Local Arrays ..
      DOUBLE PRECISION A(NP1MAX)
*     .. External Subroutines ..
      EXTERNAL         E02AEF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02AEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=60) M
      IF (M.GT.0) THEN
         READ (NIN,*) N
         IF (N.GE.0 .AND. N.LE.NMAX) THEN
            READ (NIN,*) (A(I),I=1,N+1)
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        '  R       Argument       Value of polynomial'
            DO 40 R = 1, M
               XCAP = DBLE(2*R-M-1)/DBLE(M-1)
               IFAIL = 0
*
               CALL E02AEF(N+1,A,XCAP,P,IFAIL)
*
               WRITE (NOUT,99999) R, XCAP, P
   40       CONTINUE
            GO TO 20
         END IF
      END IF
   60 STOP
*
99999 FORMAT (1X,I3,F14.4,4X,F14.4)
      END
