*     F01APF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IH, IV
      PARAMETER        (NMAX=8,IH=NMAX,IV=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, J, K, L, N
*     .. Local Arrays ..
      DOUBLE PRECISION H(IH,NMAX), V(IV,NMAX)
      INTEGER          INTGER(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01AKF, F01APF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01APF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((H(I,J),J=1,N),I=1,N)
         K = 1
         L = N
*
*        Reduce to upper Hessenberg form
         CALL F01AKF(N,K,L,H,IH,INTGER)
*
*        Form matrix of accumulated transformations
         CALL F01APF(N,K,L,INTGER,H,IH,V,IV)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Array V'
         DO 20 I = 1, N
            WRITE (NOUT,99999) (V(I,J),J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
      END
