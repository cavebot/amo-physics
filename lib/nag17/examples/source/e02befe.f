*     E02BEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NEST, LWRK
      PARAMETER        (MMAX=50,NEST=MMAX+4,LWRK=4*MMAX+16*NEST+41)
*     .. Local Scalars ..
      DOUBLE PRECISION FP, S, TXR
      INTEGER          IFAIL, J, M, N, R
      CHARACTER*1      START
*     .. Local Arrays ..
      DOUBLE PRECISION C(NEST), K(NEST), SP(2*MMAX-1), W(MMAX),
     +                 WRK(LWRK), X(MMAX), Y(MMAX)
      INTEGER          IWRK(NEST)
*     .. External Subroutines ..
      EXTERNAL         E02BBF, E02BEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02BEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Input the number of data points, followed by the data points (X),
*     the function values (Y) and the weights (W).
      READ (NIN,*) M
      IF (M.GT.0 .AND. M.LE.MMAX) THEN
         DO 20 R = 1, M
            READ (NIN,*) X(R), Y(R), W(R)
   20    CONTINUE
         START = 'Cold Start'
*        Read in successive values of S until end of data file.
   40    READ (NIN,*,END=120) S
*        Determine the spline approximation.
         IFAIL = 0
*
         CALL E02BEF(START,M,X,Y,W,S,NEST,N,K,C,FP,WRK,LWRK,IWRK,IFAIL)
*
*        Evaluate the spline at each X point and midway between
*        X points, saving the results in SP.
         DO 60 R = 1, M
            IFAIL = 0
*
            CALL E02BBF(N,K,C,X(R),SP((R-1)*2+1),IFAIL)
*
   60    CONTINUE
         DO 80 R = 1, M - 1
            IFAIL = 0
            TXR = (X(R)+X(R+1))/2
*
            CALL E02BBF(N,K,C,TXR,SP(R*2),IFAIL)
*
   80    CONTINUE
*        Output the results.
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Calling with smoothing factor S =', S
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     '                                       B-Spline'
         WRITE (NOUT,*)
     +     '             J       Knot K(J+2)   Coefficient C(J)'
         WRITE (NOUT,99998) 1, C(1)
         DO 100 J = 2, N - 5
            WRITE (NOUT,99997) J, K(J+2), C(J)
  100    CONTINUE
         WRITE (NOUT,99998) N - 4, C(N-4)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Weighted sum of squared residuals FP =', FP
         IF (FP.EQ.0.0D0) THEN
            WRITE (NOUT,*) '(The spline is an interpolating spline)'
         ELSE IF (N.EQ.8) THEN
            WRITE (NOUT,*)
     +     '(The spline is the weighted least-squares cubic polynomial)'
         END IF
         WRITE (NOUT,*)
         START = 'Warm Start'
         GO TO 40
      END IF
  120 STOP
*
99999 FORMAT (1X,A,1P,D12.3)
99998 FORMAT (11X,I4,16X,F16.4)
99997 FORMAT (11X,I4,2F16.4)
      END
