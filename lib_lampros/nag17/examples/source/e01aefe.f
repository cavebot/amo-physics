*     E01AEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX, IPMX, LWRK, LIWRK
      PARAMETER        (MMAX=4,NMAX=8,IPMX=2,LWRK=7*NMAX+5*IPMX+MMAX+7,
     +                 LIWRK=2*MMAX+2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION XMAX, XMIN
      INTEGER          I, IFAIL, IP1, IPMAX, IRES, IY, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(NMAX), WRK(LWRK), X(MMAX), Y(NMAX)
      INTEGER          IP(MMAX), IWRK(LIWRK)
*     .. External Subroutines ..
      EXTERNAL         E01AEF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E01AEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=120) M, XMIN, XMAX
      IF (M.GT.0 .AND. M.LE.MMAX) THEN
         N = 0
         IPMAX = 0
         DO 40 I = 1, M
            READ (NIN,*) IP(I), X(I), (Y(J),J=N+1,N+IP(I)+1)
            IPMAX = MAX(IPMAX,IP(I))
            N = N + IP(I) + 1
   40    CONTINUE
         IF (N.LE.NMAX .AND. IPMAX.LE.IPMX) THEN
            IFAIL = 1
*
            CALL E01AEF(M,XMIN,XMAX,X,Y,IP,N,-1,-1,A,WRK,LWRK,IWRK,
     +                  LIWRK,IFAIL)
*
            WRITE (NOUT,*)
            IF (IFAIL.EQ.0 .OR. IFAIL.GE.4) THEN
               WRITE (NOUT,99999)
     +           'Total number of interpolating conditions =', N
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Interpolating polynomial'
               WRITE (NOUT,*)
               WRITE (NOUT,*) '   I    Chebyshev Coefficient A(I+1)'
               DO 60 I = 1, N
                  WRITE (NOUT,99998) I - 1, A(I)
   60          CONTINUE
               WRITE (NOUT,*)
               WRITE (NOUT,*) '  X    R   Rth derivative    Residual'
               IY = 0
               IRES = IPMAX + 1
               DO 100 I = 1, M
                  IP1 = IP(I) + 1
                  DO 80 J = 1, IP1
                     IY = IY + 1
                     IRES = IRES + 1
                     IF (J-1.NE.0) THEN
                        WRITE (NOUT,99997) J - 1, Y(IY), WRK(IRES)
                     ELSE
                        WRITE (NOUT,99996) X(I), '   0', Y(IY),
     +                    WRK(IRES)
                     END IF
   80             CONTINUE
  100          CONTINUE
            ELSE
               WRITE (NOUT,99995) 'E01AEF exits with IFAIL =', IFAIL
            END IF
         END IF
         GO TO 20
      END IF
  120 STOP
*
99999 FORMAT (1X,A,I4)
99998 FORMAT (1X,I4,F20.3)
99997 FORMAT (5X,I4,F12.1,F17.6)
99996 FORMAT (1X,F4.1,A,F12.1,F17.6)
99995 FORMAT (1X,A,I2,A)
      END
