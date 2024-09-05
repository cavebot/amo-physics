*     E04XAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, LHES, LWORK
      PARAMETER        (N=4,LHES=N,LWORK=N*N+N)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPSRF, OBJF
      INTEGER          I, IFAIL, IMODE, IWARN, J, MODE, MSGLVL
*     .. Local Arrays ..
      DOUBLE PRECISION HCNTRL(N), HESIAN(LHES,N), HFORW(N), OBJGRD(N),
     +                 USER(1), WORK(LWORK), X(N)
      INTEGER          INFO(N), IUSER(1)
*     .. External Subroutines ..
      EXTERNAL         E04XAF, OBJFUN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04XAF Example Program Results'
      MSGLVL = 0
*     Set the point at which the derivatives are to be estimated.
      X(1) = 3.0D0
      X(2) = -1.0D0
      X(3) = 0.0D0
      X(4) = 1.0D0
*     Take default value of EPSRF.
      EPSRF = -1.0D0
*     Illustrate the different values of MODE.
      DO 40 IMODE = 0, 2
         MODE = IMODE
         WRITE (NOUT,*)
         IF (MODE.EQ.0) THEN
            WRITE (NOUT,*)
     +        'Find gradients and Hessian diagonals given function only'
            WRITE (NOUT,*) '( i.e. MODE = 0 ).'
         ELSE IF (MODE.EQ.1) THEN
            WRITE (NOUT,*)
     +        'Find Hessian matrix given function and gradients'
            WRITE (NOUT,*) '( i.e. MODE = 1 ).'
         ELSE IF (MODE.EQ.2) THEN
            WRITE (NOUT,*)
     +        'Find gradients and Hessian matrix given function only'
            WRITE (NOUT,*) '( i.e. MODE = 2 ).'
         END IF
*        Set HFORW(I) = -1.0 so that E04XAF computes the initial trial
*        interval.
         DO 20 I = 1, N
            HFORW(I) = -1.0D0
   20    CONTINUE
         IFAIL = 1
*
         CALL E04XAF(MSGLVL,N,EPSRF,X,MODE,OBJFUN,LHES,HFORW,OBJF,
     +               OBJGRD,HCNTRL,HESIAN,IWARN,WORK,IUSER,USER,INFO,
     +               IFAIL)
*
         IF (IFAIL.EQ.0 .OR. IFAIL.EQ.2) THEN
            WRITE (NOUT,99999) 'Function value is ', OBJF
            IF (MODE.EQ.1) THEN
               WRITE (NOUT,*) 'Gradient vector is'
               WRITE (NOUT,99998) (OBJGRD(I),I=1,N)
            ELSE
               WRITE (NOUT,*) 'Estimated gradient vector is'
               WRITE (NOUT,99998) (OBJGRD(I),I=1,N)
            END IF
            IF (MODE.EQ.0) THEN
               WRITE (NOUT,*) 'Estimated Hessian matrix diagonal is'
               WRITE (NOUT,99998) (HESIAN(I,1),I=1,N)
            ELSE
               WRITE (NOUT,*)
     +           'Estimated Hessian matrix (machine dependent) is'
               WRITE (NOUT,99998) ((HESIAN(I,J),J=1,N),I=1,N)
            END IF
         ELSE
            WRITE (NOUT,*)
            WRITE (NOUT,99997) 'On exit from E04XAF IFAIL = ', IFAIL
         END IF
   40 CONTINUE
      STOP
*
99999 FORMAT (1X,A,1P,D12.4)
99998 FORMAT (4(1X,1P,D12.4))
99997 FORMAT (1X,A,I2)
      END
*
      SUBROUTINE OBJFUN(MODE,N,X,OBJF,OBJGRD,NSTATE,IUSER,USER)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  OBJF
      INTEGER           MODE, N, NSTATE
*     .. Array Arguments ..
      DOUBLE PRECISION  OBJGRD(N), USER(*), X(N)
      INTEGER           IUSER(*)
*     .. Local Scalars ..
      DOUBLE PRECISION  A, B, C, D
*     .. Executable Statements ..
      A = X(1) + 10.0D0*X(2)
      B = X(3) - X(4)
      C = X(2) - 2.0D0*X(3)
      D = X(1) - X(4)
      OBJF = A**2 + 5.0D0*B**2 + C**4 + 10.0D0*D**4
      IF (MODE.EQ.1) THEN
         OBJGRD(1) = 4.0D1*X(1)**3 + 2.0D0*X(1) - 1.2D2*X(4)*X(1)**2 +
     +               1.2D2*X(1)*X(4)**2 + 2.0D1*X(2) - 4.0D1*X(4)**3
         OBJGRD(2) = 2.0D2*X(2) + 2.0D1*X(1) + 4.0D0*X(2)**3 +
     +               4.8D1*X(2)*X(3)**2 - 2.4D1*X(3)*X(2)**2 -
     +               32.0D0*X(3)**3
         OBJGRD(3) = 1.0D1*X(3) - 1.0D1*X(4) - 8.0D0*X(2)**3 +
     +               4.8D1*X(3)*X(2)**2 - 9.6D1*X(2)*X(3)**2 +
     +               6.4D1*X(3)**3
         OBJGRD(4) = 1.0D1*X(4) - 1.0D1*X(3) - 4.0D1*X(1)**3 +
     +               1.2D2*X(4)*X(1)**2 - 1.2D2*X(1)*X(4)**2 +
     +               4.0D1*X(4)**3
      END IF
      RETURN
      END
