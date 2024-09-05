*     E04DJF Example Program Text
*     Mark 16 Release. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=10)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION OBJF
      INTEGER          I, IFAIL, INFORM, ITER, N
*     .. Local Arrays ..
      DOUBLE PRECISION OBJGRD(NMAX), USER(4*NMAX), WORK(13*NMAX),
     +                 X(NMAX)
      INTEGER          IUSER(NMAX), IWORK(NMAX+1)
*     .. External Subroutines ..
      EXTERNAL         E04DGF, E04DJF, E04DKF, OBJFN1, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04DJF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read X from data file
*
         READ (NIN,*) (X(I),I=1,N)
*
*        Set two options using E04DKF
*
         CALL E04DKF(' Verify Level = -1 ')
*
         CALL E04DKF(' Maximum Step Length = 100.0 ')
*
*        Set the unit number for advisory messages to NOUT
*
         CALL X04ABF(1,NOUT)
*
*        Read the options file for the remaining options
*
         CALL E04DJF(NIN,INFORM)
*
         IF (INFORM.NE.0) THEN
            WRITE (NOUT,99999) 'E04DJF terminated with INFORM = ',
     +        INFORM
            STOP
         END IF
*
*        Solve the problem
*
         IFAIL = -1
*
         CALL E04DGF(N,OBJFN1,ITER,OBJF,OBJGRD,X,IWORK,WORK,IUSER,USER,
     +               IFAIL)
*
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
      END
*
      SUBROUTINE OBJFN1(MODE,N,X,OBJF,OBJGRD,NSTATE,IUSER,USER)
*     Routine to evaluate F(x) and approximate its 1st derivatives
*     .. Scalar Arguments ..
      DOUBLE PRECISION  OBJF
      INTEGER           MODE, N, NSTATE
*     .. Array Arguments ..
      DOUBLE PRECISION  OBJGRD(N), USER(*), X(N)
      INTEGER           IUSER(*)
*     .. Local Scalars ..
      DOUBLE PRECISION  EPSRF
      INTEGER           I, IFAIL, IMODE, IWARN, LHES, MSGLVL
*     .. Local Arrays ..
      DOUBLE PRECISION  USE(1)
      INTEGER           IUSE(1)
*     .. External Subroutines ..
      EXTERNAL          E04XAF, OBJFN2
*     .. Executable Statements ..
      IF (MODE.EQ.0) THEN
*        Evaluate F(x) only
         CALL OBJFN2(MODE,N,X,OBJF,OBJGRD,NSTATE,IUSE,USE)
*
      ELSE IF (MODE.EQ.2) THEN
*        Evaluate F(x) and approximate its 1st derivatives
         MSGLVL = 0
         EPSRF = 0.0D0
         IMODE = 0
         LHES = N
         DO 20 I = 1, N
            USER(I) = 0.0D0
   20    CONTINUE
         IFAIL = 1
*
         CALL E04XAF(MSGLVL,N,EPSRF,X,IMODE,OBJFN2,LHES,USER(1),OBJF,
     +               OBJGRD,USER(N+1),USER(2*N+1),IWARN,USER(3*N+1),
     +               IUSE,USE,IUSER,IFAIL)
*
      END IF
*
      RETURN
      END
      SUBROUTINE OBJFN2(MODE,N,X,OBJF,OBJGRD,NSTATE,IUSE,USE)
*     Routine to evaluate F(x)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  OBJF
      INTEGER           MODE, N, NSTATE
*     .. Array Arguments ..
      DOUBLE PRECISION  OBJGRD(N), USE(*), X(N)
      INTEGER           IUSE(*)
*     .. Local Scalars ..
      DOUBLE PRECISION  X1, X2
*     .. Intrinsic Functions ..
      INTRINSIC         EXP
*     .. Executable Statements ..
      X1 = X(1)
      X2 = X(2)
*
      OBJF = EXP(X1)*(4.0D0*X1**2+2.0D0*X2**2+4.0D0*X1*X2+2.0D0*X2+
     +       1.0D0)
*
      RETURN
      END
