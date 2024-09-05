*     E02DDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NXEST, NYEST, NMAX, LWRK, LIWRK
      PARAMETER        (MMAX=30,NXEST=14,NYEST=14,NMAX=7,
     +                 LWRK=(7*(NXEST-4)*(NYEST-4)+25*(NXEST-4))
     +                 *(NXEST-3)+2*((NXEST-4)+(NYEST-4)+4*MMAX)
     +                 +23*(NXEST-4)+56,LIWRK=MMAX+2*(NXEST-7)*(NYEST-7)
     +                 )
*     .. Local Scalars ..
      DOUBLE PRECISION DELTA, FP, S, XHI, XLO, YHI, YLO
      INTEGER          I, IFAIL, J, M, NPX, NPY, NX, NY, RANK
      CHARACTER        START
*     .. Local Arrays ..
      DOUBLE PRECISION C((NXEST-4)*(NYEST-4)), F(MMAX), FG(NMAX*NMAX),
     +                 LAMDA(NXEST), MU(NYEST), PX(NMAX), PY(NMAX),
     +                 W(MMAX), WRK(LWRK), X(MMAX), Y(MMAX)
      INTEGER          IWRK(LIWRK)
*     .. External Subroutines ..
      EXTERNAL         CPRINT, E02DDF, E02DFF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02DDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Input the number of data-points M.
      READ (NIN,*) M
      IF (M.GT.0 .AND. M.LE.MMAX) THEN
*        Input the data-points and the weights.
         DO 20 I = 1, M
            READ (NIN,*) X(I), Y(I), F(I), W(I)
   20    CONTINUE
         START = 'Cold Start'
         READ (NIN,*,END=120) S
*        Determine the spline approximation.
         IFAIL = 0
*
         CALL E02DDF(START,M,X,Y,F,W,S,NXEST,NYEST,NX,LAMDA,NY,MU,C,FP,
     +               RANK,WRK,LWRK,IWRK,LIWRK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Calling with smoothing factor S =', S,
     +     ': NX =', NX, ', NY =', NY, ','
         WRITE (NOUT,99998) 'rank deficiency =', (NX-4)*(NY-4) - RANK
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     '               I   Knot LAMDA(I)      J     Knot MU(J)'
         WRITE (NOUT,*)
         DO 40 J = 4, MAX(NX,NY) - 3
            IF (J.LE.NX-3 .AND. J.LE.NY-3) THEN
               WRITE (NOUT,99996) J, LAMDA(J), J, MU(J)
            ELSE IF (J.LE.NX-3) THEN
               WRITE (NOUT,99996) J, LAMDA(J)
            ELSE IF (J.LE.NY-3) THEN
               WRITE (NOUT,99995) J, MU(J)
            END IF
   40    CONTINUE
         CALL CPRINT(C,NY,NX,NOUT)
         WRITE (NOUT,*)
         WRITE (NOUT,99997) ' Sum of squared residuals FP =', FP
         IF (NX.EQ.8 .AND. NY.EQ.8) THEN
            WRITE (NOUT,*)
     +        ' ( The spline is the least-squares bi-cubic polynomial )'
         END IF
*        Evaluate the spline on a rectangular grid at NPX*NPY points
*        over the domain (XLO to XHI) x (YLO to YHI).
         READ (NIN,*) NPX, XLO, XHI
         READ (NIN,*) NPY, YLO, YHI
         IF (NPX.LE.NMAX .AND. NPY.LE.NMAX) THEN
            DELTA = (XHI-XLO)/(NPX-1)
            DO 60 I = 1, NPX
               PX(I) = MIN(XLO+(I-1)*DELTA,XHI)
   60       CONTINUE
            DO 80 I = 1, NPY
               PY(I) = MIN(YLO+(I-1)*DELTA,YHI)
   80       CONTINUE
*
            CALL E02DFF(NPX,NPY,NX,NY,PX,PY,LAMDA,MU,C,FG,WRK,LWRK,IWRK,
     +                  LIWRK,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Values of computed spline:'
            WRITE (NOUT,*)
            WRITE (NOUT,99994) '          X', (PX(I),I=1,NPX)
            WRITE (NOUT,*) '     Y'
            DO 100 I = NPY, 1, -1
               WRITE (NOUT,99993) PY(I), (FG(NPY*(J-1)+I),J=1,NPX)
  100       CONTINUE
         END IF
      END IF
  120 CONTINUE
*
99999 FORMAT (1X,A,1P,D13.4,A,I5,A,I5,A)
99998 FORMAT (1X,A,I5)
99997 FORMAT (1X,A,1P,D13.4,A)
99996 FORMAT (1X,I16,F12.4,I11,F12.4)
99995 FORMAT (1X,I39,F12.4)
99994 FORMAT (1X,A,7F8.2)
99993 FORMAT (1X,F8.2,3X,7F8.2)
      END
*
      SUBROUTINE CPRINT(C,NY,NX,NOUT)
*     .. Scalar Arguments ..
      INTEGER           NOUT, NX, NY
*     .. Array Arguments ..
      DOUBLE PRECISION  C(NY-4,NX-4)
*     .. Local Scalars ..
      INTEGER           I, J
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'The B-spline coefficients:'
      WRITE (NOUT,*)
      DO 20 I = 1, NY - 4
         WRITE (NOUT,99999) (C(I,J),J=1,NX-4)
   20 CONTINUE
      RETURN
*
99999 FORMAT (1X,7F9.2)
      END
