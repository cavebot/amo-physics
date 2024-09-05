*     E01SEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=100,NMAX=25)
*     .. Local Scalars ..
      DOUBLE PRECISION RNQ, RNW, XHI, XLO, YHI, YLO
      INTEGER          I, IFAIL, J, M, MINNQ, NQ, NW, NX, NY
*     .. Local Arrays ..
      DOUBLE PRECISION F(MMAX), FNODES(5*MMAX), PF(NMAX), PX(NMAX),
     +                 PY(NMAX), WRK(6*MMAX), X(MMAX), Y(MMAX)
*     .. External Subroutines ..
      EXTERNAL         E01SEF, E01SFF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E01SEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Input the number of nodes.
      READ (NIN,*) M
      IF (M.GE.1 .AND. M.LE.MMAX) THEN
*        Input the nodes (X,Y) and heights, F.
         DO 20 I = 1, M
            READ (NIN,*) X(I), Y(I), F(I)
   20    CONTINUE
*        Compute the nodal function coefficients.
         RNQ = 0.0D0
         NQ = 0
         IFAIL = 0
*
         CALL E01SEF(M,X,Y,F,RNW,RNQ,NW,NQ,FNODES,MINNQ,WRK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99997) '    RNW =', RNW, '  RNQ =', RNQ,
     +     '  MINNQ =', MINNQ
         WRITE (NOUT,*)
*        Evaluate the interpolant on a rectangular grid at NX*NY points
*        over the domain (XLO to XHI) x (YLO to YHI).
         READ (NIN,*) NX, XLO, XHI
         READ (NIN,*) NY, YLO, YHI
         IF (NX.LE.NMAX .AND. NY.LE.NMAX) THEN
            DO 40 I = 1, NX
               PX(I) = (DBLE(NX-I)/(NX-1))*XLO + (DBLE(I-1)/(NX-1))*XHI
   40       CONTINUE
            DO 60 I = 1, NY
               PY(I) = (DBLE(NY-I)/(NY-1))*YLO + (DBLE(I-1)/(NY-1))*YHI
   60       CONTINUE
            WRITE (NOUT,99999) '          X', (PX(I),I=1,NX)
            WRITE (NOUT,*) '     Y'
            DO 100 I = NY, 1, -1
               DO 80 J = 1, NX
                  IFAIL = 0
*
                  CALL E01SFF(M,X,Y,F,RNW,FNODES,PX(J),PY(I),PF(J),
     +                        IFAIL)
*
   80          CONTINUE
               WRITE (NOUT,99998) PY(I), (PF(J),J=1,NX)
  100       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,7F8.2)
99998 FORMAT (1X,F8.2,3X,7F8.2)
99997 FORMAT (1X,A,F8.2,A,F8.2,A,I3)
      END
