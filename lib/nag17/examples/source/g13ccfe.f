*     G13CCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NXYG, NCMAX
      PARAMETER        (NXYG=350,NCMAX=50)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION PXY
      INTEGER          I, IC, IFAIL, II, IS, IW, KC, L, MTXY, MW, NC,
     +                 NG, NXY
*     .. Local Arrays ..
      DOUBLE PRECISION CXY(NCMAX), CYX(NCMAX), XG(NXYG), YG(NXYG)
*     .. External Subroutines ..
      EXTERNAL         G13CCF
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13CCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NXY, NC, IC
      IF (NXY.GT.0 .AND. NXY.LE.NXYG .AND. NC.GT.0 .AND. NC.LE.NCMAX)
     +    THEN
         IF (IC.EQ.0) THEN
            READ (NIN,*) (XG(I),I=1,NXY)
            READ (NIN,*) (YG(I),I=1,NXY)
         ELSE
            READ (NIN,*) (CXY(I),I=1,NC)
            READ (NIN,*) (CYX(I),I=1,NC)
         END IF
*        Set parameters for call to G13CCF
*        Mean correction and 10 percent taper
         MTXY = 1
         PXY = 0.1D0
*        Parzen window and zero covariance at lag 35
         IW = 4
         MW = 35
*        Alignment shift of 3, 50 covariances to be calculated
         IS = 3
         KC = 350
         L = 80
         IFAIL = 0
*
         CALL G13CCF(NXY,MTXY,PXY,IW,MW,IS,IC,NC,CXY,CYX,KC,L,NXYG,XG,
     +               YG,NG,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) '                  Returned cross covariances'
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     + 'Lag     XY       YX   Lag     XY       YX   Lag     XY       YX'
         DO 20 I = 1, NC, 3
            WRITE (NOUT,99999) (II-1,CXY(II),CYX(II),II=I,MIN(I+2,NC))
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) '                      Returned sample spectrum'
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +'       Real  Imaginary       Real  Imaginary       Real  Imaginar
     +y'
         WRITE (NOUT,*)
     +'Lag    part     part  Lag    part     part  Lag    part     part'
         DO 40 I = 1, NG, 3
            WRITE (NOUT,99999) (II-1,XG(II),YG(II),II=I,MIN(I+2,NG))
   40    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,I3,2F9.4,I4,2F9.4,I4,2F9.4)
      END
