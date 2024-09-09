*     G13CDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NXYMAX, L, KC
      PARAMETER        (NXYMAX=300,L=80,KC=8*L)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION PW, PXY
      INTEGER          I, IFAIL, IS, J, MTXY, MW, NG, NXY
*     .. Local Arrays ..
      DOUBLE PRECISION XG(KC), YG(KC)
*     .. External Subroutines ..
      EXTERNAL         G13CDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13CDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NXY
      IF (NXY.GT.0 .AND. NXY.LE.NXYMAX) THEN
         READ (NIN,*) (XG(I),I=1,NXY)
         READ (NIN,*) (YG(I),I=1,NXY)
*        Set parameters for call to G13CDF
*        Mean correction and 10 percent taper
         MTXY = 1
         PXY = 0.1D0
*        Window shape parameter and zero covariance at lag 16
         PW = 0.5D0
         MW = 16
*        Alignment shift of 3
         IS = 3
         IFAIL = 0
*
         CALL G13CDF(NXY,MTXY,PXY,MW,IS,PW,L,KC,XG,YG,NG,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) '                      Returned sample spectrum'
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +'      Real  Imaginary       Real  Imaginary       Real  Imaginary
     +'
         WRITE (NOUT,*)
     + '      part     part         part     part         part     part'
         WRITE (NOUT,99999) (J,XG(J),YG(J),J=1,NG)
      END IF
      STOP
*
99999 FORMAT (1X,I3,F8.4,F9.4,I5,F8.4,F9.4,I5,F8.4,F9.4)
      END
