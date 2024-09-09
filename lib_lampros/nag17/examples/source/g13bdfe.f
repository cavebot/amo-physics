*     G13BDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NLMAX, NWDSMX, IWAMAX
      PARAMETER        (NLMAX=10,NWDSMX=5,IWAMAX=20)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION R0, S
      INTEGER          I, IFAIL, IWA, NL, NWDS
*     .. Local Arrays ..
      DOUBLE PRECISION R(NLMAX), WA(IWAMAX), WDS(NWDSMX)
      INTEGER          ISF(2), NNA(3)
*     .. External Subroutines ..
      EXTERNAL         G13BDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13BDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NL
      READ (NIN,*) R0
      IF (NL.GT.0 .AND. NL.LE.NLMAX) THEN
         READ (NIN,*) (R(I),I=1,NL)
         READ (NIN,*) (NNA(I),I=1,3)
         READ (NIN,*) S
         NWDS = NNA(2) + NNA(3) + 1
         IWA = NNA(3)*(NNA(3)+1)
         IF (NWDS.LE.NWDSMX .AND. IWA.LE.IWAMAX) THEN
            IFAIL = 0
*
            CALL G13BDF(R0,R,NL,NNA,S,NWDS,WA,IWA,WDS,ISF,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Success/failure indicator', ISF(1),
     +        ISF(2)
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Transfer function model B, Q, P =',
     +        (NNA(I),I=1,3)
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Parameter initial estimates'
            WRITE (NOUT,99998) (WDS(I),I=1,NWDS)
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,3I4)
99998 FORMAT (1X,4F10.4)
      END
