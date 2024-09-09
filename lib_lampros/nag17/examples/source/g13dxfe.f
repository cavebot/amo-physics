*     G13DXF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          KMAX, IPMAX
      PARAMETER        (KMAX=6,IPMAX=3)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, IP, K, NPAR
*     .. Local Arrays ..
      DOUBLE PRECISION PAR(KMAX*KMAX*IPMAX), RI(KMAX*IPMAX),
     +                 RMOD(KMAX*IPMAX), RR(KMAX*IPMAX),
     +                 WORK(KMAX*KMAX*IPMAX*IPMAX)
      INTEGER          IW(KMAX*IPMAX)
*     .. External Subroutines ..
      EXTERNAL         G13DXF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13DXF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) K, IP
      IF (K.GT.0 .AND. K.LE.KMAX .AND. IP.GT.0 .AND. IP.LE.IPMAX) THEN
*        Read the AR (or MA) parameters
         NPAR = IP*K*K
         READ (NIN,*) (PAR(I),I=1,NPAR)
         IFAIL = 0
*
         CALL G13DXF(K,IP,PAR,RR,RI,RMOD,WORK,IW,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) '        Eigenvalues       Moduli'
         WRITE (NOUT,*) '        -----------       ------'
*
         DO 20 I = 1, K*IP
            IF (RI(I).GE.0.0D0) THEN
               WRITE (NOUT,99999) RR(I), RI(I), RMOD(I)
            ELSE
               WRITE (NOUT,99998) RR(I), -RI(I), RMOD(I)
            END IF
   20    CONTINUE
      ELSE
         WRITE (NOUT,*) ' Either K or IP is out of range'
      END IF
      STOP
*
99999 FORMAT (' ',F10.3,'  + ',F6.3,' i  ',F8.3)
99998 FORMAT (' ',F10.3,'  - ',F6.3,' i  ',F8.3)
      END
