*     G03EHF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX, LDC
      PARAMETER        (NMAX=10,MMAX=10,LDC=100)
*     .. Local Scalars ..
      DOUBLE PRECISION DMIN, DSTEP
      INTEGER          I, IFAIL, J, LDX, M, METHOD, N, NSYM
      CHARACTER        DIST, SCALE, UPDATE
*     .. Local Arrays ..
      DOUBLE PRECISION CD(NMAX-1), D(NMAX*(NMAX-1)/2), DORD(NMAX),
     +                 S(MMAX), X(NMAX,MMAX)
      INTEGER          ILC(NMAX-1), IORD(NMAX), ISX(MMAX), IUC(NMAX-1),
     +                 IWK(2*NMAX)
      CHARACTER*50     C(LDC)
*     .. External Subroutines ..
      EXTERNAL         G03EAF, G03ECF, G03EHF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G03EHF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M
      IF (N.LE.NMAX .AND. M.LE.MMAX) THEN
         READ (NIN,*) METHOD
         READ (NIN,*) UPDATE, DIST, SCALE
         DO 20 J = 1, N
            READ (NIN,*) (X(J,I),I=1,M)
   20    CONTINUE
         READ (NIN,*) (ISX(I),I=1,M)
         READ (NIN,*) (S(I),I=1,M)
         READ (NIN,*) DMIN, DSTEP, NSYM
*
*        Compute the distance matrix
*
         IFAIL = 0
         LDX = NMAX
*
         CALL G03EAF(UPDATE,DIST,SCALE,N,M,X,LDX,ISX,S,D,IFAIL)
*
*        Perform clustering
*
         IFAIL = 0
*
         CALL G03ECF(METHOD,N,D,ILC,IUC,CD,IORD,DORD,IWK,IFAIL)
*
*        Produce dendrograms
*
         IFAIL = 0
*
         CALL G03EHF('E',N,DORD,DMIN,DSTEP,NSYM,C,LDC,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Dendrogram, Orientation East'
         DO 40 I = 1, N
            WRITE (NOUT,*) C(I)
   40    CONTINUE
*
         READ (NIN,*) DMIN, DSTEP, NSYM
         IFAIL = 0
*
         CALL G03EHF('S',N,DORD,DMIN,DSTEP,NSYM,C,LDC,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Dendrogram, Orientation South'
         DO 60 I = 1, NSYM
            WRITE (NOUT,*) C(I)
   60    CONTINUE
      END IF
      STOP
*
      END
