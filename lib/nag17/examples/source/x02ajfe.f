*     X02AJF Example Program Text
*     Mark 17 Revised.  NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. External Functions ..
      DOUBLE PRECISION X02AHF, X02AJF, X02AKF, X02ALF, X02AMF, X02ANF
      INTEGER          X02BBF, X02BEF, X02BHF, X02BJF, X02BKF, X02BLF
      LOGICAL          X02DAF, X02DJF
      EXTERNAL         X02AHF, X02AJF, X02AKF, X02ALF, X02AMF, X02ANF,
     +                 X02BBF, X02BEF, X02BHF, X02BJF, X02BKF, X02BLF,
     +                 X02DAF, X02DJF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X02AJF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '(results are machine-dependent)'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'The basic parameters of the model'
      WRITE (NOUT,*)
      WRITE (NOUT,99999) ' X02BHF = ', X02BHF(),
     +  '  (the model parameter B)'
      WRITE (NOUT,99999) ' X02BJF = ', X02BJF(),
     +  '  (the model parameter P)'
      WRITE (NOUT,99999) ' X02BKF = ', X02BKF(),
     +  '  (the model parameter EMIN)'
      WRITE (NOUT,99999) ' X02BLF = ', X02BLF(),
     +  '  (the model parameter EMAX)'
      WRITE (NOUT,99998) ' X02DJF = ', X02DJF(),
     +  '  (the model parameter ROUNDS)'
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  'Derived parameters of floating-point arithmetic'
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' X02AJF = ', X02AJF(),
     +  '  (the machine precision)'
      WRITE (NOUT,*) ' X02AKF = ', X02AKF(),
     +  '  (the smallest positive model number)'
      WRITE (NOUT,*) ' X02ALF = ', X02ALF(),
     +  '  (the largest positive model number)'
      WRITE (NOUT,*) ' X02AMF = ', X02AMF(),
     +  '  (the real safe range parameter)'
      WRITE (NOUT,*) ' X02ANF = ', X02ANF(),
     +  '  (the complex safe range parameter)'
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  'Parameters of other aspects of the computing environment'
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' X02AHF = ', X02AHF(0.0D0),
     +  '  (largest argument for SIN and COS)'
      WRITE (NOUT,99997) ' X02BBF = ', X02BBF(0.0D0),
     +  '  (largest positive integer)'
      WRITE (NOUT,99997) ' X02BEF = ', X02BEF(0.0D0),
     +  '  (precision in decimal digits)'
      WRITE (NOUT,99996) ' X02DAF = ', X02DAF(0.0D0),
     +  '  (indicates how underflow is handled)'
      STOP
*
99999 FORMAT (1X,A,I7,A)
99998 FORMAT (1X,A,L7,A)
99997 FORMAT (1X,A,I20,A)
99996 FORMAT (1X,A,L20,A)
      END
