      SUBROUTINE D01BAZ(A,B,ITYPE,NPTS,WEIGHT,ABSCIS,IFAIL)
C     MARK 7 RELEASE. NAG COPYRIGHT 1978.
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C     RETURNS WEIGHTS AND PIVOTS FOR ONE GAUSS-LEGENDRE FORMULA IF
C     STORED
C     IFAIL = 1 - THE NPTS RULE IS NOT AMONG THOSE STORED
C     ( WEIGHT,ABSCIS EVALUATED FOR LARGEST VALID NPTS LESS THAN
C     REQUESTED VALUE)
C
C     THE WEIGHTS AND ABSCISSAE RETURNED DEPEND ON A AND B.
C     THOSE STORED ARE WEIGHTS AND ABSCISSAE FOR A=-1,B=+1
C     THOSE RETURNED FOR GENERAL (A,B) ARE RELATED TO THOSE STORED
C     BY
C     W(A,B) = 0.5 * (B-A) * W(-1,+1)
C     X(A,B) = 0.5 * (B-A) * X(-1,+1) + 0.5 * (A+B)
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  A, B
      INTEGER           IFAIL, ITYPE, NPTS
C     .. Array Arguments ..
      DOUBLE PRECISION  ABSCIS(NPTS), WEIGHT(NPTS)
C     .. Local Scalars ..
      DOUBLE PRECISION  HFRNGE, PNTMID
      INTEGER           I, IIJJ, N, NL, NN, NPTSA
C     .. Local Arrays ..
      DOUBLE PRECISION  ABST(136), WTST(136)
      INTEGER           NSTOR(16)
C     .. Data statements ..
      DATA              WTST(1), WTST(2), WTST(3), WTST(4),
     *                  WTST(5)/0.200000000000000000000000000000D1,
     *                  0.100000000000000000000000000000D1,
     *                  0.555555555555555555555555555555D0,
     *                  0.888888888888888888888888888888D0,
     *                  0.347854845137453857373063949221D0/
      DATA              WTST(6), WTST(7), WTST(08), WTST(09),
     *                  WTST(10)/0.652145154862546142626936050778D0,
     *                  0.236926885056189087514264040719D0,
     *                  0.478628670499366468041291514835D0,
     *                  0.568888888888888888888888888888D0,
     *                  0.171324492379170345040296142172D0/
      DATA              WTST(11), WTST(12), WTST(13), WTST(14),
     *                  WTST(15)/0.360761573048138607569833513837D0,
     *                  0.467913934572691047389870343989D0,
     *                  0.101228536290376259152531354309D0,
     *                  0.222381034453374470544355994426D0,
     *                  0.313706645877887287337962201986D0/
      DATA              WTST(16), WTST(17), WTST(18), WTST(19),
     *                  WTST(20)/0.362683783378361982965150449277D0,
     *                  0.666713443086881375935688098933D-1,
     *                  0.149451349150580593145776339657D0,
     *                  0.219086362515982043995534934228D0,
     *                  0.269266719309996355091226921569D0/
      DATA              WTST(21), WTST(22), WTST(23), WTST(24),
     *                  WTST(25)/0.295524224714752870173892994651D0,
     *                  0.471753363865118271946159614850D-1,
     *                  0.106939325995318430960254718193D0,
     *                  0.160078328543346226334652529543D0,
     *                  0.203167426723065921749064455809D0/
      DATA              WTST(26), WTST(27), WTST(28), WTST(29),
     *                  WTST(30)/0.233492536538354808760849898924D0,
     *                  0.249147045813402785000562436042D0,
     *                  0.351194603317518630318328761381D-1,
     *                  0.801580871597602098056332770628D-1,
     *                  0.121518570687903184689414809072D0/
      DATA              WTST(31), WTST(32), WTST(33), WTST(34),
     *                  WTST(35)/0.157203167158193534569601938623D0,
     *                  0.185538397477937813741716590125D0,
     *                  0.205198463721295603965924065661D0,
     *                  0.215263853463157790195876443316D0,
     *                  0.271524594117540948517805724560D-1/
      DATA              WTST(36), WTST(37), WTST(38), WTST(39),
     *                  WTST(40)/0.622535239386478928628438369943D-1,
     *                  0.951585116824927848099251076022D-1,
     *                  0.124628971255533872052476282192D0,
     *                  0.149595988816576732081501730547D0,
     *                  0.169156519395002538189312079030D0/
      DATA              WTST(41), WTST(42), WTST(43), WTST(44),
     *                  WTST(45)/0.182603415044923588866763667969D0,
     *                  0.189450610455068496285396723208D0,
     *                  0.176140071391521183118619623518D-1,
     *                  0.406014298003869413310399522749D-1,
     *                  0.626720483341090635695065351870D-1/
      DATA              WTST(46), WTST(47), WTST(48), WTST(49),
     *                  WTST(50)/0.832767415767047487247581432220D-1,
     *                  0.101930119817240435036750135480D0,
     *                  0.118194531961518417312377377711D0,
     *                  0.131688638449176626898494499748D0,
     *                  0.142096109318382051329298325067D0/
      DATA              WTST(51), WTST(52), WTST(53), WTST(54),
     *                  WTST(55)/0.149172986472603746787828737001D0,
     *                  0.152753387130725850698084331955D0,
     *                  0.123412297999871995468056670700D-1,
     *                  0.285313886289336631813078159518D-1,
     *                  0.442774388174198061686027482113D-1/
      DATA              WTST(56), WTST(57), WTST(58), WTST(59),
     *                  WTST(60)/0.592985849154367807463677585001D-1,
     *                  0.733464814110803057340336152531D-1,
     *                  0.861901615319532759171852029837D-1,
     *                  0.976186521041138882698806644642D-1,
     *                  0.107444270115965634782577342446D0/
      DATA              WTST(61), WTST(62), WTST(63), WTST(64),
     *                  WTST(65)/0.115505668053725601353344483906D0,
     *                  0.121670472927803391204463153476D0,
     *                  0.125837456346828296121375382511D0,
     *                  0.127938195346752156974056165224D0,
     *                  0.701861000947009660040706373885D-2/
      DATA              WTST(66), WTST(67), WTST(68), WTST(69),
     *                  WTST(70)/0.162743947309056706051705622063D-1,
     *                  0.253920653092620594557525897892D-1,
     *                  0.342738629130214331026877322523D-1,
     *                  0.428358980222266806568786466061D-1,
     *                  0.509980592623761761961632446895D-1/
      DATA              WTST(71), WTST(72), WTST(73), WTST(74),
     *                  WTST(75)/0.586840934785355471452836373001D-1,
     *                  0.658222227763618468376500637069D-1,
     *                  0.723457941088485062253993564784D-1,
     *                  0.781938957870703064717409188283D-1,
     *                  0.833119242269467552221990746043D-1/
      DATA              WTST(76), WTST(77), WTST(78), WTST(79),
     *                  WTST(80)/0.876520930044038111427714627518D-1,
     *                  0.911738786957638847128685771116D-1,
     *                  0.938443990808045656391802376681D-1,
     *                  0.956387200792748594190820022041D-1,
     *                  0.965400885147278005667648300635D-1/
      DATA              WTST(81), WTST(82), WTST(83), WTST(84),
     *                  WTST(85)/0.315334605230583863267731154389D-2,
     *                  0.732755390127626210238397962178D-2,
     *                  0.114772345792345394895926676090D-1,
     *                  0.155793157229438487281769558344D-1,
     *                  0.196161604573555278144607196522D-1/
      DATA              WTST(86), WTST(87), WTST(88), WTST(89),
     *                  WTST(90)/0.235707608393243791405193013784D-1,
     *                  0.274265097083569482000738362625D-1,
     *                  0.311672278327980889020657568463D-1,
     *                  0.347772225647704388925485859638D-1,
     *                  0.382413510658307063172172565237D-1/
      DATA              WTST(91), WTST(92), WTST(93), WTST(94),
     *                  WTST(95)/0.415450829434647492140588223610D-1,
     *                  0.446745608566942804194485871258D-1,
     *                  0.476166584924904748259066234789D-1,
     *                  0.503590355538544749578076190878D-1,
     *                  0.528901894851936670955050562646D-1/
      DATA              WTST(96), WTST(97), WTST(98), WTST(99),
     *                  WTST(100)/0.551995036999841628682034951916D-1,
     *                  0.572772921004032157051502346847D-1,
     *                  0.591148396983956357464748174335D-1,
     *                  0.607044391658938800529692320278D-1,
     *                  0.620394231598926639041977841375D-1/
      DATA              WTST(101), WTST(102), WTST(103), WTST(104),
     *                  WTST(105)/0.631141922862540256571260227502D-1,
     *                  0.639242385846481866239062018255D-1,
     *                  0.644661644359500822065041936577D-1,
     *                  0.647376968126839225030249387365D-1,
     *                  0.178328072169643294729607914497D-2/
      DATA              WTST(106), WTST(107), WTST(108), WTST(109),
     *                  WTST(110)/0.414703326056246763528753572855D-2,
     *                  0.650445796897836285611736039998D-2,
     *                  0.884675982636394772303091465973D-2,
     *                  0.111681394601311288185904930192D-1,
     *                  0.134630478967186425980607666859D-1/
      DATA              WTST(111), WTST(112), WTST(113), WTST(114),
     *                  WTST(115)/0.157260304760247193219659952975D-1,
     *                  0.179517157756973430850453020011D-1,
     *                  0.201348231535302093723403167285D-1,
     *                  0.222701738083832541592983303841D-1,
     *                  0.243527025687108733381775504090D-1/
      DATA              WTST(116), WTST(117), WTST(118), WTST(119),
     *                  WTST(120)/0.263774697150546586716917926252D-1,
     *                  0.283396726142594832275113052002D-1,
     *                  0.302346570724024788679740598195D-1,
     *                  0.320579283548515535854675043478D-1,
     *                  0.338051618371416093915654821107D-1/
      DATA              WTST(121), WTST(122), WTST(123), WTST(124),
     *                  WTST(125)/0.354722132568823838106931467152D-1,
     *                  0.370551285402400460404151018095D-1,
     *                  0.385501531786156291289624969468D-1,
     *                  0.399537411327203413866569261283D-1,
     *                  0.412625632426235286101562974736D-1/
      DATA              WTST(126), WTST(127), WTST(128), WTST(129),
     *                  WTST(130)/0.424735151236535890073397679088D-1,
     *                  0.435837245293234533768278609737D-1,
     *                  0.445905581637565630601347100309D-1,
     *                  0.454916279274181444797709969712D-1,
     *                  0.462847965813144172959532492322D-1/
      DATA              WTST(131), WTST(132), WTST(133), WTST(134),
     *                  WTST(135)/0.469681828162100173253262857545D-1,
     *                  0.475401657148303086622822069442D-1,
     *                  0.479993885964583077281261798713D-1,
     *                  0.483447622348029571697695271580D-1,
     *                  0.485754674415034269347990667839D-1/
      DATA              WTST(136)/0.486909570091397203833653907347D-1/
      DATA              ABST(1), ABST(2), ABST(3), ABST(4),
     *                  ABST(5)/0.000000000000000000000000000000D0,
     *                  0.577350269189625764509148780501D0,
     *                  0.774596669241483377035853079956D0,
     *                  0.000000000000000000000000000000D0,
     *                  0.861136311594052575223946488892D0/
      DATA              ABST(6), ABST(7), ABST(8), ABST(9),
     *                  ABST(10)/0.339981043584856264802665759103D0,
     *                  0.906179845938663992797626878299D0,
     *                  0.538469310105683091036314420700D0,
     *                  0.000000000000000000000000000000D0,
     *                  0.932469514203152027812301554493D0/
      DATA              ABST(11), ABST(12), ABST(13), ABST(14),
     *                  ABST(15)/0.661209386466264513661399595019D0,
     *                  0.238619186083196908630501721680D0,
     *                  0.960289856497536231683560868569D0,
     *                  0.796666477413626739591553936475D0,
     *                  0.525532409916328985817739049189D0/
      DATA              ABST(16), ABST(17), ABST(18), ABST(19),
     *                  ABST(20)/0.183434642495649804939476142360D0,
     *                  0.973906528517171720077964012084D0,
     *                  0.865063366688984510732096688423D0,
     *                  0.679409568299024406234327365114D0,
     *                  0.433395394129247190799265943165D0/
      DATA              ABST(21), ABST(22), ABST(23), ABST(24),
     *                  ABST(25)/0.148874338981631210884826001129D0,
     *                  0.981560634246719250690549090149D0,
     *                  0.904117256370474856678465866119D0,
     *                  0.769902674194304687036893833212D0,
     *                  0.587317954286617447296702418940D0/
      DATA              ABST(26), ABST(27), ABST(28), ABST(29),
     *                  ABST(30)/0.367831498998180193752691536643D0,
     *                  0.125233408511468915472441369463D0,
     *                  0.986283808696812338841597266704D0,
     *                  0.928434883663573517336391139377D0,
     *                  0.827201315069764993189794742650D0/
      DATA              ABST(31), ABST(32), ABST(33), ABST(34),
     *                  ABST(35)/0.687292904811685470148019803019D0,
     *                  0.515248636358154091965290718551D0,
     *                  0.319112368927889760435671824168D0,
     *                  0.108054948707343662066244650219D0,
     *                  0.989400934991649932596154173450D0/
      DATA              ABST(36), ABST(37), ABST(38), ABST(39),
     *                  ABST(40)/0.944575023073232576077988415534D0,
     *                  0.865631202387831743880467897712D0,
     *                  0.755404408355003033895101194847D0,
     *                  0.617876244402643748446671764048D0,
     *                  0.458016777657227386342419442983D0/
      DATA              ABST(41), ABST(42), ABST(43), ABST(44),
     *                  ABST(45)/0.281603550779258913230460501460D0,
     *                  0.950125098376374401853193354249D-1,
     *                  0.993128599185094924786122388471D0,
     *                  0.963971927277913791267666131197D0,
     *                  0.912234428251325905867752441203D0/
      DATA              ABST(46), ABST(47), ABST(48), ABST(49),
     *                  ABST(50)/0.839116971822218823394529061701D0,
     *                  0.746331906460150792614305070355D0,
     *                  0.636053680726515025452836696226D0,
     *                  0.510867001950827098004364050955D0,
     *                  0.373706088715419560672548177024D0/
      DATA              ABST(51), ABST(52), ABST(53), ABST(54),
     *                  ABST(55)/0.227785851141645078080496195368D0,
     *                  0.765265211334973337546404093988D-1,
     *                  0.995187219997021360179997409700D0,
     *                  0.974728555971309498198391993008D0,
     *                  0.938274552002732758523649001708D0/
      DATA              ABST(56), ABST(57), ABST(58), ABST(59),
     *                  ABST(60)/0.886415527004401034213154341982D0,
     *                  0.820001985973902921953949872669D0,
     *                  0.740124191578554364243828103099D0,
     *                  0.648093651936975569252495786910D0,
     *                  0.545421471388839535658375617218D0/
      DATA              ABST(61), ABST(62), ABST(63), ABST(64),
     *                  ABST(65)/0.433793507626045138487084231913D0,
     *                  0.315042679696163374386793291319D0,
     *                  0.191118867473616309158639820757D0,
     *                  0.640568928626056260850430826247D-1,
     *                  0.997263861849481563544981128665D0/
      DATA              ABST(66), ABST(67), ABST(68), ABST(69),
     *                  ABST(70)/0.985611511545268335400175044630D0,
     *                  0.964762255587506430773811928118D0,
     *                  0.934906075937739689170919134835D0,
     *                  0.896321155766052123965307243719D0,
     *                  0.849367613732569970133693004967D0/
      DATA              ABST(71), ABST(72), ABST(73), ABST(74),
     *                  ABST(75)/0.794483795967942406963097298970D0,
     *                  0.732182118740289680387426665091D0,
     *                  0.663044266930215200975115168663D0,
     *                  0.587715757240762329040745476401D0,
     *                  0.506899908932229390023747474377D0/
      DATA              ABST(76), ABST(77), ABST(78), ABST(79),
     *                  ABST(80)/0.421351276130635345364119436172D0,
     *                  0.331868602282127649779916805730D0,
     *                  0.239287362252137074544603209165D0,
     *                  0.144471961582796493485186373598D0,
     *                  0.483076656877383162348125704405D-1/
      DATA              ABST(81), ABST(82), ABST(83), ABST(84),
     *                  ABST(85)/0.998771007252426118600541491563D0,
     *                  0.993530172266350757547928750849D0,
     *                  0.984124583722826857744583600026D0,
     *                  0.970591592546247250461411983800D0,
     *                  0.952987703160430860722960666025D0/
      DATA              ABST(86), ABST(87), ABST(88), ABST(89),
     *                  ABST(90)/0.931386690706554333114174380101D0,
     *                  0.905879136715569672822074835671D0,
     *                  0.876572020274247885905693554805D0,
     *                  0.843588261624393530711089844519D0,
     *                  0.807066204029442627082553043024D0/
      DATA              ABST(91), ABST(92), ABST(93), ABST(94),
     *                  ABST(95)/0.767159032515740339253855437522D0,
     *                  0.724034130923814654674482233493D0,
     *                  0.677872379632663905211851280675D0,
     *                  0.628867396776513623995164933069D0,
     *                  0.577224726083972703817809238540D0/
      DATA              ABST(96), ABST(97), ABST(98), ABST(99),
     *                  ABST(100)/0.523160974722233033678225869137D0,
     *                  0.466902904750958404544928861650D0,
     *                  0.408686481990716729916225495814D0,
     *                  0.348755886292160738159817937270D0,
     *                  0.287362487355455576735886461316D0/
      DATA              ABST(101), ABST(102), ABST(103), ABST(104),
     *                  ABST(105)/0.224763790394689061224865440174D0,
     *                  0.161222356068891718056437390783D0,
     *                  0.970046992094626989300539558536D-1,
     *                  0.323801709628693620333222431521D-1,
     *                  0.999305041735772139456905624345D0/
      DATA              ABST(106), ABST(107), ABST(108), ABST(109),
     *                  ABST(110)/0.996340116771955279346924500676D0,
     *                  0.991013371476744320739382383443D0,
     *                  0.983336253884625956931299302156D0,
     *                  0.973326827789910963741853507352D0,
     *                  0.961008799652053718918614121897D0/
      DATA              ABST(111), ABST(112), ABST(113), ABST(114),
     *                  ABST(115)/0.946411374858402816062481491347D0,
     *                  0.929569172131939575821490154559D0,
     *                  0.910522137078502805756380668008D0,
     *                  0.889315445995114105853404038272D0,
     *                  0.865999398154092819760783385070D0/
      DATA              ABST(116), ABST(117), ABST(118), ABST(119),
     *                  ABST(120)/0.840629296252580362751691544695D0,
     *                  0.813265315122797559741923338086D0,
     *                  0.783972358943341407610220525213D0,
     *                  0.752819907260531896611863774885D0,
     *                  0.719881850171610826848940217831D0/
      DATA              ABST(121), ABST(122), ABST(123), ABST(124),
     *                  ABST(125)/0.685236313054233242563558371031D0,
     *                  0.648965471254657339857761231993D0,
     *                  0.611155355172393250248852971018D0,
     *                  0.571895646202634034283878116659D0,
     *                  0.531279464019894545658013903544D0/
      DATA              ABST(126), ABST(127), ABST(128), ABST(129),
     *                  ABST(130)/0.489403145707052957478526307021D0,
     *                  0.446366017253464087984947714758D0,
     *                  0.402270157963991603695766771260D0,
     *                  0.357220158337668115950442615046D0,
     *                  0.311322871990210956157512698560D0/
      DATA              ABST(131), ABST(132), ABST(133), ABST(134),
     *                  ABST(135)/0.264687162208767416373964172510D0,
     *                  0.217423643740007084149648748988D0,
     *                  0.169644420423992818037313629748D0,
     *                  0.121462819296120554470376463492D0,
     *                  0.729931217877990394495429419403D-1/
      DATA              ABST(136)/0.243502926634244325089558428537D-1/
      DATA              NSTOR(1), NSTOR(2), NSTOR(3), NSTOR(4)/1, 2, 3,
     *                  4/
      DATA              NSTOR(5), NSTOR(6), NSTOR(7), NSTOR(8)/5, 6, 8,
     *                  10/
      DATA              NSTOR(9), NSTOR(10), NSTOR(11), NSTOR(12)/12,
     *                  14, 16, 20/
      DATA              NSTOR(13), NSTOR(14), NSTOR(15), NSTOR(16)/24,
     *                  32, 48, 64/
C     .. Executable Statements ..
      DO 20 I = 1, NPTS
         WEIGHT(I) = 0.0D0
         ABSCIS(I) = 0.0D0
   20 CONTINUE
      N = 0
      NPTSA = 0
      IFAIL = 0
      DO 40 I = 1, 16
         IF (NPTS.LT.NSTOR(I)) GO TO 60
         N = N + (NPTSA+1)/2
         NPTSA = NSTOR(I)
         IF (NPTS.EQ.NSTOR(I)) GO TO 80
   40 CONTINUE
   60 IFAIL = 1
   80 HFRNGE = 0.5D0*(B-A)
      PNTMID = 0.5D0*(A+B)
      NL = NPTSA/2
      IF (NL.LT.1) GO TO 120
      DO 100 NN = 1, NL
         N = N + 1
         IIJJ = NPTSA + 1 - NN
         ABSCIS(NN) = HFRNGE*ABST(N) + PNTMID
         WEIGHT(NN) = HFRNGE*WTST(N)
         ABSCIS(IIJJ) = -HFRNGE*ABST(N) + PNTMID
         WEIGHT(IIJJ) = HFRNGE*WTST(N)
  100 CONTINUE
  120 IF (NPTSA.LE.(NL+NL)) GO TO 140
      N = N + 1
      ABSCIS(NL+1) = HFRNGE*ABST(N) + PNTMID
      WEIGHT(NL+1) = HFRNGE*WTST(N)
  140 RETURN
      END
