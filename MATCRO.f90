PROGRAM MATCRO

  IMPLICIT NONE
  include "./param.inc"

  !! !!!!!!!!!!!!!!!!!!!!!!! !!
  !! [[ SETTING PARAMETER ]] !!
  !! !!!!!!!!!!!!!!!!!!!!!!! !!

  !! [TIME and REGION] !!
  INTEGER STYR      ! Start year [Year]
  INTEGER ENYR      ! End year   [Year]
  INTEGER STDOY     ! Start DOY  [day]
  INTEGER ENDOY     ! End DOY    [day]

  INTEGER TRES      ! Time resolution     [Second]
  INTEGER clmTRES  ! Time resolution of climate forcing [Second]


  REAL*8  WEST      ! West edge  [degree]
  REAL*8  EAST      ! East edge  [degree]
  REAL*8  NORTH     ! North edge [degree]
  REAL*8  SOUTH     ! South edge [degree]
  REAL*8  WERES     ! Resolution of west to east [degree]
  REAL*8  NSRES     ! Resolution of north to south [degree]

  !! [CROP NAME and FILE] !!
  CHARACTER*200 CRP_NAME   ! Crop name (Rice, Wheat, Maize, or Soybeans)
  CHARACTER*200 CRP_FILE   ! Input file name for crop parameter


  !! [CO2 FILE] !!
  CHARACTER*200 CO2_HEAD   ! Head of input file name for atmospheric CO2
  CHARACTER*200 CO2_FOOT   ! Foot of input file name for atmospheric CO2
  CHARACTER*5   CO2_YEAR   ! Specify YYYY if you want to fix year for CO2, otherwise write nothing 

  !! [OUTPUT FILE] !!
  CHARACTER*200 YLD_HEAD   ! Output file name for yields of first growing season
  CHARACTER*200 LAI_HEAD   ! Output file name for yields of first growing season

  !! [OTHERs] !!
  REAL*8  WND_HGT          ! Hegith of input data for input [m]
  INTEGER NGRW             ! Number of growing season in a year

  !! [WEST EDGE of INPUTs (NetCDF)] !! [degree]
  REAL*8  TMP_WEST      
  REAL*8  TMX_WEST      
  REAL*8  TMN_WEST      
  REAL*8  PRC_WEST      
  REAL*8  RSD_WEST      
  REAL*8  SHM_WEST      
  REAL*8  WND_WEST      
  REAL*8  PRS_WEST      
  REAL*8  OZN_WEST      
  REAL*8  PLT1_WEST      
  REAL*8  GDHm1_WEST     
  REAL*8  SOIL_WEST     
  REAL*8  NFERT_WEST    
  REAL*8  Vsat_WEST    
  REAL*8  LND_WEST    

  !! [EAST EDGE of INPUTs (NetCDF) ] !! [degree]
  REAL*8  TMP_EAST      
  REAL*8  TMX_EAST      
  REAL*8  TMN_EAST      
  REAL*8  PRC_EAST      
  REAL*8  RSD_EAST      
  REAL*8  SHM_EAST      
  REAL*8  WND_EAST      
  REAL*8  PRS_EAST      
  REAL*8  OZN_EAST      
  REAL*8  PLT1_EAST      
  REAL*8  GDHm1_EAST     
  REAL*8  SOIL_EAST     
  REAL*8  NFERT_EAST    
  REAL*8  Vsat_EAST
  REAL*8  LND_EAST

  !! [NORTH EDGE of INPUTs (NetCDF) ] !! [degree]
  REAL*8  TMP_NORTH      
  REAL*8  TMX_NORTH      
  REAL*8  TMN_NORTH      
  REAL*8  PRC_NORTH      
  REAL*8  RSD_NORTH      
  REAL*8  SHM_NORTH      
  REAL*8  WND_NORTH      
  REAL*8  PRS_NORTH      
  REAL*8  OZN_NORTH      
  REAL*8  PLT1_NORTH      
  REAL*8  GDHm1_NORTH     
  REAL*8  SOIL_NORTH     
  REAL*8  NFERT_NORTH    
  REAL*8  Vsat_NORTH
  REAL*8  LND_NORTH

  !! [SOUTH EDGE of INPUTs (NetCDF) ] !! [degree]
  REAL*8  TMP_SOUTH      
  REAL*8  TMX_SOUTH      
  REAL*8  TMN_SOUTH      
  REAL*8  PRC_SOUTH      
  REAL*8  RSD_SOUTH      
  REAL*8  SHM_SOUTH      
  REAL*8  WND_SOUTH      
  REAL*8  PRS_SOUTH      
  REAL*8  OZN_SOUTH      
  REAL*8  PLT1_SOUTH      
  REAL*8  GDHm1_SOUTH     
  REAL*8  SOIL_SOUTH     
  REAL*8  NFERT_SOUTH    
  REAL*8  Vsat_SOUTH
  REAL*8  LND_SOUTH

  !! [RESOLUTION for WEST-EAST of INPUTs (NetCDF) ] !! [degree]
  REAL*8  TMP_WERES      
  REAL*8  TMX_WERES      
  REAL*8  TMN_WERES      
  REAL*8  PRC_WERES      
  REAL*8  RSD_WERES      
  REAL*8  SHM_WERES      
  REAL*8  WND_WERES      
  REAL*8  PRS_WERES      
  REAL*8  OZN_WERES      
  REAL*8  PLT1_WERES      
  REAL*8  GDHm1_WERES     
  REAL*8  SOIL_WERES     
  REAL*8  NFERT_WERES    
  REAL*8  Vsat_WERES
  REAL*8  LND_WERES

  !! [RESOLUTION for NORTH-SOUTH of INPUTs (NetCDF) ] !! [degree]
  REAL*8  TMP_NSRES      
  REAL*8  TMX_NSRES      
  REAL*8  TMN_NSRES      
  REAL*8  PRC_NSRES      
  REAL*8  RSD_NSRES      
  REAL*8  SHM_NSRES      
  REAL*8  WND_NSRES      
  REAL*8  PRS_NSRES      
  REAL*8  OZN_NSRES      
  REAL*8  PLT1_NSRES      
  REAL*8  GDHm1_NSRES     
  REAL*8  SOIL_NSRES     
  REAL*8  NFERT_NSRES    
  REAL*8  Vsat_NSRES
  REAL*8  LND_NSRES

  !! [DIRECTION for NORTH-SOUTH of INPUTs (NetCDF) ] !! "NS" means North to South; "SN" mean South to North
  CHARACTER*2  TMP_NSDIR      
  CHARACTER*2  TMX_NSDIR      
  CHARACTER*2  TMN_NSDIR      
  CHARACTER*2  PRC_NSDIR      
  CHARACTER*2  RSD_NSDIR      
  CHARACTER*2  SHM_NSDIR      
  CHARACTER*2  WND_NSDIR      
  CHARACTER*2  PRS_NSDIR      
  CHARACTER*2  OZN_NSDIR      
  CHARACTER*2  PLT1_NSDIR      
  CHARACTER*2  GDHm1_NSDIR     
  CHARACTER*2  SOIL_NSDIR     
  CHARACTER*2  NFERT_NSDIR    
  CHARACTER*2  Vsat_NSDIR    
  CHARACTER*2  LND_NSDIR    

  !! [MULTIPLIER of INPUTs (NetCDF) ] !! DAT * MUL + ADD
  REAL*8  TMP_MUL      
  REAL*8  TMX_MUL      
  REAL*8  TMN_MUL      
  REAL*8  PRC_MUL      
  REAL*8  RSD_MUL      
  REAL*8  SHM_MUL      
  REAL*8  WND_MUL      
  REAL*8  PRS_MUL      
  REAL*8  OZN_MUL      
  REAL*8  PLT1_MUL      
  REAL*8  GDHm1_MUL     
  REAL*8  SOIL_MUL     
  REAL*8  NFERT_MUL    
  REAL*8  Vsat_MUL    
  REAL*8  LND_MUL    

  !! [ADDITION of INPUTs (NetCDF) ] !! DAT * MUL + ADD
  REAL*8  TMP_ADD      
  REAL*8  TMX_ADD      
  REAL*8  TMN_ADD      
  REAL*8  PRC_ADD      
  REAL*8  RSD_ADD      
  REAL*8  SHM_ADD      
  REAL*8  WND_ADD      
  REAL*8  PRS_ADD      
  REAL*8  OZN_ADD      
  REAL*8  PLT1_ADD      
  REAL*8  GDHm1_ADD     
  REAL*8  SOIL_ADD     
  REAL*8  NFERT_ADD    
  REAL*8  Vsat_ADD    
  REAL*8  LND_ADD    

  !! [DATA FORM of INPUTs (NetCDF) ] !! "I" means Integer; "R" means Real
  CHARACTER*1  TMP_FORM      
  CHARACTER*1  TMX_FORM      
  CHARACTER*1  TMN_FORM      
  CHARACTER*1  PRC_FORM      
  CHARACTER*1  RSD_FORM      
  CHARACTER*1  SHM_FORM      
  CHARACTER*1  WND_FORM      
  CHARACTER*1  PRS_FORM      
  CHARACTER*1  OZN_FORM      
  CHARACTER*1  PLT1_FORM      
  CHARACTER*1  GDHm1_FORM     
  CHARACTER*1  SOIL_FORM     
  CHARACTER*1  NFERT_FORM    
  CHARACTER*1  Vsat_FORM    
  CHARACTER*1  LND_FORM    

  !! [DATA BYTE of INPUTs (NetCDF) ] !! "4" means 4 bytes; "8" means 8 bytes
  INTEGER  TMP_BYTE      
  INTEGER  TMX_BYTE      
  INTEGER  TMN_BYTE      
  INTEGER  PRC_BYTE      
  INTEGER  RSD_BYTE      
  INTEGER  SHM_BYTE      
  INTEGER  WND_BYTE      
  INTEGER  PRS_BYTE      
  INTEGER  OZN_BYTE      
  INTEGER  PLT1_BYTE      
  INTEGER  GDHm1_BYTE     
  INTEGER  SOIL_BYTE     
  INTEGER  NFERT_BYTE    
  INTEGER  Vsat_BYTE    
  INTEGER  LND_BYTE    

  !! [HEAD of INPUT FILE] !! 
  CHARACTER*200  TMP_HEAD      
  CHARACTER*200  TMX_HEAD      
  CHARACTER*200  TMN_HEAD      
  CHARACTER*200  PRC_HEAD      
  CHARACTER*200  RSD_HEAD      
  CHARACTER*200  SHM_HEAD      
  CHARACTER*200  WND_HEAD      
  CHARACTER*200  PRS_HEAD      
  CHARACTER*200  OZN_HEAD      
  CHARACTER*200  PLT1_HEAD      
  CHARACTER*200  GDHm1_HEAD     
  CHARACTER*200  SOIL_HEAD     
  CHARACTER*200  NFERT_HEAD    
  CHARACTER*200  Vsat_HEAD    
  CHARACTER*200  LND_HEAD    

  !! [FOOT of INPUT FILE] !! 
  CHARACTER*200  TMP_FOOT      
  CHARACTER*200  TMX_FOOT      
  CHARACTER*200  TMN_FOOT      
  CHARACTER*200  PRC_FOOT      
  CHARACTER*200  RSD_FOOT      
  CHARACTER*200  SHM_FOOT      
  CHARACTER*200  WND_FOOT      
  CHARACTER*200  PRS_FOOT      
  CHARACTER*200  OZN_FOOT      
  CHARACTER*200  PLT1_FOOT      
  CHARACTER*200  GDHm1_FOOT     
  CHARACTER*200  SOIL_FOOT     
  CHARACTER*200  NFERT_FOOT    
  CHARACTER*200  Vsat_FOOT    
  CHARACTER*200  LND_FOOT    

  !! [VAR NAME of INPUTs (NetCDF) ] !! 
  CHARACTER*200  TMP_VNM      
  CHARACTER*200  TMX_VNM      
  CHARACTER*200  TMN_VNM      
  CHARACTER*200  PRC_VNM      
  CHARACTER*200  RSD_VNM      
  CHARACTER*200  SHM_VNM      
  CHARACTER*200  WND_VNM      
  CHARACTER*200  PRS_VNM      
  CHARACTER*200  OZN_VNM      
  CHARACTER*200  PLT1_VNM      
  CHARACTER*200  GDHm1_VNM     
  CHARACTER*200  SOIL_VNM     
  CHARACTER*200  NFERT_VNM    
  CHARACTER*200  Vsat_VNM    
  CHARACTER*200  LND_VNM    

  !! [SPECIFIC YEAR of INPUTs (NetCDF) ] !! Write "YYYY" or "YYYYs" if you want to run in a specifc year or years, otherwise write nothing, which means running year. 
  CHARACTER*5  TMP_YEAR      
  CHARACTER*5  TMX_YEAR      
  CHARACTER*5  TMN_YEAR      
  CHARACTER*5  PRC_YEAR      
  CHARACTER*5  RSD_YEAR      
  CHARACTER*5  SHM_YEAR     
  CHARACTER*5  WND_YEAR      
  CHARACTER*5  PRS_YEAR      
  CHARACTER*5  OZN_YEAR      
  CHARACTER*5  PLT1_YEAR      
  CHARACTER*5  GDHm1_YEAR     
  CHARACTER*5  SOIL_YEAR     
  CHARACTER*5  NFERT_YEAR    
  CHARACTER*5  Vsat_YEAR    
  CHARACTER*5  LND_YEAR    


  !! !!!!!!!!!!!!!!!!!!! !!
  !! [LOCATION VARIABLE] !!
  !! !!!!!!!!!!!!!!!!!!! !!
  REAL*8 LAT    ! Latitude [**.** (-90.0 - 90.0)]
  REAL*8 LON    ! Longitude [**.** (-180.0 - 180.0)]
  INTEGER ILAT  ! Index of latitude
  INTEGER ILON  ! Index of longitude
  INTEGER NLAT  ! Number of grids in latitude
  INTEGER NLON  ! Number of grids in longitude
    
  !! !!!!!!!!!!!!!!!!!! !!
  !! [FORCING VARIABLE] !!
  !! !!!!!!!!!!!!!!!!!! !!
  REAL*8 TMP    ! Air temperature              [K]
  REAL*8 TMX    ! Daily Maximum temperature    [K]
  REAL*8 PRC    ! Precipitation                [kg/m**2/s]
  REAL*8 RSD    ! Downward shortwave radiation [W/m-2]
  REAL*8 SHM    ! Specific density of H2O      [kg/kg]
  REAL*8 WND    ! Wind speed                   [m/s]
  REAL*8 PRS    ! Surface pressure             [Pa]
  REAL*8 OZN    ! Ozone conentration           [ppb]

  REAL*8,ALLOCATABLE:: INTMP(:,:,:)    ! Air temperature               [same unit of the input data]
  REAL*8,ALLOCATABLE:: INTMX(:,:,:)    ! Maximum Air temperature       [same unit of the input data]
  REAL*8,ALLOCATABLE:: INTMN(:,:,:)    ! Minimum Air temperature       [same unit of the input data]
  REAL*8,ALLOCATABLE:: INPRC(:,:,:)    ! Precipitation                 [same unit of the input data]
  REAL*8,ALLOCATABLE:: INRSD(:,:,:)    ! Downward shortwave radiation  [same unit of the input data]
  REAL*8,ALLOCATABLE:: INSHM(:,:,:)    ! Specific density of H2O       [same unit of the input data]
  REAL*8,ALLOCATABLE:: INWND(:,:,:)    ! Wind speed                    [same unit of the input data]
  REAL*8,ALLOCATABLE:: INPRS(:,:,:)    ! Surface pressure              [same unit of the input data]
  REAL*8,ALLOCATABLE:: INOZN(:,:,:)    ! Ozone concentration           [same unit of the input data]



  REAL*8 CO2PPM ! CO2 concentration [PPM]

  !! !!!!!!!!!!!!!!!! !!
  !! [CROP PARAMETER] !!
  !! !!!!!!!!!!!!!!!! !!
  !<Leaf photosynthesis>
  REAL*8 RESPCP    ! Respiration fraction of Vmax                   [-]
  REAL*8 EFFCON    ! Quantum efficiency                             [mol/mol]
  REAL*8 ATHETA    ! Coupling parameter (Wc,We)                     [-]
  REAL*8 BTHETA    ! Coupling parameter (Wp,Ws)                     [-]
  REAL*8 MH2O      ! Conductance-phtosynthesis slope parameter      [mol/m**2(l)/s] (for GSH2O)
  REAL*8 BH2O      ! Conductance-photosynthesis intercept           [mol/m**2(l)/s] (for GSH2O)
  REAL*8 KN        ! Vertical distribution factor for Nitrogen      [-]

  REAL*8 ZKCA      ! Kc at 298K                                     [Pa]
  REAL*8 ZKCB      ! Parameter for temperature dependence of Kc     [-]
  REAL*8 ZKOA      ! Ko at 298K                                     [Pa]
  REAL*8 ZKOB      ! Parameter nfor temperature dependence of Ko    [-]
  REAL*8 GMMA      ! Parameter for temperature dependence of Gamma* [-]
  REAL*8 GMMB      ! Parameter for temperature dependence of Gamma* [-]

  !<Bulk transfer coefficient>
  REAL*8 LTCH      ! Leaf transfer coefficient for heat             [-]

  !<Radiation>
  REAL*8 RLFV      ! Leaf albedo (VIS)                              [-]
  REAL*8 TLFV      ! Leaf trans. (VIS)                              [-]
  REAL*8 RLFN      ! Leaf albedo (NIR)                              [-]
  REAL*8 TLFN      ! Leaf trans. (NIR)                              [-]

  !<Crop>
  REAL*8 hDVS      ! DVS at heading                                 [-]

  REAL*8 CFLF      ! Fraction of C for leaf                         [kg(C)/kg(leaf)]
  REAL*8 CFST      ! Fraction of C for stem                         [kg(C)/kg(stem)]
  REAL*8 CFRT      ! Fraction of C for root                         [kg(C)/kg(root)]
  REAL*8 CFSO      ! Fraction of C for storage                      [kg(C)/kg(storage)]

  INTEGER VN
  REAL*8 TB
  REAL*8 TO
  REAL*8 TH

  REAL*8 DLFX1     ! 1st point of DVS for dead leaf                 [-]
  REAL*8 DLFY1     ! Rate of dead leaf at 1st point of DVS          [-]
  REAL*8 DLFX2     ! 2nd point of DVS for dead leaf                 [-]
  REAL*8 DLFY2     ! Rate of dead leaf at 2nd point of DVS          [-]
  REAL*8 DLFX3     ! 3rd point of DVS for dead leaf                 [-]
  REAL*8 DLFY3     ! Rate of dead leaf at 3rd point of DVS          [-]

  REAL*8 RTX       ! 1st point of DVS for partition for root        [-]
  REAL*8 RTY       ! Rate of partition for root at 1st point        [-]
  REAL*8 RTX2      ! 2nd point of DVS for partition for root        [-]

  REAL*8 LEFY0     ! Rate of partition for leaf at DVS=0            [-]
  REAL*8 LEFX1     ! 1st point of DVS for leaf                      [-]
  REAL*8 LEFY1     ! Rate of partition for leaf at 1st point        [-]
  REAL*8 LEFX2     ! 2nd point of DVS for leaf                      [-]
  REAL*8 LEFY2     ! Rate of partition for leaf at 2nd point        [-]
  REAL*8 LEFX3     ! 3rd point of DVS for leaf                      [-]
  REAL*8 LEFY3     ! Rate of partition ofr leaf at 3rd point        [-]

  REAL*8 PNCLX1    ! 1st point of DVS for panicle                   [-]
  REAL*8 PNCLY1    ! Rate of partition for panicle at 1st point     [-]
  REAL*8 PNCLX2    ! 2nd point of DVS for panicle                   [-]
  REAL*8 PNCLY2    ! Rate of partition for panicle at 2nd point     [-]
  REAL*8 PNCLX3    ! 3rd point of DVS for panicle                   [-]
  REAL*8 PNCLY3    ! Rate of partition for panicle at 3rd point     [-]

  REAL*8 FSTR      ! Partition to sielded reserve (starch) in stem  [-]

  REAL*8 SLWYA     ! Specific leaf weight at DVS = 0                [kg/m**2(l)]
  REAL*8 SLWYB     ! Specific leaf weight at  DVS = infinity        [kg/m**2(l)]
  REAL*8 SLWX      ! Shape parameter of specific leaf weight        [-]

  REAL*8 HGTAA     ! HGTAA * LAI ** HGTBA (before heading)          [-]
  REAL*8 HGTAB     ! HGTAB * LAI ** HGTBB (after heading)           [-]
  REAL*8 HGTBA     ! HGTAA * LAI ** HGTBA (before heading)          [-]
  REAL*8 HGTBB     ! HGTAB * LAI ** HGTBB (after heading)           [-]

  REAL*8 GZRT      ! Growth rate of root                            [m/day]
  REAL*8 MXRT      ! Maximum root length                            [m]
  REAL*8 GMMSL     ! Soil stress factor                             [-]

  REAL*8 SLNX1     ! 1st point of DVS for SLN                       [-]
  REAL*8 SLNX2     ! 2nd point of DVS for SLN                       [-]
  REAL*8 SLNX3     ! 3rd point of DVS for SLN                       [-]
  REAL*8 SLNYMX    ! Maximum SLN                                    [-]
  REAL*8 SLNYMN    ! Minimum SLN                                    [-]
  REAL*8 SLNK      ! Shape parameter of SLN curve                   [-]

  REAL*8 TCmin     ! Critical temperature for cool damage           [deg] 
  REAL*8 THcrit    ! Critical temperature for hot damage            [deg]
  REAL*8 HI
 
  REAL*8 PLTDIF    ! Day of difference of PLTDOY                    [Day]

  REAL*8 HVT_TAVE  ! Critical temperature at harvest 

  !! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! !!
  !! [PLANT DOY and GDH at maturity] !!
  !! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! !!
  INTEGER,ALLOCATABLE:: SOILTXT(:,:)    ! Soil texture            [-]
  INTEGER,ALLOCATABLE:: PLTDOY(:,:)   ! Planting doy            [DOY]
  REAL*8, ALLOCATABLE:: GDHm(:,:)     ! GDH at maturity         [Degree seconds]
  REAL*8, ALLOCATABLE:: NFERT(:,:)      ! Nitrogen fertilizer application [kg/ha]
  REAL*8, ALLOCATABLE:: Vsat(:,:)        ! required vernarization days
  REAL*8, ALLOCATABLE:: LND(:,:)        ! required vernarization days

  REAL*8, ALLOCATABLE:: BUF(:,:)        ! Temerate array for inputs [same unit of inputs]

  !! !!!!!!!!!!!!!!!!!!! !!
  !! [INTERNAL VARIABLE] !!
  !! !!!!!!!!!!!!!!!!!!! !!
  INTEGER IRR         ! FLAG of irrigation     [-] IRR=1: IRRIGATION; IRR=0: RAINFED
  INTEGER GRW         ! FLAG of growing season [-] GRW=1: first growing season; GRW=2: second growing season  
  INTEGER IGRW
  

  INTEGER YEAR        ! Year                   [Year]
  INTEGER DOY         ! Day of year            [Day]
  REAL*8  HOUR        ! Hour                   [Hour]

  INTEGER IHOUR       ! Index of hour          [-]
  INTEGER NDOY        ! DOY of next day        [-] 
  INTEGER PDOY        ! DOY of previous day    [-]

  REAL*8 INTSINB      ! Integration of SINB during a day [second]


  REAL*8 GPP          ! Gross primary production         [mol(CO2)/m**2/s]
  REAL*8 RSP          ! Respiration                      [mol(CO2)/m**2/s]
  REAL*8 TSP          ! Transpiration                    [kg/m**2/s]

  REAL*8 QPARSNLF     ! Absorbed PAR per unit leaf area for sunlit leaf   [W/m**2(l)]
  REAL*8 QPARSHLF     ! Absorbed PAR per unit leaf area for sunshade leaf [W/m**2(l)]
  REAL*8 VMXSNLF      ! Vmax per unit leaf area for sunlit leaf           [CO2/m**2(l)/s]
  REAL*8 VMXSHLF      ! Vmax per unit leaf area for sunshade leaf         [CO2/m**2(l)/s]
  REAL*8 LAISN        ! LAI for sunlit leaf                               [m**2(l)/m**2]
  REAL*8 LAISH        ! LAI for sunshade leaf                             [m**2(l)/m**2]

  INTEGER I,J

  INTEGER STDD,ENDD

  !! < State variable > !!
  REAL*8 WSL(NSL)     ! Soil water content       [m**3/m**3] 
  REAL*8 WSTRS        ! Soil water stress factor [-] (0~1)


  REAL*8 SLN          ! Leaf nitrogen concentration per unit leaf area[g/m**2(l)]

  INTEGER PLT         ! FLAG for planting        [-]
  LOGICAL EMR         ! FLAG for emergence       [-]
  INTEGER GRN         ! FLAG for grain filling   [-]

  REAL*8 DVS          ! OZN
  REAL*8 DVSL
  REAL*8 aO3FLX       ! OZN
  REAL*8 aGDH         ! accumulated GDH

  REAL*8 aVD  

  REAL*8 CDI          ! Cold damage index
  REAL*8 HDI          ! Hot damage index
  REAL*8 TAVE
  REAL*8 NHED         ! Number of days during heading for heat damage [day]


  REAL*8 LAI          ! LAI               [m**2(l)/m**2(ground)]
  REAL*8 ROT          ! Root depth             [m]
  REAL*8 HGT          ! Height            [m]

  REAL*8 DLFX
  REAL*8 LLFst
  REAL*8 kLLF

  REAL*8 WSH          ! Weight of shoot                                 [kg/ha]
  REAL*8 WSO          ! Weight of storage organ                         [kg/ha]
  REAL*8 WST          ! Weight of stem                                  [kg/ha]
  REAL*8 WLF          ! Weight of leaf                                  [kg/ha]
  REAL*8 WRT          ! Weight of root                                  [kg/ha]
  REAL*8 WAR          ! Weight of easily available reserve (glucose)    [kg/ha]
  REAL*8 WIR          ! Weight of non-easily available reserve (starch) [kg/ha]
  REAL*8 WDL          ! Weight of dead leaf                             [kg/ha]
  REAL*8 WGR
  REAL*8 SWGR
  REAL*8 NGR
  REAL*8 NSP


  REAL*8 GLF
  REAL*8 GST
  REAL*8 GRT
  REAL*8 GSO


  !! [NUMBER of LONGITUDE of INPUTs] 
  INTEGER  TMP_NLON      
  INTEGER  TMX_NLON      
  INTEGER  TMN_NLON      
  INTEGER  PRC_NLON      
  INTEGER  RSD_NLON      
  INTEGER  SHM_NLON      
  INTEGER  WND_NLON      
  INTEGER  PRS_NLON      
  INTEGER  OZN_NLON      
  INTEGER  PLT1_NLON      
  INTEGER  GDHm1_NLON     
  INTEGER  SOIL_NLON     
  INTEGER  NFERT_NLON    
  INTEGER  Vsat_NLON    
  INTEGER  LND_NLON    

  !! [NUMBER of LATITUDE of INPUTs] 
  INTEGER  TMP_NLAT      
  INTEGER  TMX_NLAT      
  INTEGER  TMN_NLAT      
  INTEGER  PRC_NLAT      
  INTEGER  RSD_NLAT      
  INTEGER  SHM_NLAT      
  INTEGER  WND_NLAT      
  INTEGER  PRS_NLAT      
  INTEGER  OZN_NLAT      
  INTEGER  PLT1_NLAT      
  INTEGER  GDHm1_NLAT     
  INTEGER  SOIL_NLAT     
  INTEGER  NFERT_NLAT    
  INTEGER  Vsat_NLAT    
  INTEGER  LND_NLAT    



  

! Storage for state variables
  REAL*8,ALLOCATABLE:: pWSL(:,:,:)
  REAL*8,ALLOCATABLE:: pWSTRS(:,:)

  REAL*8,ALLOCATABLE:: pSLN(:,:)

  INTEGER,ALLOCATABLE:: pPLT(:,:)
  INTEGER,ALLOCATABLE:: pGRN(:,:)
  LOGICAL,ALLOCATABLE:: pEMR(:,:)

  REAL*8,ALLOCATABLE:: paGDH(:,:)
  REAL*8,ALLOCATABLE:: paVD(:,:)
  REAL*8,ALLOCATABLE:: pDVS(:,:)  !OZN
  REAL*8,ALLOCATABLE:: pDVSL(:,:)  !OZN
  REAL*8,ALLOCATABLE:: paO3FLX(:,:)  !OZN

  REAL*8,ALLOCATABLE:: pCDI(:,:)
  REAL*8,ALLOCATABLE:: pHDI(:,:)
  REAL*8,ALLOCATABLE:: pTAVE(:,:)
  REAL*8,ALLOCATABLE:: pNHED(:,:)


  REAL*8,ALLOCATABLE:: pLAI(:,:)
  REAL*8,ALLOCATABLE:: LAImx(:,:,:) !changed LAImx to DOYmx
  INTEGER,ALLOCATABLE:: DOYmx(:,:,:) !changed LAImx to DOYmx
  REAL*8,ALLOCATABLE:: pDLFX(:,:)

  REAL*8,ALLOCATABLE:: pROT(:,:)
  REAL*8,ALLOCATABLE:: pHGT(:,:)

  REAL*8,ALLOCATABLE:: pWSH(:,:)
  REAL*8,ALLOCATABLE:: pWSO(:,:)
  REAL*8,ALLOCATABLE:: pWST(:,:)
  REAL*8,ALLOCATABLE:: pWLF(:,:)
  REAL*8,ALLOCATABLE:: pWRT(:,:)
  REAL*8,ALLOCATABLE:: pWAR(:,:)
  REAL*8,ALLOCATABLE:: pWIR(:,:)
  REAL*8,ALLOCATABLE:: pWDL(:,:)
  REAL*8,ALLOCATABLE:: pWGR(:,:)
  REAL*8,ALLOCATABLE:: pSWGR(:,:)
  REAL*8,ALLOCATABLE:: pNGR(:,:)
  REAL*8,ALLOCATABLE:: pNSP(:,:)

  REAL*8,ALLOCATABLE:: pGLF(:,:)
  REAL*8,ALLOCATABLE:: pGST(:,:)
  REAL*8,ALLOCATABLE:: pGRT(:,:)
  REAL*8,ALLOCATABLE:: pGSO(:,:)


  REAL*8,ALLOCATABLE:: TAVE5DAY(:) ! mean temperature during previous 5 days
  REAL*8,ALLOCATABLE:: pTAVE5DAY(:,:,:)
  INTEGER TAVE5CNT
  INTEGER,ALLOCATABLE:: pTAVE5CNT(:,:) 

!  [OUTPUT]
  REAL*8,ALLOCATABLE:: YLD(:,:,:)   ! Yield    [kg/ha]




!  [FUNCTION]
  REAL*8 SINB
  REAL*8 CALHOUR
  


  
  print *,"START"



  
  !! !!!!!!!!!!!!!!!!!!!! !!
  !!  <READ SETTING FILE> !!
  !! !!!!!!!!!!!!!!!!!!!! !!
  CALL RDSET(STYR,ENYR,STDOY,ENDOY,TRES,clmTRES,WEST,EAST,NORTH,SOUTH,WERES,NSRES,IRR,CRP_NAME,CRP_FILE,CO2_HEAD,CO2_YEAR,CO2_FOOT,YLD_HEAD,LAI_HEAD,TMP_HEAD,TMP_YEAR,TMP_FOOT,TMP_VNM,TMP_WEST,TMP_EAST,TMP_NORTH,TMP_SOUTH,TMP_WERES,TMP_NSRES,TMP_NSDIR,TMP_FORM,TMP_BYTE,TMP_MUL,TMP_ADD,TMX_HEAD,TMX_YEAR,TMX_FOOT,TMX_VNM,TMX_WEST,TMX_EAST,TMX_NORTH,TMX_SOUTH,TMX_WERES,TMX_NSRES,TMX_NSDIR,TMX_FORM,TMX_BYTE,TMX_MUL,TMX_ADD,TMN_HEAD,TMN_YEAR,TMN_FOOT,TMN_VNM,TMN_WEST,TMN_EAST,TMN_NORTH,TMN_SOUTH,TMN_WERES,TMN_NSRES,TMN_NSDIR,TMN_FORM,TMN_BYTE,TMN_MUL,TMN_ADD,PRC_HEAD,PRC_YEAR,PRC_FOOT,PRC_VNM,PRC_WEST,PRC_EAST,PRC_NORTH,PRC_SOUTH,PRC_WERES,PRC_NSRES,PRC_NSDIR,PRC_FORM,PRC_BYTE,PRC_MUL,PRC_ADD,RSD_HEAD,RSD_YEAR,RSD_FOOT,RSD_VNM,RSD_WEST,RSD_EAST,RSD_NORTH,RSD_SOUTH,RSD_WERES,RSD_NSRES,RSD_NSDIR,RSD_FORM,RSD_BYTE,RSD_MUL,RSD_ADD,SHM_HEAD,SHM_YEAR,SHM_FOOT,SHM_VNM,SHM_WEST,SHM_EAST,SHM_NORTH,SHM_SOUTH,SHM_WERES,SHM_NSRES,SHM_NSDIR,SHM_FORM,SHM_BYTE,SHM_MUL,SHM_ADD,WND_HEAD,WND_YEAR,WND_FOOT,WND_VNM,WND_WEST,WND_EAST,WND_NORTH,WND_SOUTH,WND_WERES,WND_NSRES,WND_NSDIR,WND_FORM,WND_BYTE,WND_MUL,WND_ADD,WND_HGT,PRS_HEAD,PRS_YEAR,PRS_FOOT,PRS_VNM,PRS_WEST,PRS_EAST,PRS_NORTH,PRS_SOUTH,PRS_WERES,PRS_NSRES,PRS_NSDIR,PRS_FORM,PRS_BYTE,PRS_MUL,PRS_ADD,OZN_HEAD,OZN_YEAR,OZN_FOOT,OZN_VNM,OZN_WEST,OZN_EAST,OZN_NORTH,OZN_SOUTH,OZN_WERES,OZN_NSRES,OZN_NSDIR,OZN_FORM,OZN_BYTE,OZN_MUL,OZN_ADD,PLT1_HEAD,PLT1_YEAR,PLT1_FOOT,PLT1_VNM,PLT1_WEST,PLT1_EAST,PLT1_NORTH,PLT1_SOUTH,PLT1_WERES,PLT1_NSRES,PLT1_NSDIR,PLT1_FORM,PLT1_BYTE,PLT1_MUL,PLT1_ADD,GDHm1_HEAD,GDHm1_YEAR,GDHm1_FOOT,GDHm1_VNM,GDHm1_WEST,GDHm1_EAST,GDHm1_NORTH,GDHm1_SOUTH,GDHm1_WERES,GDHm1_NSRES,GDHm1_NSDIR,GDHm1_FORM,GDHm1_BYTE,GDHm1_MUL,GDHm1_ADD,LND_HEAD,LND_YEAR,LND_FOOT,LND_VNM,LND_WEST,LND_EAST,LND_NORTH,LND_SOUTH,LND_WERES,LND_NSRES,LND_NSDIR,LND_FORM,LND_BYTE,LND_MUL,LND_ADD,SOIL_HEAD,SOIL_YEAR,SOIL_FOOT,SOIL_VNM,SOIL_WEST,SOIL_EAST,SOIL_NORTH,SOIL_SOUTH,SOIL_WERES,SOIL_NSRES,SOIL_NSDIR,SOIL_FORM,SOIL_BYTE,SOIL_MUL,SOIL_ADD,NFERT_HEAD,NFERT_YEAR,NFERT_FOOT,NFERT_VNM,NFERT_WEST,NFERT_EAST,NFERT_NORTH,NFERT_SOUTH,NFERT_WERES,NFERT_NSRES,NFERT_NSDIR,NFERT_FORM,NFERT_BYTE,NFERT_MUL,NFERT_ADD,Vsat_HEAD,Vsat_YEAR,Vsat_FOOT,Vsat_VNM,Vsat_WEST,Vsat_EAST,Vsat_NORTH,Vsat_SOUTH,Vsat_WERES,Vsat_NSRES,Vsat_NSDIR,Vsat_FORM,Vsat_BYTE,Vsat_MUL,Vsat_ADD)

  IF(EAST>WEST)THEN
     NLON=INT((EAST-WEST)/WERES)
  ELSE
     NLON=INT((EAST+360.D0-WEST)/WERES)
  END IF
  NLAT=INT((NORTH-SOUTH)/NSRES)


  IF(TMP_EAST>TMP_WEST)THEN
     TMP_NLON=INT((TMP_EAST-TMP_WEST)/TMP_WERES)
  ELSE
     TMP_NLON=INT((TMP_EAST+360.D0-TMP_WEST)/TMP_WERES)
  END IF
  TMP_NLAT=INT((TMP_NORTH-TMP_SOUTH)/TMP_NSRES)

  
  IF(TMX_EAST>TMX_WEST)THEN
     TMX_NLON=INT((TMX_EAST-TMX_WEST)/TMX_WERES)
  ELSE
     TMX_NLON=INT((TMX_EAST+360.D0-TMX_WEST)/TMX_WERES)
  END IF
  TMX_NLAT=INT((TMX_NORTH-TMX_SOUTH)/TMX_NSRES)

  IF(TMN_EAST>TMN_WEST)THEN
     TMN_NLON=INT((TMN_EAST-TMN_WEST)/TMN_WERES)
  ELSE
     TMN_NLON=INT((TMN_EAST+360.D0-TMN_WEST)/TMN_WERES)
  END IF
  TMN_NLAT=INT((TMN_NORTH-TMN_SOUTH)/TMN_NSRES)

  IF(PRC_EAST>PRC_WEST)THEN
     PRC_NLON=INT((PRC_EAST-PRC_WEST)/PRC_WERES)
  ELSE
     PRC_NLON=INT((PRC_EAST+360.D0-PRC_WEST)/PRC_WERES)
  END IF
  PRC_NLAT=INT((PRC_NORTH-PRC_SOUTH)/PRC_NSRES)

  IF(RSD_EAST>RSD_WEST)THEN
     RSD_NLON=INT((RSD_EAST-RSD_WEST)/RSD_WERES)
  ELSE
     RSD_NLON=INT((RSD_EAST+360.D0-RSD_WEST)/RSD_WERES)
  END IF
  RSD_NLAT=INT((RSD_NORTH-RSD_SOUTH)/RSD_NSRES)

  IF(SHM_EAST>SHM_WEST)THEN
     SHM_NLON=INT((SHM_EAST-SHM_WEST)/SHM_WERES)
  ELSE
     SHM_NLON=INT((SHM_EAST+360.D0-SHM_WEST)/SHM_WERES)
  END IF
  SHM_NLAT=INT((SHM_NORTH-SHM_SOUTH)/SHM_NSRES)

  IF(WND_EAST>WND_WEST)THEN
     WND_NLON=INT((WND_EAST-WND_WEST)/WND_WERES)
  ELSE
     WND_NLON=INT((WND_EAST+360.D0-WND_WEST)/WND_WERES)
  END IF
  WND_NLAT=INT((WND_NORTH-WND_SOUTH)/WND_NSRES)

  IF(PRS_EAST>PRS_WEST)THEN
     PRS_NLON=INT((PRS_EAST-PRS_WEST)/PRS_WERES)
  ELSE
     PRS_NLON=INT((PRS_EAST+360.D0-PRS_WEST)/PRS_WERES)
  END IF
  PRS_NLAT=INT((PRS_NORTH-PRS_SOUTH)/PRS_NSRES)

  IF(PLT1_EAST>PLT1_WEST)THEN
     PLT1_NLON=INT((PLT1_EAST-PLT1_WEST)/PLT1_WERES)
  ELSE
     PLT1_NLON=INT((PLT1_EAST+360.D0-PLT1_WEST)/PLT1_WERES)
  END IF
  PLT1_NLAT=INT((PLT1_NORTH-PLT1_SOUTH)/PLT1_NSRES)

  IF(GDHm1_EAST>GDHm1_WEST)THEN
     GDHm1_NLON=INT((GDHm1_EAST-GDHm1_WEST)/GDHm1_WERES)
  ELSE
     GDHm1_NLON=INT((GDHm1_EAST+360.D0-GDHm1_WEST)/GDHm1_WERES)
  END IF
  GDHm1_NLAT=INT((GDHm1_NORTH-GDHm1_SOUTH)/GDHm1_NSRES)



  IF(SOIL_EAST>SOIL_WEST)THEN
     SOIL_NLON=INT((SOIL_EAST-SOIL_WEST)/SOIL_WERES)
  ELSE
     SOIL_NLON=INT((SOIL_EAST+360.D0-SOIL_WEST)/SOIL_WERES)
  END IF
  SOIL_NLAT=INT((SOIL_NORTH-SOIL_SOUTH)/SOIL_NSRES)

  IF(NFERT_EAST>NFERT_WEST)THEN
     NFERT_NLON=INT((NFERT_EAST-NFERT_WEST)/NFERT_WERES)
  ELSE
     NFERT_NLON=INT((NFERT_EAST+360.D0-NFERT_WEST)/NFERT_WERES)
  END IF
  NFERT_NLAT=INT((NFERT_NORTH-NFERT_SOUTH)/NFERT_NSRES)

  IF(Vsat_EAST>Vsat_WEST)THEN
     Vsat_NLON=INT((Vsat_EAST-Vsat_WEST)/Vsat_WERES)
  ELSE
     Vsat_NLON=INT((Vsat_EAST+360.D0-Vsat_WEST)/Vsat_WERES)
  END IF
  Vsat_NLAT=INT((Vsat_NORTH-Vsat_SOUTH)/Vsat_NSRES)

  IF(LND_EAST>LND_WEST)THEN
     LND_NLON=INT((LND_EAST-LND_WEST)/LND_WERES)
  ELSE
     LND_NLON=INT((LND_EAST+360.D0-LND_WEST)/LND_WERES)
  END IF
  LND_NLAT=INT((LND_NORTH-LND_SOUTH)/LND_NSRES)




  
!!!< Allocate >
  ALLOCATE(INTMP(NLON,NLAT,365*86400/clmTRES),INTMX(NLON,NLAT,365*86400/clmTRES),INTMN(NLON,NLAT,365*86400/clmTRES),INPRC(NLON,NLAT,365*86400/clmTRES),INRSD(NLON,NLAT,365*86400/clmTRES),INSHM(NLON,NLAT,365*86400/clmTRES),INWND(NLON,NLAT,365*86400/clmTRES),INPRS(NLON,NLAT,365*86400/clmTRES))
  ALLOCATE(INOZN(NLON,NLAT,365*86400/clmTRES))  !OZN

  ALLOCATE(PLTDOY(NLON,NLAT),GDHm(NLON,NLAT),NFERT(NLON,NLAT),Vsat(NLON,NLAT),LND(NLON,NLAT))
  ALLOCATE(SOILTXT(NLON,NLAT))
  ALLOCATE(BUF(NLON,NLAT))
  
  ALLOCATE(pPLT(NLON,NLAT),pGRN(NLON,NLAT),pEMR(NLON,NLAT))
  ALLOCATE(paGDH(NLON,NLAT))
  ALLOCATE(paVD(NLON,NLAT))
  ALLOCATE(pDVS(NLON,NLAT)) !OZN
  ALLOCATE(pDVSL(NLON,NLAT)) !OZN
  ALLOCATE(paO3FLX(NLON,NLAT))  !OZN
  ALLOCATE(pWSL(NLON,NLAT,NSL),pWSTRS(NLON,NLAT))
  ALLOCATE(pSLN(NLON,NLAT))

  ALLOCATE(pCDI(NLON,NLAT))
  ALLOCATE(pHDI(NLON,NLAT))
  ALLOCATE(pTAVE(NLON,NLAT))
  ALLOCATE(pNHED(NLON,NLAT))

  ALLOCATE(TAVE5DAY(86400/TRES*5))
  ALLOCATE(pTAVE5DAY(NLON,NLAT,86400/TRES*5)) !5days
  ALLOCATE(pTAVE5CNT(NLON,NLAT))

  
  ALLOCATE(pLAI(NLON,NLAT),pROT(NLON,NLAT),pHGT(NLON,NLAT))
  ALLOCATE(pDLFX(NLON,NLAT))
  ALLOCATE(pWSH(NLON,NLAT),pWSO(NLON,NLAT),pWST(NLON,NLAT),pWLF(NLON,NLAT),pWRT(NLON,NLAT),pWAR(NLON,NLAT),pWIR(NLON,NLAT),pWDL(NLON,NLAT),pGLF(NLON,NLAT),pGST(NLON,NLAT),pGRT(NLON,NLAT),pGSO(NLON,NLAT))
  ALLOCATE(pWGR(NLON,NLAT),pSWGR(NLON,NLAT),pNGR(NLON,NLAT),pNSP(NLON,NLAT))

  ALLOCATE(YLD(NLON,NLAT,ENYR-STYR+1),DOYmx(NLON,NLAT,ENYR-STYR+1)) !LAImx to DOYmx
  ALLOCATE(LAImx(NLON,NLAT,ENYR-STYR+1)) 


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! [CROP PARAMETER FILE] !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CALL RDPRM(CRP_FILE,RESPCP,EFFCON,ATHETA,BTHETA,MH2O,BH2O,KN,LTCH,ZKCA,ZKCB,ZKOA,ZKOB,GMMA,GMMB,RLFV,TLFV,RLFN,TLFN,hDVS,CFLF,CFST,CFRT,CFSO,VN,TB,TO,TH,LEFY0,LEFX1,LEFY1,LEFX2,LEFY2,LEFX3,LEFY3,PNCLX1,PNCLY1,PNCLX2,PNCLY2,PNCLX3,PNCLY3,DLFX1,DLFY1,DLFX2,DLFY2,DLFX3,DLFY3,LLFst,kLLF,RTX,RTY,RTX2,FSTR,SLWYA,SLWYB,SLWX,HGTAA,HGTAB,HGTBA,HGTBB,GZRT,MXRT,GMMSL,SLNX1,SLNX2,SLNX3,SLNYMX,SLNYMN,SLNK,TCmin,THcrit,HI,PLTDIF,HVT_TAVE)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! [SOIL TEXTURE, PLTDOY, and GDHm] !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CALL RDNC(BUF,SOIL_HEAD,SOIL_YEAR,0,SOIL_FOOT,SOIL_VNM,SOIL_NLON,SOIL_NLAT,1,SOIL_WEST,SOIL_EAST,SOIL_NORTH,SOIL_SOUTH,SOIL_WERES,SOIL_NSRES,SOIL_NSDIR,SOIL_MUL,SOIL_ADD,SOIL_FORM,SOIL_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)
  BUF=MIN(BUF,100000.D0)
  SOILTXT=INT(BUF)


  CALL RDNC(BUF,PLT1_HEAD,PLT1_YEAR,0,PLT1_FOOT,PLT1_VNM,PLT1_NLON,PLT1_NLAT,1,PLT1_WEST,PLT1_EAST,PLT1_NORTH,PLT1_SOUTH,PLT1_WERES,PLT1_NSRES,PLT1_NSDIR,PLT1_MUL,PLT1_ADD,PLT1_FORM,PLT1_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)
  BUF=MIN(BUF,100000.D0)
  PLTDOY(:,:)=INT(BUF)


  CALL RDNC(GDHm,GDHm1_HEAD,GDHm1_YEAR,0,GDHm1_FOOT,GDHm1_VNM,GDHm1_NLON,GDHm1_NLAT,1,GDHm1_WEST,GDHm1_EAST,GDHm1_NORTH,GDHm1_SOUTH,GDHm1_WERES,GDHm1_NSRES,GDHm1_NSDIR,GDHm1_MUL,GDHm1_ADD,GDHm1_FORM,GDHm1_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)


  IF(CRP_NAME .eq. "Wheat")THEN

     CALL RDNC(Vsat,Vsat_HEAD,Vsat_YEAR,0,Vsat_FOOT,Vsat_VNM,Vsat_NLON,Vsat_NLAT,1,Vsat_WEST,Vsat_EAST,Vsat_NORTH,Vsat_SOUTH,Vsat_WERES,Vsat_NSRES,Vsat_NSDIR,Vsat_MUL,Vsat_ADD,Vsat_FORM,Vsat_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)

  END IF

  

  
  !! !!!!!!!!!!!!!!!!!!!!!!!!!! !!
  !! Initialize state variables !!
  !! !!!!!!!!!!!!!!!!!!!!!!!!!! !!
  pPLT(:,:)=0
  pGRN(:,:)=0
  pEMR(:,:)=.FALSE.

  paGDH(:,:)=0.D0
  paVD(:,:)=0.D0
  pDVS(:,:)=0.D0   !OZN
  pDVSL(:,:)=0.D0   !OZN
  paO3FLX(:,:)=0.D0 !OZN
  pLAI(:,:)=0.D0
  pROT(:,:)=0.D0
  pHGT(:,:)=0.D0
  pDLFX(:,:)=0.D0

  pCDI(:,:)=0.D0
  pHDI(:,:)=0.D0
  pTAVE(:,:)=0.D0
  pNHED(:,:)=0.D0

  pTAVE5DAY(:,:,:)=0.D0
  pTAVE5CNT(:,:)=0

  
  pWSL(:,:,:)=1.D0
  pWSTRS(:,:)=1.D0
  pSLN(:,:)=0.D0

  pWSH(:,:)=0.D0
  pWSO(:,:)=0.D0
  pWST(:,:)=0.D0
  pWLF(:,:)=0.D0
  pWRT(:,:)=0.D0
  pWAR(:,:)=0.D0
  pWIR(:,:)=0.D0
  pWDL(:,:)=0.D0
  pWGR(:,:)=0.D0
  pSWGR(:,:)=0.D0
  pNGR(:,:)=0.D0
  pNSP(:,:)=0.D0

  pGLF(:,:)=0.D0
  pGST(:,:)=0.D0
  pGRT(:,:)=0.D0
  pGSO(:,:)=0.D0



  !! !!!!!!!!!!!!!!!!!!! !!
  !! [Calculation START] !!
  !! !!!!!!!!!!!!!!!!!!! !!

  DO YEAR = STYR,ENYR

     !! !!!!!!!!!!!!!!!!!!!!! !!
     !! [READING YEARLY FILE] !!
     !! !!!!!!!!!!!!!!!!!!!!! !!
     print *,"READING FORCING for ",YEAR
     !! [FORCING FILE] !!
     !!<TMP>
     CALL RDNC(INTMP,TMP_HEAD,TMP_YEAR,YEAR,TMP_FOOT,TMP_VNM,TMP_NLON,TMP_NLAT,365,TMP_WEST,TMP_EAST,TMP_NORTH,TMP_SOUTH,TMP_WERES,TMP_NSRES,TMP_NSDIR,TMP_MUL,TMP_ADD,TMP_FORM,TMP_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)
     !!<TMX>
     CALL RDNC(INTMX,TMX_HEAD,TMX_YEAR,YEAR,TMX_FOOT,TMX_VNM,TMX_NLON,TMX_NLAT,365,TMX_WEST,TMX_EAST,TMX_NORTH,TMX_SOUTH,TMX_WERES,TMX_NSRES,TMX_NSDIR,TMX_MUL,TMX_ADD,TMX_FORM,TMX_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)
     !!<TMN>
     CALL RDNC(INTMN,TMN_HEAD,TMN_YEAR,YEAR,TMN_FOOT,TMN_VNM,TMN_NLON,TMN_NLAT,365,TMN_WEST,TMN_EAST,TMN_NORTH,TMN_SOUTH,TMN_WERES,TMN_NSRES,TMN_NSDIR,TMN_MUL,TMN_ADD,TMN_FORM,TMN_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)
     !!<PRC>
     CALL RDNC(INPRC,PRC_HEAD,PRC_YEAR,YEAR,PRC_FOOT,PRC_VNM,PRC_NLON,PRC_NLAT,365,PRC_WEST,PRC_EAST,PRC_NORTH,PRC_SOUTH,PRC_WERES,PRC_NSRES,PRC_NSDIR,PRC_MUL,PRC_ADD,PRC_FORM,PRC_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)
     !!<RSD>
     CALL RDNC(INRSD,RSD_HEAD,RSD_YEAR,YEAR,RSD_FOOT,RSD_VNM,RSD_NLON,RSD_NLAT,365,RSD_WEST,RSD_EAST,RSD_NORTH,RSD_SOUTH,RSD_WERES,RSD_NSRES,RSD_NSDIR,RSD_MUL,RSD_ADD,RSD_FORM,RSD_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)
     !!<SHM>
     CALL RDNC(INSHM,SHM_HEAD,SHM_YEAR,YEAR,SHM_FOOT,SHM_VNM,SHM_NLON,SHM_NLAT,365,SHM_WEST,SHM_EAST,SHM_NORTH,SHM_SOUTH,SHM_WERES,SHM_NSRES,SHM_NSDIR,SHM_MUL,SHM_ADD,SHM_FORM,SHM_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)
     !!<WND>
     CALL RDNC(INWND,WND_HEAD,WND_YEAR,YEAR,WND_FOOT,WND_VNM,WND_NLON,WND_NLAT,365,WND_WEST,WND_EAST,WND_NORTH,WND_SOUTH,WND_WERES,WND_NSRES,WND_NSDIR,WND_MUL,WND_ADD,WND_FORM,WND_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)
     !!<PRS>
     CALL RDNC(INPRS,PRS_HEAD,PRS_YEAR,YEAR,PRS_FOOT,PRS_VNM,PRS_NLON,PRS_NLAT,365,PRS_WEST,PRS_EAST,PRS_NORTH,PRS_SOUTH,PRS_WERES,PRS_NSRES,PRS_NSDIR,PRS_MUL,PRS_ADD,PRS_FORM,PRS_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)
     !!<OZN>
!!     CALL RDNC(INOZN,OZN_HEAD,OZN_YEAR,YEAR,OZN_FOOT,OZN_VNM,OZN_NLON,OZN_NLAT,365,OZN_WEST,OZN_EAST,OZN_NORTH,OZN_SOUTH,OZN_WERES,OZN_NSRES,OZN_NSDIR,OZN_MUL,OZN_ADD,OZN_FORM,OZN_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)
     INOZN=0.0D0
     
     !! <NITROGEN>
     CALL RDNC(NFERT,NFERT_HEAD,NFERT_YEAR,YEAR,NFERT_FOOT,NFERT_VNM,NFERT_NLON,NFERT_NLAT,1,NFERT_WEST,NFERT_EAST,NFERT_NORTH,NFERT_SOUTH,NFERT_WERES,NFERT_NSRES,NFERT_NSDIR,NFERT_MUL,NFERT_ADD,NFERT_FORM,NFERT_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)


     CALL RDNC(LND,LND_HEAD,LND_YEAR,YEAR,LND_FOOT,LND_VNM,LND_NLON,LND_NLAT,1,LND_WEST,LND_EAST,LND_NORTH,LND_SOUTH,LND_WERES,LND_NSRES,LND_NSDIR,LND_MUL,LND_ADD,LND_FORM,LND_BYTE,NLON,NLAT,WEST,EAST,NORTH,SOUTH,WERES,NSRES)

     DO I=1,NLON
        DO J=1,NLAT
           IF(NFERT(I,J) > 10000000.D0)THEN
              NFERT(I,J)=-1.D0
           END IF
           IF(LND(I,J) > 10000000.D0)THEN
              LND(I,J)=-1.D0
           END IF
        END DO
     END DO




     !! <CO2> 
     CALL RD_CO2(CO2PPM,CO2_HEAD,YEAR,CO2_YEAR,CO2_FOOT)

     !! !!!!!!!!!!!!!! !!
     !! INITIALIZATION !!
     !! !!!!!!!!!!!!!! !!


     !! [Yield initialization] !!
     YLD(:,:,YEAR-STYR+1)=-999.D0
     DOYmx(:,:,YEAR-STYR+1)= -366
     LAImx(:,:,YEAR-STYR+1)=-10.D0



!     GRW=1
!     print *,NGRW


!        DO IRR=0,1     !0,1
!           DO GRW=1,NGRW     !1,2

!     print *,LND(1,1)
!     print *,NFERT(1,1)
!     print *,PLTDOY(1,1)
!     print *,GDHm(1,1)

              DO ILON=1,NLON


                 print *,ILON,"/",NLON

                 DO ILAT=1,NLAT

!                 print *,ILAT,"/",NLAT,";",ILON,"/",NLON


                    LON = WEST + WERES * ILON - (WERES/2.D0)
                    LAT = SOUTH + NSRES * ILAT - (NSRES/2.D0)

!                    print *,ILAT,LAT
           
                    IF(LON > 180.D0)THEN
                       LON = LON - 360.D0
                    END IF
           
!                    print *,GDHm(ILON,ILAT,1,GRW),PLTDOY(ILON,ILAT,1,GRW),LON,LAT
                       
                    IF(LND(ILON,ILAT) > 0.D0 .AND. NFERT(ILON,ILAT) > -1.D0 .AND. PLTDOY(ILON,ILAT) < 1000 .AND. PLTDOY(ILON,ILAT) > 0   .AND. INPRS(ILON,ILAT,1) < 100000000.0 .AND. INPRS(ILON,ILAT,1) > 0.0 .AND. GDHm(ILON,ILAT) > 0.D0 .AND. SOILTXT(ILON,ILAT)<12 .AND. SOILTXT(ILON,ILAT) > 0)THEN !YM

                 
!                       print *,"PLT=",PLTDOY(ILON,ILAT)

                       !! Update state variables !!!
                       PLT=pPLT(ILON,ILAT)
                       GRN=pGRN(ILON,ILAT)
                       EMR=pEMR(ILON,ILAT)
                       aGDH=paGDH(ILON,ILAT)
                       aVD=paVD(ILON,ILAT)
                       DVS=pDVS(ILON,ILAT)   !OZN
                       DVSL=pDVSL(ILON,ILAT)   !OZN
                       aO3FLX=paO3FLX(ILON,ILAT)  !OZN
                       LAI=pLAI(ILON,ILAT)
                       ROT=pROT(ILON,ILAT)
                       HGT=pHGT(ILON,ILAT)
                       DLFX=pDLFX(ILON,ILAT)
                       

                       CDI=pCDI(ILON,ILAT)
                       HDI=pHDI(ILON,ILAT)
                       TAVE=pTAVE(ILON,ILAT)
                       
                       NHED=pNHED(ILON,ILAT)
                       
                       TAVE5DAY(:)=pTAVE5DAY(ILON,ILAT,:)
                       TAVE5CNT=pTAVE5CNT(ILON,ILAT)
                       
                       
                       WSH=pWSH(ILON,ILAT)
                       WSO=pWSO(ILON,ILAT)
                       WST=pWST(ILON,ILAT)
                       WLF=pWLF(ILON,ILAT)
                       WRT=pWRT(ILON,ILAT)
                       WAR=pWAR(ILON,ILAT)
                       WIR=pWIR(ILON,ILAT)
                       WDL=pWDL(ILON,ILAT)
                       WGR=pWGR(ILON,ILAT)
                       SWGR=pSWGR(ILON,ILAT)
                       NGR=pNGR(ILON,ILAT)
                       NSP=pNSP(ILON,ILAT)
                       
                       GLF=pGLF(ILON,ILAT)
                       GST=pGST(ILON,ILAT)
                       GRT=pGRT(ILON,ILAT)
                       GSO=pGSO(ILON,ILAT)
                       
                       
                       SLN=pSLN(ILON,ILAT)
                       WSL(:)=pWSL(ILON,ILAT,:)
                       WSTRS=pWSTRS(ILON,ILAT)
          
                       IF(YEAR==STYR .AND. YEAR==ENYR)THEN
                          STDD=STDOY
                          ENDD=ENDOY
                       ELSE if(YEAR==STYR)THEN
                          STDD=STDOY
                          ENDD=365
                       ELSE if(YEAR==ENYR)THEN
                          STDD=1
                          ENDD=ENDOY
                       ELSE
                          STDD=1
                          ENDD=365
                       END IF
                       
                          

                       DO DOY  = STDD,ENDD  


                       TMX=INTMX(ILON,ILAT,DOY)
                       
                       INTSINB = 0.D0
                       DO IHOUR=1,86400/TRES
                          HOUR = CALHOUR(IHOUR,TRES)
                          INTSINB = INTSINB + SINB(DOY,HOUR,LAT)*DBLE(TRES)
                       END DO
                       
                       PDOY = DOY - 1 
                       NDOY = DOY + 1
                       IF(PDOY==0)THEN
                          PDOY=365
                       END IF
                       IF(NDOY==366)THEN
                          NDOY=1
                       END IF
                       
                       DO IHOUR=1,86400/TRES
                          
                          HOUR =  CALHOUR(IHOUR,TRES)    

!                          print *,""
!                          WRITE(*,"(I5,F8.3)"),DOY,HOUR


!                          print *,"HOUR=",HOUR

                          IF(clmTRES<86400)THEN
                             TMP=INTMP(ILON,ILAT,(DOY-STDD)*(86400/TRES)+IHOUR)
                             PRC=INPRC(ILON,ILAT,(DOY-STDD)*(86400/TRES)+IHOUR)
                             RSD=INRSD(ILON,ILAT,(DOY-STDD)*(86400/TRES)+IHOUR)
                             SHM=INSHM(ILON,ILAT,(DOY-STDD)*(86400/TRES)+IHOUR)
                             WND=INWND(ILON,ILAT,(DOY-STDD)*(86400/TRES)+IHOUR)
                             PRS=INPRS(ILON,ILAT,(DOY-STDD)*(86400/TRES)+IHOUR)
                             OZN=INOZN(ILON,ILAT,(DOY-STDD)*(86400/TRES)+IHOUR)
                          ELSE
                             
                             CALL TINTERP(TMP,PRC,RSD,SHM,WND,PRS,OZN,INTMX(ILON,ILAT,PDOY)-273.15D0,INTMX(ILON,ILAT,DOY)-273.15D0,INTMX(ILON,ILAT,NDOY)-273.15D0,INTMN(ILON,ILAT,PDOY)-273.15D0,INTMN(ILON,ILAT,DOY)-273.15D0,INTMN(ILON,ILAT,NDOY)-273.15D0,INPRC(ILON,ILAT,DOY),INRSD(ILON,ILAT,DOY),INSHM(ILON,ILAT,DOY),INWND(ILON,ILAT,DOY),INPRS(ILON,ILAT,DOY),INOZN(ILON,ILAT,DOY),PDOY,DOY,NDOY,HOUR,LAT,TRES,INTSINB,WND_HGT)
                             
                          END IF
                          
                          WND=MAX(WND,0.001D0)

!                          IF(YEAR==STYR .AND. DOY<=(4+STDD))THEN
                          IF(TAVE5CNT < (86400 / TRES * 5))THEN
                             TAVE5CNT = TAVE5CNT + 1
                             TAVE5DAY(TAVE5CNT) = TMP - 273.15D0
                          ELSE
!                             IF(ILON==119)THEN
!                                print *,ILAT,TAVE5CNT
!                             END IF
                             TAVE5DAY(1:(TAVE5CNT-1))=TAVE5DAY(2:TAVE5CNT)
                             TAVE5DAY(TAVE5CNT)=TMP - 273.15d0
                          END IF
                          
                          !! Absorbed PAR, Vmax, in Sunlit and Shade leaves per LAI
                          CALL RAD(QPARSNLF,QPARSHLF,VMXSNLF,VMXSHLF,LAISN,LAISH,SLN,KN,RSD,LAI,RLFV,TLFV,RLFN,TLFN,LAT,DOY,HOUR,ILON,ILAT,CRP_NAME,DVS)


!                          IF(IRR==1 .AND. GRW==1)THEN
!                             WRITE(*,"(I5,7F20.10)"),DOY,HOUR,TMP,SHM,PRS,WND,CO2PPM,WSTRS
!                             WRITE(*,"(6F22.15)"),QPARSNLF,QPARSHLF,VMXSNLF,VMXSHLF,LAISN,LAISH
!                          END IF
                          
                          !! Canopy Gross Assimilation and Respiration, Bulk coefficient of conductance for vapor
!                          print *,VMXSNLF,VMXSHLF,LAISN,LAISH,SLN,DVS

                          CALL PHSYN(GPP,RSP,TSP,QPARSNLF,QPARSHLF,VMXSNLF,VMXSHLF,LAISN,LAISH,TMP,SHM,PRS,WND,CO2PPM,OZN,WSTRS,aO3FLX,LAI,HGT,DVS,hDVS,RESPCP,EFFCON,ATHETA,BTHETA,MH2O,BH2O,LTCH,ZKCA,ZKCB,ZKOA,ZKOB,GMMA,GMMB,TRES,CRP_NAME) 

 !                         print *,"GPP=",GPP*1000000.D0,RSP*1000000.D0


                          
                          !! Crop simulation
                          CALL CROP(DOYmx(ILON,ILAT,YEAR-STYR+1),YLD(ILON,ILAT,YEAR-STYR+1),PLT,EMR,GRN,DVS,DVSL,aGDH,aVD,CDI,HDI,TAVE,TMX,NHED,aO3FLX,LAI,LAImx(ILON,ILAT,YEAR-STYR+1),HGT,ROT,WSH,WSO,WST,WLF,WRT,WAR,WIR,WDL,WGR,SWGR,NGR,NSP,GLF,GST,GRT,GSO,SLN,NFERT(ILON,ILAT),Vsat(ILON,ILAT),GPP,RSP,TMP,PLTDOY(ILON,ILAT)+INT(PLTDIF),DOY,HOUR,TRES,GDHm(ILON,ILAT),hDVS,CFLF,CFST,CFRT,CFSO,VN,TB,TO,TH,LEFY0,LEFX1,LEFY1,LEFX2,LEFY2,LEFX3,LEFY3,PNCLX1,PNCLY1,PNCLX2,PNCLY2,PNCLX3,PNCLY3,DLFX1,DLFY1,DLFX2,DLFY2,DLFX3,DLFY3,RTX,RTY,RTX2,FSTR,SLWYA,SLWYB,SLWX,HGTAA,HGTAB,HGTBA,HGTBB,GZRT,MXRT,SLNX1,SLNX2,SLNX3,SLNYMX,SLNYMN,SLNK,WSTRS,ILON,ILAT,IRR,TCmin,THcrit,HI,TAVE5DAY,TAVE5CNT,HVT_TAVE,LLFst,kLLF,DLFX,CO2PPM,CRP_NAME) 


!                          IF(ILON==210 .AND. ILAT==210 .AND. IRR==1 .AND. GRW==1)THEN
!                          IF(ILON==281 .AND. ILAT==253 .AND. IRR==1 .AND. GRW==1)THEN
!                          IF(IRR==1 .AND. GRW==1)THEN
!
!                             WRITE(*,"(I5,3F10.2,6F21.13,2I5)"),DOY,LON,LAT,HOUR,TMP-273.15D0,PRC,RSD,SHM,WND,PRS,IRR,GRW

!                          END IF

!                          print *,CRP_NAME

                          !! Soil water balance
                          CALL SOIL(WSTRS,WSL,TSP*EL,PRC,ROT,IRR,GMMSL,TRES,SOILTXT(ILON,ILAT),TMP,PRS,WND,SHM,HGT,PLT,LON,LAT,CRP_NAME) 

!                          WRITE(*,"(I5,F8.3,2F15.5,I4,3F8.3,5F15.5)"),DOY,HOUR,DVS,LAI,PLT,Vsat(ILON,ILAT),TMP-273.15D0,WSTRS,WLF+WAR,WST+WIR,WRT,WSO,YLD(ILON,ILAT,YEAR-STYR+1)
!                          WRITE(*,"(I5,3F7.3,8F11.6,5F10.3)"),DOY,HOUR,DVS,LAI,TMP,PRC,WSTRS,WSL,WLF+WAR,WST+WIR,WRT,WSO,YLD(ILON,ILAT,YEAR-STYR+1)
!                          WRITE(*,"(I5,3F7.3,14F11.6,5F10.3)"),DOY,HOUR,DVS,LAI,TMP,PRC,WSTRS,WSL,WLF+WAR,WST+WIR,WRT,WSO,YLD(ILON,ILAT,YEAR-STYR+1)

                       END DO
                       

                    END DO
                       
                       !! Store state variables !!!!!!!!
                       pPLT(ILON,ILAT)=PLT
                       pGRN(ILON,ILAT)=GRN
                       pEMR(ILON,ILAT)=EMR
                       
                       paGDH(ILON,ILAT)=aGDH
                       paVD(ILON,ILAT)=aVD

                       pDVS(ILON,ILAT)=DVS   !OZN
                       pDVSL(ILON,ILAT)=DVSL   !OZN
                       paO3FLX(ILON,ILAT)=aO3FLX !OZN
                       pLAI(ILON,ILAT)=LAI
                       pROT(ILON,ILAT)=ROT
                       pHGT(ILON,ILAT)=HGT
                       pDLFX(ILON,ILAT)=DLFX
                       
                       pCDI(ILON,ILAT)=CDI
                       pHDI(ILON,ILAT)=HDI
                       pTAVE(ILON,ILAT)=TAVE
                       pNHED(ILON,ILAT)=NHED
                       pTAVE5DAY(ILON,ILAT,:)=TAVE5DAY(:)
                       pTAVE5CNT(ILON,ILAT)=TAVE5CNT
                       
                       
                       pWSH(ILON,ILAT)=WSH
                       pWSO(ILON,ILAT)=WSO
                       pWST(ILON,ILAT)=WST
                       pWLF(ILON,ILAT)=WLF
                       pWRT(ILON,ILAT)=WRT
                       pWAR(ILON,ILAT)=WAR
                       pWIR(ILON,ILAT)=WIR
                       pWDL(ILON,ILAT)=WDL
                       pWGR(ILON,ILAT)=WGR
                       pSWGR(ILON,ILAT)=SWGR
                       pNGR(ILON,ILAT)=NGR
                       pNSP(ILON,ILAT)=NSP
                       
                       pGLF(ILON,ILAT)=GLF
                       pGST(ILON,ILAT)=GST
                       pGRT(ILON,ILAT)=GRT
                       pGSO(ILON,ILAT)=GSO
                       
                       
                       pSLN(ILON,ILAT)=SLN
                       pWSL(ILON,ILAT,:)=WSL(:)
                       pWSTRS(ILON,ILAT)=WSTRS
                       !! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                    END IF
              
                 END DO !ILAT
              END DO !ILON


!           END DO !GRW
!        END DO !IRR
!     END DO !DOY


              ! tentative yield
             CALL WRTNC_YLD(YLD_HEAD,YLD(:,:,YEAR-STYR+1),YEAR,NLON,NLAT)
print *, "YLDMATCRO", YLD(:,:,YEAR-STYR+1)

!              CALL WRTNC_YLD(LAI_HEAD,LAImx(:,:,YEAR-STYR+1),YEAR,NLON,NLAT)
!print *, DOYmx(:,:,YEAR-STYR+1)
              CALL WRTNC_LAI(LAI_HEAD,DOYmx(:,:,YEAR-STYR+1),YEAR,NLON,NLAT) !LAImx changed into DOYmx
 
  END DO            ! YEAR

!  DO YEAR=STYR,ENYR
!     CALL WRTNC_YLD(YLD_HEAD,YLD(:,:,YEAR-STYR+1),YEAR,NLON,NLAT)
!     CALL WRTNC_YLD(LAI_HEAD,LAImx(:,:,YEAR-STYR+1),YEAR,NLON,NLAT)
!  END DO

END PROGRAM MATCRO
