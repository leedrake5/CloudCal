#define SPECCHANNELS	8192

typedef struct {
	float	sngHVADC;			// Requested HV in kV
	float	sngCurADC;			// Requested Current in µA
	byte	bytVolt;			// Ignore – legacy support
	byte	bytCurrent;			// Ignore – legacy support
	byte	bytToggle;			// Scaler selected
	byte	bytPulseLength;		// Ignore – legacy support
	byte	bytPulsePeriod;		// Ignore – legacy support
	byte	bytFilter;			// Ignore – legacy support
	byte	bytExtActual;		// Ignore – legacy support
	byte	bytTimes2;			// Ignore – legacy support
} XRAY, *PXRAY;

typedef struct {
	short		bElement;		// layer element atomic number
	short		sThickness;		// layer thickness
} FILTERLAYER, *PFILTERLAYER;

typedef struct {
	int		iFilterNum;		// filter identifier in wheel
	FILTERLAYER	flLayer[3];		// layer description structures
} S1FILTER, *PS1FILTER; 

struct 
{
	byte			FPGA_Ver;		// FPGA version (Ignore)
byte			FPGA_SubVer;	// FPGA subversion (Ignore)		unsigned short	iPacket_Len;	// Ignore					unsigned long	iTDur;		// Total packet duration (mS)
unsigned long	iRaw_Cnts;		// Raw counts, final packet		unsigned long	iValid_Cnts;	// valid counts, final packet			unsigned long	iValid_CntsRng;	// Ignore					unsigned long	iADur;		// packet active time (mS)
unsigned long	iADead;		// packet dead time (mS)		unsigned long	iAReset;		// packet reset time (mS)			unsigned long	iALive;		// packet live time (mS)			unsigned long	iService;		// Ignore					unsigned short	iReset_Cnt;		// Ignore
unsigned short	iPacket_Cnt;	// packet number			
byte			Unused[20];		// Ignore
float			fXRay_ActualHV;	// Actaul HV (not valid)		
float			fXRay_ActualAC;	// Actual anode current (not valid)	
byte			bValidActuals;	// Ignore					
byte			XRay_ActualHVDAC; // Ignore
	byte			XRay_ActualACDAC; // Ignore
	byte			Unused2;		// Ignore					
byte			Xilinx_Vars[46];  // Xilinx variables (Ignore)
	short			Det_Temp;		// detector temp in C			unsigned short	Amb_Temp;		// ambient temp in F 		
byte			MCU_Ver;		// MCU version (Ignore)					
byte			MCU_SubVer;		  // MCU subversion (Ignore)			unsigned long	iRaw_Cnts_Acc;	  // Assay Raw counts				unsigned long	iValid_Cnts_Acc;    // Assay Valid counts
	unsigned long	iValid_CntsRng_Acc; // Ignore					unsigned long	iReset_Cnt_Acc;	  // Ignore					
float			fTDur;		  // Assay duration (seconds)		
float			fADur;		  // Assay active time (seconds)
float			fADead;		  // Assay dead time (seconds)
float			fAReset;		  // Assay reset time (seconds)
float			fALive;		  // Assay live time (seconds)		unsigned long	lVacuum_Acc;	  // Ignore				
unsigned long	lPacket_Cnt;	  // packets in assay			
S1FILTER		xTubeFilter;	  // beam filter structure
XRAY			XRay;			  // x-ray settings structure
} s1_cooked_head;

struct
{
	float			fEVPerChannel;	  // eV per channel
	s1_cooked_head 	stCookedHeader;     // described above
	unsigned long	SpectrumData[SPECCHANNELS];  // spectrum data
} SpecData;