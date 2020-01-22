M2C=	mcl
BASE=	MCBase.o MCFileNames.o MCPublic.o
LIB=	Conversions.o FileLookup.o FileNames.o FilePool.o FileSystem.o memory.o monitor.o\
	NewStreams.o Options.o Program.o ProgramMess.o Storage.o Terminal.o\
	WriteString.o
INIT=	MCInit.o
PASS1=	mcp1.o mcp1id.o mcp1io.o mcp1re.o mcsymf.o
PASS2=	mcp2.o mcp2co.o mcp2id.o mcp2in.o mcp2io.o mcp2re.o mcoper.o
PASS3=	mcp3.o mcoper.o mcp3id.o mcp3io.o
PASS4=	MCPass4.o MCMnemonics.o MCP4Attribu.o MCP4CallSys.o MCP4CodeSys.o MCP4Express.o MCP4Global.o
SYM=	mcsym.o mcsymf.o
LIST=	mclist.o
LOADF=	C18.Base C18.Init C18.Pass1 C18.Pass2 C18.Pass3 C18.Pass4 C18.Symfile C18.Lister

all:	${LOADF}

C18.Base:	${BASE} ${LIB}
	${M2C} -o C18.Base ${BASE} ${LIB}

C18.Init:	${INIT} C18.Base
	${M2C} -o C18.Init -b C18.Base ${INIT}

C18.Pass1: ${PASS1} C18.Base
	${M2C} -o C18.Pass1 -b C18.Base ${PASS1}

C18.Pass2: ${PASS2} C18.Base
	${M2C} -o C18.Pass2 -b C18.Base ${PASS2}

C18.Pass3: ${PASS3} C18.Base
	${M2C} -o C18.Pass3 -b C18.Base ${PASS3}

C18.Pass4: ${PASS4} C18.Base
	${M2C} -o C18.Pass4 -b C18.Base ${PASS4}

C18.Symfile:	${SYM} C18.Base
	${M2C} -o C18.Symfile -b C18.Base ${SYM}

C18.Lister:	${LIST} C18.Base
	${M2C} -o C18.Lister -b C18.Base ${LIST}

clean:
	rm -f ${LOADF}

ll:
	ll ${LOADF}
