# Determine if the OS is Windows or a Unixoide
ifeq ($(OS),Windows_NT)
	OSFLAG := WINDOWS
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Linux)
		OSFLAG := UNIX
	endif
	ifeq ($(UNAME_S),Darwin)
		OSFLAG := UNIX
	endif
	ifeq ($(UNAME_S),FreeBSD)
		OSFLAG := UNIX
	endif
	ifeq ($(UNAME_S),OpenBSD)
		OSFLAG := UNIX
	endif
	ifeq ($(UNAME_S),SunOS)
		OSFLAG := UNIX
	endif
endif

# Determine What gradlew file to use
ifeq ($(OSFLAG), WINDOWS)
	GRADLEW := gradlew.bat
else ifeq ($(OSFLAG), UNIX)
	GRADLEW := ./gradlew
endif

all: build-fabric build-forge


# Give Gradlew files permission to be executed (Unix Only)
ifeq ($(OSFLAG),UNIX)
chmod:
	chmod +x ./fabric/gradlew
	chmod +x ./annotation-processor/gradlew
	chmod +x ./forge/gradlew
endif

build-fabric: chmod
	@echo "Building Fabric..."
	@cd fabric && $(GRADLEW) build

build-forge: chmod
	@echo "Building Forge..."
	@cd forge && $(GRADLEW) build
