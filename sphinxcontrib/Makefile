PREFIX := ./

PROGRAMS := cldomain.sh

QUICKLISP_LOCATION := "(merge-pathnames \".quicklisp/setup.lisp\" (user-homedir-pathname))"

QUICKLISP_INIT := "(let ((quicklisp-init "${QUICKLISP_LOCATION}"))" \
    "(load quicklisp-init))"

ASDF_INIT := "(asdf:initialize-source-registry)"

APP_INIT := "(load \"${CURDIR}/cldomain.lisp\")"

CL_LAUNCH ?= cl-launch

LISP ?= sbcl
CL_LAUNCH_FLAGS ?=
CL_LAUNCH_FLAGS += --lisp '${LISP} sbcl clisp ccl'
CL_LAUNCH_FLAGS += --no-include

INSTALL_BIN ?= ${PREFIX}

all: $(PROGRAMS)

.init.lisp:
	echo ${QUICKLISP_INIT} > .init.lisp
	echo ${ASDF_INIT} >> .init.lisp

cldomain.sh: .init.lisp
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} --file .init.lisp --source-registry ${CURDIR} --system sphinxcontrib.cldomain --init "(load \"${CURDIR}/cldomain.lisp\")" --init "(sphinxcontrib.cldomain:main)" --output cldomain.sh
