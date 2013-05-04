SVGZ=$1
LEVEL_DATA=$2

if [ "${LEVEL_DATA}" == "" ]
then
	LEVEL_DATA=`basename ${SVGZ} .svgz`.data
fi

zcat ${SVGZ} | ocaml mk-level-data.ml > ${LEVEL_DATA}

