SVGZ=$1
LEVEL_DATA=$2

if [ "${LEVEL_DATA}" == "" ]
then
	LEVEL_DATA=`basename ${SVGZ} .svgz`.data
fi

gzcat ${SVGZ} | ./mk_level_data.byte > `basename ${LEVEL_DATA}`

