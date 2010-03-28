#!/bin/sh

## Run combinatorent while gathering statistics. Use this script by symlinking it from the directory
## where you run tests in and then call it.

## Find the combinatorrent executable
thisscript=$(readlink -f $0)
basedir=$(dirname ${thisscript})
COMBINATORRENT=${basedir}/../dist/build/Combinatorrent/Combinatorrent
POSTPROC=${basedir}/postproc.hs

## Set up default locations for statistics
DBFILE=${basedir}/stat_db.txt
STATDIR='/tmp/combinatorrent'

RTSOPTS="+RTS -t${STATDIR}/Combinatorrent.rts_stat --machine-readable -RTS"

ct () {
    date +%s >> ${STATDIR}/Combinatorrent.times
    touch ${STATDIR}/Combinatorrent.stat
    ${COMBINATORRENT} ${RTSOPTS} $*
    date +%s >> ${STATDIR}/Combinatorrent.times
}

postproc () {
        ${POSTPROC} gather ${STATDIR}/Combinatorrent.rts_stat ${STATDIR}/Combinatorrent.stat \
                ${STATDIR}/Combinatorrent.times >> ${DBFILE}
}

cleanfiles () {
        rm -f ${STATDIR}/Combinatorrent.times
        rm -f ${STATDIR}/Combinatorrent.rts_stat
        rm -f ${STATDIR}/Combinatorrent.stat
}

mkdir -p ${STATDIR}
cleanfiles
ct $*
postproc
