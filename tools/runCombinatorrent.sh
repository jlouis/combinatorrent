#!/bin/sh

## Run combinatorent while gathering statistics. Use this script by symlinking it from the directory
## where you run tests in and then call it.

## Find the combinatorrent executable
thisscript=$(readlink -f $0)
basedir=$(dirname ${thisscript})
${COMBINATORRENT:=${basedir}/../dist/build/Combinatorrent/Combinatorrent}

## Set default temporary directory
${STATDIR:='/tmp/combinatorrent'}

RTSOPTS="+RTS -t${STATDIR}/Combinatorrent.rts_stat --machine-readable -RTS"

ct () {
    date +%s >> ${STATDIR}/Combinatorrent.times
    ${COMBINATORRENT} ${RTSOPTS} $*
    date +%s >> ${STATDIR}/Combinatorrent.times
}

        
cleanfiles () {
        rm -f ${STATDIR}/Combinatorrent.times
        rm -f ${STATDIR}/Combinatorrent.rts_stat
}

mkdir -p ${STATDIR}
cleanfiles
ct $*
