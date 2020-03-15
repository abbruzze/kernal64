#!/bin/bash
###############################################################################

NAME=$0

# defaults for global variables set by commandline
target=""
filter=""
verbose=0
resume=0
# extra options
videotype=""
videosubtype=""
sidtype=""
ciatype=""
memoryexpansion=""

# globals set by utility functions that can be used in the hooks
refscreenshotname=""

source "./chameleon-hooks.sh"
source "./u64-hooks.sh"
source "./cham20-hooks.sh"
source "./c64rmk2-hooks.sh"
source "./x64-hooks.sh"
source "./x64sc-hooks.sh"
source "./x128-hooks.sh"
source "./x128c64-hooks.sh"
source "./xscpu64-hooks.sh"
source "./x64dtv-hooks.sh"
source "./xpet-hooks.sh"
source "./xcbm2-hooks.sh"
source "./xcbm5x0-hooks.sh"
source "./xplus4-hooks.sh"
source "./xvic-hooks.sh"
source "./vsid-hooks.sh"
source "./hoxs64-hooks.sh"
source "./micro64-hooks.sh"
source "./emu64-hooks.sh"
source "./yace-hooks.sh"
source "./z64kc64-hooks.sh"
source "./z64kc128-hooks.sh"
source "./z64kc128c64-hooks.sh"
source "./z64kvic20-hooks.sh"
source "./denise-hooks.sh"
source "./kernal64c64-hooks.sh"
source "./kernal64c128c64-hooks.sh"
source "./kernal64c128c128-hooks.sh"

###############################################################################

function checktarget
{
    case "$1" in
    # C64 targets
        x64)
                target="$1"
            ;;
        x64sc)
                target="$1"
            ;;
        x128c64)
                target="$1"
            ;;
        chameleon)
                target="$1"
            ;;
        u64)
                target="$1"
            ;;
        c64rmk2)
                target="$1"
            ;;
        hoxs64)
                target="$1"
            ;;
        micro64)
                target="$1"
            ;;
        emu64)
                target="$1"
            ;;
        yace)
                target="$1"
            ;;
        z64kc64)
                target="$1"
            ;;
        z64kc128c64)
                target="$1"
            ;;
    	kernal64c64)
	            target="$1"
	    ;;
        kernal64c128c64)
	            target="$1"
	    ;;
        kernal64c128c128)
	            target="$1"
	    ;;
        denise)
                target="$1"
            ;;
    # C128 targets
        x128)
                target="$1"
            ;;
        z64kc128)
                target="$1"
            ;;
    # SCPU targets
        xscpu64)
                target="$1"
            ;;
    # PET targets
        xpet)
                target="$1"
            ;;
    # CBM2 / CBM500 targets
        xcbm5x0)
                target="$1"
            ;;
    # CBM2 / CBM600 targets
        xcbm2)
                target="$1"
            ;;
    # VIC20 targets
        xvic)
                target="$1"
            ;;
        cham20)
                target="$1"
            ;;
        z64kvic20)
                target="$1"
            ;;
    # Plus4 targets
        xplus4)
                target="$1"
            ;;
    # DTV targets
        x64dtv)
                target="$1"
            ;;
    # SID-player targets
        vsid)
                target="$1"
            ;;
        *)
            echo "error:" "$1" "is not a valid target. (--help to get help)"
            exit -1
            ;;
    esac
}

# $1  test path
# $2  test program name
# $3  mounted crt
function getscreenshotname
{
    refscreenshottest=""
    refscreenshotname=""
    screenshot_videosubtype=""
    
    if [ "$2" == "" ] ; then
        refscreenshottest="$3"
    else
        refscreenshottest="$2"
    fi
    
    if [ "$videosubtype" != "" ] ; then
        # if a subtype was given on cmdline, use that
        screenshot_videosubtype=$videosubtype
    else
        # if subtype was not given on cmdline, use the default value for this emu
        if [ "$emu_default_videosubtype" != "" ] ; then
            # if the default is a specific type, use that for the screenshot
            screenshot_videosubtype=$emu_default_videosubtype
        fi
    fi

    # if the testprog is NTSC, and either the default or the cmdline is a PAL chip, change
    # the screenshot type to the respective NTSC chip
    if [ "$testprogvideotype" == "NTSC" ]; then
        if [ "$screenshot_videosubtype" == "8565" ]; then
            screenshot_videosubtype="8562"
        else
            if [ "$screenshot_videosubtype" == "8565early" ]; then
                screenshot_videosubtype="8562early"
            else
                if [ "$screenshot_videosubtype" == "8565late" ]; then
                    screenshot_videosubtype="8562late"
                fi
            fi
        fi
    fi

    # if the testprog is PAL, and either the default or the cmdline is a NTSC chip, change
    # the screenshot type to the respective PAL chip
    if [ "$testprogvideotype" == "PAL" ]; then
        if [ "$screenshot_videosubtype" == "8562" ]; then
            screenshot_videosubtype="8565"
        else
            if [ "$screenshot_videosubtype" == "8562early" ]; then
                screenshot_videosubtype="8565early"
            else
                if [ "$screenshot_videosubtype" == "8562late" ]; then
                    screenshot_videosubtype="8565late"
                fi
            fi
        fi
    fi

    if [ "$screenshot_videosubtype" != "" ] ; then
        if [ -f "$1"/references/"$refscreenshottest"-"$screenshot_videosubtype".png ]
        then
            refscreenshotname="$1"/references/"$refscreenshottest"-"$screenshot_videosubtype".png
            return 0
        fi
        # if the exact subtype could not be found, try more general one (PAL)
        if [ "$screenshot_videosubtype" == "8565early" ] || [ "$screenshot_videosubtype" == "8565late" ]; then
            if [ -f "$1"/references/"$refscreenshottest"-"8565".png ]
            then
                refscreenshotname="$1"/references/"$refscreenshottest"-"8565".png
                return 0
            fi
        fi
        # if the exact subtype could not be found, try more general one (NTSC)
        if [ "$screenshot_videosubtype" == "8562early" ] || [ "$screenshot_videosubtype" == "8562late" ]; then
            if [ -f "$1"/references/"$refscreenshottest"-"8562".png ]
            then
                refscreenshotname="$1"/references/"$refscreenshottest"-"8562".png
                return 0
            fi
        fi
    fi

    if [ "$testprogvideotype" == "NTSC" ] && [ -f "$1"/references/"$refscreenshottest"-ntsc.png ]
    then
        refscreenshotname="$1"/references/"$refscreenshottest"-ntsc.png
        return 0
    fi

    if [ "$testprogvideotype" == "NTSCOLD" ] && [ -f "$1"/references/"$refscreenshottest"-ntscold.png ]
    then
        refscreenshotname="$1"/references/"$refscreenshottest"-ntscold.png
        return 0
    fi

    if [ -f "$1"/references/"$refscreenshottest".png ]
    then
        refscreenshotname="$1"/references/"$refscreenshottest".png
        return 0
    fi
    return 255
}

###############################################################################

# read the list of tests for the given target
function gettestsfortarget
{
#    echo "reading list of tests for" "$1".
# readarray does only work on bash4 (not in mingw)
#    readarray -t testlist < "$1"-testlist.txt
    IFS=$'\n' read -d '' -r -a testlist < "$1"-testlist.txt
}

# read the existing list of results for the given target
function getresultsfortarget
{
    if [ "${resume}" == "1" ]; then
#    echo "reading list of tests for" "$1".
# readarray does only work on bash4 (not in mingw)
#    readarray -t resultlist < "$1"-results.txt
        IFS=$'\n' read -d '' -r -a resultlist < "$1"-result.txt
    fi
}

###############################################################################

# reset all flags used for options per test
function resetflags
{
    isepic_enabled=0
    fullbanks_enabled=0
    ramcart_enabled=0
    reu_enabled=0
    georam_enabled=0
    dqbb_enabled=0
    plus60k_enabled=0
    plus256k_enabled=0
    extfuncram_enabled=0
    intfuncram_enabled=0
    memory_expansion_enabled=0

    testprogvideotype=-1
    testprogvideosubtype=-1
    new_sid_enabled=-1
    new_cia_enabled=-1
    
    mounted_d64=""
    mounted_g64=""
    mounted_p64=""
    mounted_crt=""
}

###############################################################################

# the results file is a simple comma-seperated list
#
# 1) path of the test
# 2) executable name of the test
# 3) exit status (0:ok, $ff:error, 1:timeout, noref)
# 4) type of the test (exitstatus,screenshot,interactive,analyzer)
# 5) mounted d64 (if any)
# 6) mounted g64 (if any)
# 7) mounted crt (if any)
# 8) CIA type flag
# 9) SID type flag

function resultstartlog
{
    RESULT_LOG_NAME="$target"
    RESULT_LOG_NAME+="-result.txt"
    if [ "${resume}" == "0" ]; then
        rm -f "$RESULT_LOG_NAME"
    fi
}

# $1 - path
# $2 - exe name
# $3 - status
# $4 - test type

#FIXME: p64 is not included
function resultprintline
{
    echo "$1","$2","$3","$4","$mounted_d64","$mounted_g64","$mounted_crt","${new_cia_enabled}","${new_sid_enabled}","${testprogvideotype}" >> "$RESULT_LOG_NAME"
}

function resultstoplog
{
    echo "$1""$2""$3""$4" >> "$RESULT_LOG_NAME"
}

# check if a result exists in the resultfile already
# $1 - path
# $2 - exe name
# $3 - status
# $4 - test type

#FIXME: p64 is not included
function resultfind
{
    echo "find:""$1","$2","$3","$4","$mounted_d64","$mounted_g64","$mounted_crt","${new_cia_enabled}","${new_sid_enabled}","${testprogvideotype}"
    for re in "${resultlist[@]}"
    do
#        echo "$re"
        if [ "${re:0:1}" != "#" ]; then
            IFS=',' read -a rarray <<< "$re"
            echo "check:""${rarray[0]}","${rarray[1]}","${rarray[2]}","${rarray[3]}","${rarray[4]}","${rarray[5]}","${rarray[6]}","${rarray[7]}","${rarray[8]}","${rarray[9]}"
            if [ x"$1"x == x"${rarray[0]}"x ] && [ x"$2"x == x"${rarray[1]}"x ] &&
               [ x"$4"x == x"${rarray[3]}"x ] && [ x"$mounted_d64"x == x"${rarray[4]}"x ] &&
               [ x"$mounted_g64"x == x"${rarray[5]}"x ] && [ x"$mounted_crt"x == x"${rarray[6]}"x ] &&
               [ x"${new_cia_enabled}"x == x"${rarray[7]}"x ] && [ x"${new_sid_enabled}"x == x"${rarray[8]}"x ] &&
               [ x"${testprogvideotype}"x == x"${rarray[9]}"x ]
            then
                echo "found:""${rarray[0]}","${rarray[1]}","${rarray[2]}","${rarray[3]}","${rarray[4]}","${rarray[5]}","${rarray[6]}","${rarray[7]}","${rarray[8]}","${rarray[9]}"
                return 1
            fi
        fi
    done
    return 0
}

###############################################################################

# $1 - target
# $2 - filter substring
function runprogsfortarget
{
#    checktarget "$1"
#    if [ "$2" == "" ]; then
#        echo "running tests for" "$target"":"
#    else
#        echo "running tests for" "$target" "(""$2"")"":"
#    fi

    gettestsfortarget "$target"
    getresultsfortarget "$target"
    resultstartlog

    "$target"_prepare

    for e in "${testlist[@]}"
    do
#        echo "$e"
        resetflags

        if [ "${e:0:1}" != "#" ]; then
            IFS=',' read -a myarray <<< "$e"
#            echo line:${e}
#            echo 1:${myarray[0]}
#            echo 2:${myarray[1]}
#            echo 3:${myarray[2]}
#            echo 4:${myarray[3]}
#            echo 5:${myarray[4]}
            arraylength=${#myarray[@]}
            if [ "$arraylength" -lt "4" ]; then
                echo "error: unexpected end of line in input (arraylenght=${arraylength})"
                echo "line:${e}"
                exit -1
            fi

            testpath="${myarray[0]}"
            testprog="${myarray[1]}"
            testtype="${myarray[2]}"
            testtimeout="${myarray[3]}"
            progpath=${testpath}${testprog}

#            echo " path: $testpath"
#            echo " program: $testprog"
#            echo " type: $testtype"
#            echo " timeout: $testtimeout"
#            echo " options: $testoptions"

            skiptest=0
            if [ "$2" == "" ] || [ "${progpath#*$2}" != "$progpath" ]; then
                # create the commandline for the target...
                testoptions=""
                # first loop over the options given for the test
                for (( i=5; i<${arraylength}+1; i++ ));
                do
#                    echo $i " / " ${arraylength} " : " ${myarray[$i-1]}
                    "$target"_get_options "${myarray[$i-1]}" "$testpath"
#                    echo "exitoptions: $exitoptions"
#                    echo "testprogvideotype: ${testprogvideotype}"
#                    echo "memoryexpansion:"  "${memoryexpansion}"
                    testoptions+="${exitoptions} "
                    # skip test if videomode was given on commandline and it does
                    # not match the videomode given in the testlist
                    if [ "${videotype}" == "PAL" ]; then
                        if [ "${testprogvideotype}" == "NTSC" ] || [ "${testprogvideotype}" == "NTSCOLD" ]; then
                            echo "$testpath" "$testprog" "- " "not" "${videotype}" "(skipped)"
                            skiptest=1
                        fi
                    fi
                    if [ "${videotype}" == "NTSC" ]; then
                        if [ "${testprogvideotype}" == "PAL" ] || [ "${testprogvideotype}" == "NTSCOLD" ]; then
                            echo "$testpath" "$testprog" "- " "not" "${videotype}" "(skipped)"
                            skiptest=1
                        fi
                    fi
                    if [ "${videotype}" == "NTSCOLD" ]; then
                        if [ "${testprogvideotype}" == "NTSC" ] || [ "${testprogvideotype}" == "PAL" ]; then
                            echo "$testpath" "$testprog" "- " "not" "${videotype}" "(skipped)"
                            skiptest=1
                        fi
                    fi
                    # skip test if SID type was given on commandline and it does not match
                    if [ "${sidtype}" == "6581" ]; then
                        if [ x"${new_sid_enabled}"x == x"1"x ]; then
                            echo "$testpath" "$testprog" "- " "not" "${sidtype}" "(skipped)"
                            skiptest=1
                        fi
                    fi
                    if [ "${sidtype}" == "8580" ]; then
                        if [ x"${new_sid_enabled}"x == x"0"x ]; then
                            echo "$testpath" "$testprog" "- " "not" "${sidtype}" "(skipped)"
                            skiptest=1
                        fi
                    fi
                    # skip test if CIA type was given on commandline and it does not match
                    if [ "${ciatype}" == "6526" ]; then
                        if [ x"${new_cia_enabled}"x == x"1"x ]; then
                            echo "$testpath" "$testprog" "- " "not" "${ciatype}" "(skipped)"
                            skiptest=1
                        fi
                    fi
                    if [ "${ciatype}" == "6526A" ]; then
                        if [ x"${new_cia_enabled}"x == x"0"x ]; then
                            echo "$testpath" "$testprog" "- " "not" "${ciatype}" "(skipped)"
                            skiptest=1
                        fi
                    fi
                    # skip test if memory expansion type was given on commandline and it does not match
                    if [ "${memoryexpansion}" == "8K" ]; then
                        if [ x"${memory_expansion_enabled}"x != x"8K"x ]; then
                            echo "$testpath" "$testprog" "- " "not" "${memoryexpansion}" "(skipped)"
                            skiptest=1
                        fi
                    fi
                    if [ "${memoryexpansion}" == "32K" ]; then
                        if [ x"${memory_expansion_enabled}"x != x"32K"x ]; then
                            echo "$testpath" "$testprog" "- " "not" "${memoryexpansion}" "(skipped)"
                            skiptest=1
                        fi
                    fi
                done
                # now setup additional options depending on commandline options given to the testbench script
                if [ "${videotype}" == "PAL" ]; then
                    "$target"_get_cmdline_options "PAL"
                    testoptions+="${exitoptions} "
                fi
                if [ "${videotype}" == "NTSC" ]; then
                    "$target"_get_cmdline_options "NTSC"
                    testoptions+="${exitoptions} "
                fi
                if [ "${videotype}" == "NTSCOLD" ]; then
                    "$target"_get_cmdline_options "NTSCOLD"
                    testoptions+="${exitoptions} "
                fi
                if [ "${ciatype}" == "6526" ]; then
                    "$target"_get_cmdline_options "6526"
                    testoptions+="${exitoptions} "
                fi
                if [ "${ciatype}" == "6526A" ]; then
                    "$target"_get_cmdline_options "6526A"
                    testoptions+="${exitoptions} "
                fi
                if [ "${videosubtype}" == "6569" ]; then
                    "$target"_get_cmdline_options "6569"
                    testoptions+="${exitoptions} "
                fi
                if [ "${videosubtype}" == "8565" ]; then
                    "$target"_get_cmdline_options "8565"
                    testoptions+="${exitoptions} "
                fi
                if [ "${videosubtype}" == "8562" ]; then
                    "$target"_get_cmdline_options "8562"
                    testoptions+="${exitoptions} "
                fi
                if [ "${memoryexpansion}" == "8K" ]; then
                    "$target"_get_cmdline_options "8K"
                    testoptions+="${exitoptions} "
                fi
                if [ "${memoryexpansion}" == "32K" ]; then
                    "$target"_get_cmdline_options "32K"
                    testoptions+="${exitoptions} "
                fi
            if [ "${skiptest}" == "0" ] && [ "${testtype}" == "interactive" ]; then
                echo "$testpath" "$testprog" "- " "interactive (skipped)"
                skiptest=1
            fi
            if [ "${skiptest}" == "0" ] && [ "${testtype}" == "analyzer" ]; then
                echo "$testpath" "$testprog" "- " "analyzer (skipped)"
                skiptest=1
            fi
            if [ "${resume}" == "1" ] &&  [ "${skiptest}" == "0" ] && [ "${testtype}" != "interactive" ] && [ "${testtype}" != "analyzer" ]; then
                resultfind "$testpath" "$testprog" "$exitstatus" "${testtype}"
                if [ "$?" == "1" ]; then
                    echo "$testpath" "$testprog" "(skipped)"
                    skiptest=1
                fi
            fi
            if [ "${skiptest}" == "0" ]; then
#                if [ "$2" == "" ] || [ "${testpath#*$2}" != "$testpath" ]; then
                    echo -ne "$testpath" "$testprog" "- "
                    if [ "${verbose}" == "1" ]; then
                        echo -ne ["${testtype}"]
                    fi

                    if [ "${testtype}" == "screenshot" ]
                    then
                        getscreenshotname "$testpath" "$testprog" "$mounted_crt"
                        refscreenshotvideotype="PAL"
                        if [ "${refscreenshotname#*_ntsc.prg}" != "$refscreenshotname" ] || 
                           [ "${refscreenshotname#*_ntsc-8562.prg}" != "$refscreenshotname" ] ||
                           [ "${refscreenshotname#*_ntsc-8562early.png}" != "$refscreenshotname" ]
                        then
                            refscreenshotvideotype="NTSC"
                        fi
                        if [ "${refscreenshotname#*_ntscold.prg}" != "$refscreenshotname" ] || 
                           [ "${refscreenshotname#*-ntscold.png}" != "$refscreenshotname" ]
                        then
                            refscreenshotvideotype="NTSCOLD"
                        fi
                        if [ "${verbose}" == "1" ]; then
                            echo -ne ["$refscreenshotvideotype": "$refscreenshotname"]
                        fi
                    fi

                    if [ "${testtype}" == "screenshot" ] && [ "$refscreenshotname" == "" ]
                    then
                        echo "reference screenshot missing (skipped)"
                        resultprintline "$testpath" "$testprog" "noref" "${testtype}"
                    else
                    
                        # make sure the full name is empty when there is no program file
                        # (ie dont just pass a path)
                        if [ "x"$testprog"x" == "x""x" ]; then
                            testprogfullname=""
                        else
                            testprogfullname="$testpath/$testprog"
                        fi
                    
#                        echo "$target"_run_"$testtype" "$testpath" "$testprog" "$testtimeout" "$testoptions"
                        "$target"_run_"$testtype" "$testpath" "$testprog" "$testtimeout" "$testprogfullname" "$testoptions"
#                        echo "exited with: " $exitcode
                        GREEN='\033[1;32m'
                        RED='\033[1;31m'
                        NC='\033[0m'
                        case "$exitcode" in
                            0)
                                    echo -ne $GREEN
                                    exitstatus="ok"
                                ;;
                            1)
                                    echo -ne $RED
                                    exitstatus="timeout"
                                ;;
                            255)
                                    echo -ne $RED
                                    exitstatus="error"
                                ;;
                            *)
                                    echo -ne $RED
                                    exitstatus="error"
                                ;;
                        esac
                        echo -e "$exitstatus" $NC
                        resultprintline "$testpath" "$testprog" "$exitstatus" "${testtype}"
                    fi
                fi
            fi
        fi
    done
}

function showfailedfortarget
{
    checktarget "$1"
    if [ -f "$target"_result.txt ]; then
        if [ `grep -v ok "$target"_result.txt | wc -l ` -eq 0 ]; then
            echo "no test(s) failed for" "$target""."
        else
            echo "failed tests for" "$target"":"
            grep "error" "$target"_result.txt
            grep "timeout" "$target"_result.txt
            grep "noref" "$target"_result.txt
        fi
    fi
}

###############################################################################
function showhelp
{
    echo $NAME" - run test programs."
    echo "usage: "$NAME" [target] <filter> <options>"
    echo "  targets: x64, x64sc, x128c64, x128, xscpu64, x64dtv, xpet, xcbm2, xcbm5x0, xvic, xplus4, vsid,"
    echo "           chameleon, u64, cham20, c64rmk2, hoxs64, micro64, emu64, yace, z64kc64, z64kc128, z64kc128c64, z64kvic20, denise"
    echo "  <filter> is a substring of the path of tests to restrict to"
    echo "  --help       show this help"
    echo "  --verbose    be more verbose"
    echo "  --resume     resume previously aborted testrun"
    echo "  --pal        run tests in PAL, skip tests that do not work on PAL"
    echo "  --ntsc       run tests in NTSC, skip tests that do not work on NTSC"
    echo "  --ntscold    run tests in NTSC(old), skip tests that do not work on NTSC(old)"
    echo "  --ciaold     run tests on 'old' CIA, skip tests that do not work on 'new' CIA"
    echo "  --cianew     run tests on 'new' CIA, skip tests that do not work on 'old' CIA"
    echo "  --6581       run tests on 6581 (old SID), skip tests that do not work on 8580 (new SID)"
    echo "  --8580       run tests on 8580 (new SID), skip tests that do not work on 6581 (old SID)"
    echo "  --6569       target VICII type is 6569 (PAL)"
    echo "  --6567       target VICII type is 6567 (NTSC)"
    echo "  --8562       target VICII type is 8562 (NTSC, grey dot)"
    echo "  --8562early  target VICII type is 8562 (NTSC, new color instead of grey dot)"
    echo "  --8562late   target VICII type is 8562 (NTSC, old color instead of grey dot)"
    echo "  --8565       target VICII type is 8565 (PAL, grey dot)"
    echo "  --8565early  target VICII type is 8565 (PAL, new color instead of grey dot)"
    echo "  --8565late   target VICII type is 8565 (PAL, old color instead of grey dot)"
    echo "  --8k         skip tests that do not work with 8k RAM expansion (VIC20)"
}

function checkparams
{
    if [ "$target" == "" ]; then
        echo "error: no valid target given (--help to get help)"
        exit -1
    fi
}
###############################################################################

for thisarg in "$@"
do
#    echo "arg:" "$thisarg"
    case "$thisarg" in
        --help)
                showhelp
                exit 0
            ;;
        --verbose)
                verbose=1
            ;;
        --resume)
                resume=1
            ;;
        --pal)
                videotype="PAL"
            ;;
        --ntsc)
                videotype="NTSC"
            ;;
        --ntscold)
                videotype="NTSCOLD"
            ;;
        --6581) # "old" SID
                sidtype="6581"
            ;;
        --8580) # "new" SID
                sidtype="8580"
            ;;
        --ciaold) # "old" CIA
                ciatype="6526"
            ;;
        --cianew) # "new" CIA
                ciatype="6526A"
            ;;
        --6569) # PAL VICII
                videosubtype="6569"
            ;;
        --8565) # "new" PAL VICII (grey dot)
                videosubtype="8565"
            ;;
        --8565early) # "new" PAL VICII (no grey dot, new color instead)
                videosubtype="8565early"
            ;;
        --8565late) # "new" PAL VICII (no grey dot, old color instead)
                videosubtype="8565late"
            ;;
        --6567) # NTSC VICII
                videosubtype="6567"
            ;;
        --8562) # "new" NTSC VICII (grey dot)
                videosubtype="8562"
            ;;
        --8562early) # "new" NTSC VICII (no grey dot, new color instead)
                videosubtype="8562early"
            ;;
        --8562late) # "new" NTSC VICII (no grey dot, old color instead)
                videosubtype="8562late"
            ;;
        --8k) # 8k RAM expansion
                memoryexpansion="8K"
            ;;
        *) # is either target or filter
            if [ "${thisarg:0:2}" == "--" ]; then
                echo "error: unknown option '"$thisarg"'."
                exit -1
            fi
            if [ "$thisarg" = "" ] ; then
                showhelp
                exit -1
            fi
            # try to set target if no target set
            if [ "$target" = "" ] ; then
                checktarget "$thisarg"
            else
            # if target is set, set filter
                filter="$thisarg"
            fi
            ;;
    esac
    
done

if [ "$EMUDIR" == "" ] ; then
    case "$target" in
        x64)
                EMUDIR="../../trunk/vice/src/"
            ;;
        x64sc)
                EMUDIR="../../trunk/vice/src/"
            ;;
        x128c64)
                EMUDIR="../../trunk/vice/src/"
            ;;
        x128)
                EMUDIR="../../trunk/vice/src/"
            ;;
        xscpu64)
                EMUDIR="../../trunk/vice/src/"
            ;;
        xpet)
                EMUDIR="../../trunk/vice/src/"
            ;;
        xcbm5x0)
                EMUDIR="../../trunk/vice/src/"
            ;;
        xcbm2)
                EMUDIR="../../trunk/vice/src/"
            ;;
        xvic)
                EMUDIR="../../trunk/vice/src/"
            ;;
        xplus4)
                EMUDIR="../../trunk/vice/src/"
            ;;
        x64dtv)
                EMUDIR="../../trunk/vice/src/"
            ;;
        vsid)
                EMUDIR="../../trunk/vice/src/"
            ;;
    esac
else
# if EMUDIR is not empty, make sure it ends with a path seperator
    EMUDIR+="/"
fi

if [ "$verbose" = "1" ] ; then
    echo target:"$target"
    echo filter:"$filter"
    echo verbose:"$verbose"
    echo "video type:" "$videotype"
    echo "video subtype:" "$videosubtype"
    echo "SID type:" "$sidtype"
    echo "CIA type:" "$ciatype"
    echo "memory expansion:" "$memoryexpansion"
    echo "using EMUDIR="$EMUDIR
fi

checkparams

"$target"_check_environment

if [ "$filter" == "" ]; then
    echo "running tests for" "$target" "(all):"
else
    echo "running tests for" "$target" "(""$filter"")"":"
fi

make prereq

SECONDS=0

runprogsfortarget "$target" "$filter"

duration=$SECONDS
echo "$(($duration / 60)) minutes and $(($duration % 60)) seconds elapsed."

resultstoplog "# $(($duration / 60)) minutes and $(($duration % 60)) seconds elapsed."

showfailedfortarget "$target"

exit 0;
