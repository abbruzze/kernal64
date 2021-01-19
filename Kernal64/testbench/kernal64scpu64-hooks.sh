KERNAL64SCPU64OPTS+=" --testcart"
KERNAL64SCPU64OPTS+=" --headless"
KERNAL64SCPU64OPTS+=" --warp"
KERNAL64SCPU64OPTS+=" --vic-palette pepto"
KERNAL64SCPU64OPTS+=" --screen-dim 1"
KERNAL64SCPU64OPTS+=" --cpujam-continue true"
KERNAL64SCPU64OPTS+=" --sid-cycle-exact"
KERNAL64SCPU64OPTS+=" --ignore-config-file"

function kernal64scpu64_check_environment
{
    KERNAL64SCPU64=$EMUDIR/kscpu64.sh
    
    if ! [ -x "$(command -v $JAVA_HOME/bin/java)" ]; then
        echo 'Error: java not installed.' >&2
        exit 1
    fi
    
}

# $1  option
# $2  test path
function kernal64scpu64_get_options
{
    exitoptions=""
    case "$1" in
        "default")
                exitoptions=""
            ;;
        "vicii-pal")
                exitoptions=""
                testprogvideotype="PAL"
            ;;
        "vicii-ntsc")
            exitoptions="--ntsc true --screen-dim 1"
            testprogvideotype="NTSC"
        ;;
        "sid-old")
                new_sid_enabled=0
            ;;
        "sid-new")
                exitoptions="--sid-8580 true"
                new_sid_enabled=1
            ;;
        "reu512k")
                exitoptions="--reu-type 512"
                reu_enabled=1
            ;;
        "geo256k")
                exitoptions="--geo-ram 256"
                georam_enabled=1
            ;;
        "cia-old")
                exitoptions="--cia-model 6526"
                new_cia_enabled=0
            ;;
        "cia-new")
                exitoptions="--cia-model 8521"
                new_cia_enabled=1
            ;;
        *)
                exitoptions=""
                if [ "${1:0:9}" == "mountd64:" ]; then
                    exitoptions="--drive8-file $2/${1:9}"
                    mounted_d64="${1:9}"
                    echo -ne "(disk:${1:9}) "
                fi
                if [ "${1:0:9}" == "mountg64:" ]; then
                    exitoptions="--drive8-file $2/${1:9}"
                    mounted_g64="${1:9}"
                    echo -ne "(disk:${1:9}) "
                fi
                if [ "${1:0:9}" == "mountcrt:" ]; then
                    exitoptions="--cart $2/${1:9}"
                    mounted_crt="${1:9}"
                    echo -ne "(cartridge:${1:9}) "
                fi
            ;;
    esac
}


# $1  option
# $2  test path
function kernal64scpu64_get_cmdline_options
{
    exitoptions=""
}

# called once before any tests run
function kernal64scpu64_prepare
{
    true
}

################################################################################
# reset
# run test program
# exit when write to $d7ff occurs - the value written determines success (=$00) or fail (=$ff)
# exit after $timeout cycles (exitcode=$01)
# save a screenshot at exit - success or failure is determined by comparing screenshots

# $1  test path
# $2  test program name
# $3  timeout cycles
# $4  test full path+name (may be empty)
# $5- extra options for the emulator
function kernal64scpu64_run_screenshot
{
    if [ "$2" == "" ] ; then
        screenshottest="$mounted_crt"
    else
        screenshottest="$2"
    fi

    mkdir -p "$1"/".testbench"
    rm -f "$1"/.testbench/"$screenshottest"-kernal64c64.png
    if [ $verbose == "1" ]; then
        echo "RUN: "$KERNAL64SCPU64 $KERNAL64SCPU64OPTS $KERNAL64SCPU64OPTSSCREENSHOT ${@:5} "--limitcycles" "$3" "--screenshot" "$1"/.testbench/"$screenshottest"-kernal64c64.png "$4"
    fi
    $KERNAL64SCPU64 $KERNAL64SCPU64OPTS $KERNAL64SCPU64OPTSSCREENSHOT ${@:5} "--limitcycles" "$3" "--screenshot" "$1"/.testbench/"$screenshottest"-kernal64c64.png "$4" 1> /dev/null 2> /dev/null
    exitcode=$?
#    echo exitcode:$exitcode
    if [ $exitcode -ne 0 ]
    then
        if [ $exitcode -ne 1 ]
        then
            if [ $exitcode -ne 255 ]
            then
                echo -ne "\nerror: call to $KERNAL64SCPU64 failed.\n"
#                exit -1
            fi
        fi
    fi
    if [ $exitcode -eq 0 ]
    then
        if [ -f "$refscreenshotname" ]
        then
        
            # defaults for PAL
            KERNAL64SCPU64REFSXO=32
            KERNAL64SCPU64REFSYO=35
            KERNAL64SCPU64SXO=32
            KERNAL64SCPU64SYO=35

            if [ "${refscreenshotvideotype}" == "NTSC" ]; then
                KERNAL64SCPU64REFSXO=32
                KERNAL64SCPU64REFSYO=23
                KERNAL64SCPU64SXO=32
                KERNAL64SCPU64SYO=23
            fi

            # when either the testbench was run with --ntsc, or the test is ntsc-specific,
            # then we need the offsets on the NTSC screenshot
            if [ "${videotype}" == "NTSC" ] || [ "${testprogvideotype}" == "NTSC" ]; then
                KERNAL64SCPU64REFSXO=32
                KERNAL64SCPU64REFSYO=23
                KERNAL64SCPU64SXO=32
                KERNAL64SCPU64SYO=23
            fi
        
            if [ $verbose == "1" ]; then
                echo ./cmpscreens "$refscreenshotname" "$KERNAL64SCPU64REFSXO" "$KERNAL64SCPU64REFSYO" "$1"/.testbench/"$screenshottest"-kernal64c64.png "$KERNAL64SCPU64SXO" "$KERNAL64SCPU64SYO"
            fi
            ./cmpscreens "$refscreenshotname" "$KERNAL64SCPU64REFSXO" "$KERNAL64SCPU64REFSYO" "$1"/.testbench/"$screenshottest"-kernal64c64.png "$KERNAL64SCPU64SXO" "$KERNAL64SCPU64SYO"
            exitcode=$?
        else
            echo -ne "reference screenshot missing - "
            exitcode=255
        fi
    fi
    if [ $verbose == "1" ]; then
        echo "cmpscreen exited with: " $exitcode
    fi
}

################################################################################
# reset
# run test program
# exit when write to $d7ff occurs - the value written determines success (=$00) or fail (=$ff)
# exit after $timeout cycles (exitcode=$01)

# $1  test path
# $2  test program name
# $3  timeout cycles
# $4  test full path+name (may be empty)
# $5- extra options for the emulator
function kernal64scpu64_run_exitcode
{
    if [ $verbose == "1" ]; then
        echo "RUN: "$KERNAL64SCPU64 $KERNAL64SCPU64OPTS $KERNAL64SCPU64OPTSEXITCODE ${@:5} "--limitcycles" "$3" "$4"
    fi
    $KERNAL64SCPU64 $KERNAL64SCPU64OPTS $KERNAL64SCPU64OPTSEXITCODE ${@:5} "--limitcycles" "$3" "$4" 1> /dev/null 2> /dev/null
    exitcode=$?
    #echo EXIT CODE for $2 is $exitcode
    if [ $exitcode -ne 0 ]
    then
        if [ $exitcode -ne 1 ]
        then
            if [ $exitcode -ne 255 ]
            then
                echo -ne "\nerror: call to $KERNAL64SCPU64 failed.\n"
#                exit -1
            fi
        fi
    fi
}
