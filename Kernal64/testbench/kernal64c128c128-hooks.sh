KERNAL64C128C128OPTS+=" --testcart"
KERNAL64C128C128OPTS+=" --headless"
KERNAL64C128C128OPTS+=" --warp"
KERNAL64C128C128OPTS+=" --vic-palette pepto"
KERNAL64C128C128OPTS+=" --screen-dim 1"
KERNAL64C128C128OPTS+=" --cpujam-continue true"

function kernal64c128c128_check_environment
{
    KERNAL64C128C128=$EMUDIR/k128.sh
    
    if ! [ -x "$(command -v $JAVA_HOME/bin/java)" ]; then
        echo 'Error: java not installed.' >&2
        exit 1
    fi
    
}

# $1  option
# $2  test path
function kernal64c128c128_get_options
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
function kernal64c128c128_get_cmdline_options
{
    exitoptions=""
}

# called once before any tests run
function kernal64c128c128_prepare
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
function kernal64c128c128_run_screenshot
{
    if [ "$2" == "" ] ; then
        screenshottest="$mounted_crt"
    else
        screenshottest="$2"
    fi

    mkdir -p "$1"/".testbench"
    rm -f "$1"/.testbench/"$screenshottest"-kernal64C128C128.png
    if [ $verbose == "1" ]; then
        echo "RUN: "$KERNAL64C128C128 $KERNAL64C128C128OPTS $KERNAL64C128C128OPTSSCREENSHOT ${@:5} "--limitcycles" "$3" "--screenshot" "$1"/.testbench/"$screenshottest"-kernal64C128C128.png "$4"
    fi
    $KERNAL64C128C128 $KERNAL64C128C128OPTS $KERNAL64C128C128OPTSSCREENSHOT ${@:5} "--limitcycles" "$3" "--screenshot" "$1"/.testbench/"$screenshottest"-kernal64C128C128.png "$4" 1> /dev/null 2> /dev/null
    exitcode=$?
#    echo exitcode:$exitcode
    if [ $exitcode -ne 0 ]
    then
        if [ $exitcode -ne 1 ]
        then
            if [ $exitcode -ne 255 ]
            then
                echo -ne "\nerror: call to $KERNAL64C128C128 failed.\n"
                exit -1
            fi
        fi
    fi
    if [ $exitcode -eq 0 ]
    then
        if [ -f "$refscreenshotname" ]
        then
        
            # defaults for PAL
            KERNAL64C128C128REFSXO=32
            KERNAL64C128C128REFSYO=35
            KERNAL64C128C128SXO=32
            KERNAL64C128C128SYO=35
        
            echo ./cmpscreens "$refscreenshotname" "$KERNAL64C128C128REFSXO" "$KERNAL64C128C128REFSYO" "$1"/.testbench/"$screenshottest"-kernal64C128C128.png "$KERNAL64C128C128SXO" "$KERNAL64C128C128SYO"
            ./cmpscreens "$refscreenshotname" "$KERNAL64C128C128REFSXO" "$KERNAL64C128C128REFSYO" "$1"/.testbench/"$screenshottest"-kernal64C128C128.png "$KERNAL64C128C128SXO" "$KERNAL64C128C128SYO"
            exitcode=$?
        else
            echo -ne "reference screenshot missing - "
            exitcode=255
        fi
    fi
    echo "cmpscreen exited with: " $exitcode
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
function kernal64c128c128_run_exitcode
{
    if [ $verbose == "1" ]; then
        echo "RUN: "$KERNAL64C128C128 $KERNAL64C128C128OPTS $KERNAL64C128C128OPTSEXITCODE ${@:5} "--limitcycles" "$3" "$4"
    fi
    $KERNAL64C128C128 $KERNAL64C128C128OPTS $KERNAL64C128C128OPTSEXITCODE ${@:5} "--limitcycles" "$3" "$4" 1> /dev/null 2> /dev/null
    exitcode=$?
    #echo EXIT CODE for $2 is $exitcode
}
