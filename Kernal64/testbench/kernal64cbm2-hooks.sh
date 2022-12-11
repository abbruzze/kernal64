# use --shell for Windows only
#KERNAL64CBM2OPTS+=" --shell"
KERNAL64CBM2OPTS+=" --testcart"
KERNAL64CBM2OPTS+=" --headless"
KERNAL64CBM2OPTS+=" --warp"
KERNAL64CBM2OPTS+=" --cpujam-continue true"
KERNAL64CBM2OPTS+=" --ignore-config-file"
KERNAL64CBM2OPTS+=" --model 610pal"

# extra options for the different ways tests can be run
# FIXME: the emulators may crash when making screenshots when emu was started
#        with -console
KERNAL64CBM2OPTSEXITCODE+=""
KERNAL64CBM2OPTSSCREENSHOT+=""


# X and Y offsets for saved screenshots. when saving a screenshot in the
# computers reset/startup screen, the offset gives the top left pixel of the
# top left character on screen.
KERNAL64CBM2SXO=32
KERNAL64CBM2SYO=35

KERNAL64CBM2REFSXO=32
KERNAL64CBM2REFSYO=35

function kernal64cbm2_check_environment
{
    KERNAL64CBM2=$EMUDIR/kcbm2.sh

        if ! [ -x "$(command -v $JAVA_HOME/bin/java)" ]; then
            echo 'Error: java not installed. JAVA_HOME='$JAVA_HOME >&2
            exit 1
        fi
}

# $1  option
# $2  test path
function kernal64cbm2_get_options
{
#    echo KERNAL64CBM2_get_options "$1" "$2"
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
                exitoptions="--model 610ntsc"
                testprogvideotype="NTSC"
            ;;
        *)
                exitoptions=""
            ;;
    esac
}


# $1  option
# $2  test path
function kernal64cbm2_get_cmdline_options
{
#    echo KERNAL64CBM2_get_cmdline_options "$1" "$2"
    exitoptions=""
    case "$1" in
        "PAL")
                exitoptions=""
            ;;
        "NTSC")
                exitoptions="--model 610ntsc"
            ;;
    esac
}

# called once before any tests run
function kernal64cbm2_prepare
{
    true
}

################################################################################
# reset
# run test program
# exit when write to $daff occurs - the value written determines success (=$00) or fail (=$ff)
# exit after $timeout cycles (exitcode=$01)
# save a screenshot at exit - success or failure is determined by comparing screenshots

# $1  test path
# $2  test program name
# $3  timeout cycles
# $4  test full path+name (may be empty)
# $5- extra options for the emulator
function kernal64cbm2_run_screenshot
{
    if [ "$2" == "" ] ; then
        screenshottest="$mounted_crt"
    else
        screenshottest="$2"
    fi

    mkdir -p "$1"/".testbench"
    rm -f "$1"/.testbench/"$screenshottest"-KERNAL64CBM2.png
    if [ $verbose == "1" ]; then
        echo $KERNAL64CBM2 $KERNAL64CBM2OPTS $KERNAL64CBM2OPTSSCREENSHOT ${@:5} "--limitcycles" "$3" "--screenshot" "$1"/.testbench/"$screenshottest"-KERNAL64CBM2.png "$4"
    fi
    $KERNAL64CBM2 $KERNAL64CBM2OPTS $KERNAL64CBM2OPTSSCREENSHOT ${@:5} "--limitcycles" "$3" "--screenshot" "$1"/.testbench/"$screenshottest"-KERNAL64CBM2.png "$4" 1> /dev/null 2> /dev/null
    exitcode=$?
    
    if [ $verbose == "1" ]; then
        echo $KERNAL64CBM2 "exited with: " $exitcode
    fi
    
    if [ $exitcode -ne 0 ]
    then
        if [ $exitcode -ne 1 ]
        then
            if [ $exitcode -ne 255 ]
            then
                echo -ne "\nerror: call to $KERNAL64CBM2 failed.\n"
                exit -1
            fi
        fi
    fi

    if [ $exitcode -eq 0 ] || [ $exitcode -eq 255 ]
    then
        if [ -f "$refscreenshotname" ]
        then
        
            # defaults for PAL
            KERNAL64CBM2REFSXO=32
            KERNAL64CBM2REFSYO=35
            KERNAL64CBM2SXO=32
            KERNAL64CBM2SYO=35
            
    #        echo [ "${refscreenshotvideotype}" "${videotype}" ]
        
            if [ "${refscreenshotvideotype}" == "NTSC" ]; then
                KERNAL64CBM2REFSXO=32
                KERNAL64CBM2REFSYO=23
            fi
        
            # when either the testbench was run with --ntsc, or the test is ntsc-specific,
            # then we need the offsets on the NTSC screenshot
            if [ "${videotype}" == "NTSC" ] || [ "${testprogvideotype}" == "NTSC" ]; then
                KERNAL64CBM2SXO=32
                KERNAL64CBM2SYO=23
            fi
        
            ./cmpscreens "$refscreenshotname" "$KERNAL64CBM2REFSXO" "$KERNAL64CBM2REFSYO" "$1"/.testbench/"$screenshottest"-KERNAL64CBM2.png "$KERNAL64CBM2SXO" "$KERNAL64CBM2SYO"
            exitcode=$?
        else
            echo -ne "reference screenshot missing - "
            exitcode=255
        fi
    fi
#    echo "exited with: " $exitcode
}

################################################################################
# reset
# run test program
# exit when write to $daff occurs - the value written determines success (=$00) or fail (=$ff)
# exit after $timeout cycles (exitcode=$01)

# $1  test path
# $2  test program name
# $3  timeout cycles
# $4  test full path+name (may be empty)
# $5- extra options for the emulator
function kernal64cbm2_run_exitcode
{
    if [ $verbose == "1" ]; then
        echo $KERNAL64CBM2 $KERNAL64CBM2OPTS $KERNAL64CBM2OPTSEXITCODE ${@:5} "--limitcycles" "$3" "$4"
    fi
    #$KERNAL64CBM2 $KERNAL64CBM2OPTS $KERNAL64CBM2OPTSEXITCODE ${@:5} "--limitcycles" "$3" "$4" 1> /dev/null 2> /dev/null
	$KERNAL64CBM2 $KERNAL64CBM2OPTS $KERNAL64CBM2OPTSEXITCODE ${@:5} "--limitcycles" "$3" "$4"
    exitcode=$?
    echo "exited with: " $exitcode
}
