# use --shell for Windows only
#KERNAL64VIC20OPTS+=" --shell"
KERNAL64VIC20OPTS+=" --testcart"
KERNAL64VIC20OPTS+=" --headless"
KERNAL64VIC20OPTS+=" --warp"
KERNAL64VIC20OPTS+=" --vic-palette mike_pal"
KERNAL64VIC20OPTS+=" --screen-dim 3"
KERNAL64VIC20OPTS+=" --cpujam-continue true"
KERNAL64VIC20OPTS+=" --ignore-config-file"

# extra options for the different ways tests can be run
# FIXME: the emulators may crash when making screenshots when emu was started
#        with -console
KERNAL64VIC20OPTSEXITCODE+=""
KERNAL64VIC20OPTSSCREENSHOT+=""

# X and Y offsets for saved screenshots. when saving a screenshot in the
# computers reset/startup screen, the offset gives the top left pixel of the
# top left character on screen.
KERNAL64VIC20SXO=96
KERNAL64VIC20SYO=48

KERNAL64VIC20REFSXO=96
KERNAL64VIC20REFSYO=48

function kernal64vic20_check_environment
{
    KERNAL64VIC20=$EMUDIR/k20.sh

        if ! [ -x "$(command -v $JAVA_HOME/bin/java)" ]; then
            echo 'Error: java not installed.' >&2
            exit 1
        fi
}

# $1  option
# $2  test path
function kernal64vic20_get_options
{
#    echo xvic_get_options "$1" "$2"
    exitoptions=""
    case "$1" in
        "default")
                exitoptions=""
            ;;
        "vic-pal")
                exitoptions=""
                testprogvideotype="PAL"
            ;;
        "vic-ntsc")
                exitoptions="--ntsc true"
                testprogvideotype="NTSC"
            ;;
        "vic20-8k")
                exitoptions="--8k"
                memory_expansion_enabled="8K"
            ;;
        "vic20-32k")
                exitoptions="--32k"
                memory_expansion_enabled="32K"
            ;;
          "geo256k")
                exitoptions="--geo-ram 256"
                georam_enabled=1
            ;;
          "geo512k")
                exitoptions="--geo-ram 512"
                georam_enabled=1
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
            ;;
    esac
}

# $1  option
# $2  test path
function kernal64vic20_get_cmdline_options
{
#    echo xvic_get_cmdline_options "$1" "$2"
    exitoptions=""
#    case "$1" in
#        "PAL")
#                exitoptions="-pal"
#            ;;
#        "NTSC")
#                exitoptions="-ntsc"
#            ;;
#        "NTSCOLD")
#                exitoptions="-ntscold"
#            ;;
#        "8K")
#                exitoptions="-memory 8k"
#            ;;
#        "32K")
#                exitoptions="-memory all"
#            ;;
#    esac
}

# called once before any tests run
function kernal64vic20_prepare
{
    true
}

################################################################################
# reset
# run test program
# exit when write to $910f occurs - the value written determines success (=$00) or fail (=$ff)
# exit after $timeout cycles (exitcode=$01)
# save a screenshot at exit - success or failure is determined by comparing screenshots

# $1  test path
# $2  test program name
# $3  timeout cycles
# $4  test full path+name (may be empty)
# $5- extra options for the emulator
function kernal64vic20_run_screenshot
{
    if [ "$2" == "" ] ; then
        screenshottest="$mounted_crt"
    else
        screenshottest="$2"
    fi

    mkdir -p "$1"/".testbench"
    rm -f "$1"/.testbench/"$screenshottest"-kernal64vic20.png
    if [ $verbose == "1" ]; then
        echo $KERNAL64VIC20 $KERNAL64VIC20OPTS $KERNAL64VIC20OPTSSCREENSHOT ${@:5} "--limitcycles" "$3" "--screenshot" "$1"/.testbench/"$screenshottest"-kernal64vic20.png "$4"
        $KERNAL64VIC20 $KERNAL64VIC20OPTS $KERNAL64VIC20OPTSSCREENSHOT ${@:5} "--limitcycles" "$3" "--screenshot" "$1"/.testbench/"$screenshottest"-kernal64vic20.png "$4" 2> /dev/null | grep "cycles elapsed" | tr '\n' ' '
        exitcode=${PIPESTATUS[0]}
    else
        $KERNAL64VIC20 $KERNAL64VIC20OPTS $KERNAL64VIC20OPTSSCREENSHOT ${@:5} "--limitcycles" "$3" "--screenshot" "$1"/.testbench/"$screenshottest"-kernal64vic20.png "$4" 1> /dev/null 2> /dev/null
        exitcode=$?
    fi

    if [ $verbose == "1" ]; then
        echo $KERNAL64VIC20 "exited with: " $exitcode
    fi
    
    if [ $exitcode -ne 0 ]
    then
        if [ $exitcode -ne 1 ]
        then
            if [ $exitcode -ne 255 ]
            then
                echo -ne "\nerror: call to $KERNAL64VIC20 failed.\n"
                exit -1
            fi
        fi
    fi

 
    if [ $exitcode -eq 0 ] || [ $exitcode -eq 255 ]
    then
        if [ -f "$refscreenshotname" ]
        then
            # defaults for PAL
            KERNAL64VIC20SXO=96
            KERNAL64VIC20SYO=48
            KERNAL64VIC20REFSXO=96
            KERNAL64VIC20REFSYO=48
            
    #        echo [ "${refscreenshotvideotype}" "${videotype}" ]
        
            if [ "${refscreenshotvideotype}" == "NTSC" ]; then
                KERNAL64VIC20REFSXO=40
                KERNAL64VIC20REFSYO=22
            fi
        
            # when either the testbench was run with --ntsc, or the test is ntsc-specific,
            # then we need the offsets on the NTSC screenshot
            if [ "${videotype}" == "NTSC" ] || [ "${testprogvideotype}" == "NTSC" ]; then
                KERNAL64VIC20SXO=40
                KERNAL64VIC20SYO=22
            fi

    #        echo ./cmpscreens "$refscreenshotname" "$XVICREFSXO" "$XVICREFSYO" "$1"/.testbench/"$screenshottest"-xvic.png "$XVICSXO" "$XVICSYO"
            ./cmpscreens "$refscreenshotname" "$KERNAL64VIC20REFSXO" "$KERNAL64VIC20REFSYO" "$1"/.testbench/"$screenshottest"-kernal64vic20.png "$KERNAL64VIC20SXO" "$KERNAL64VIC20SYO"
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
# exit when write to $910f occurs - the value written determines success (=$00) or fail (=$ff)
# exit after $timeout cycles (exitcode=$01)

# $1  test path
# $2  test program name
# $3  timeout cycles
# $4  test full path+name (may be empty)
# $5- extra options for the emulator
function kernal64vic20_run_exitcode
{
    if [ $verbose == "1" ]; then
        echo $KERNAL64VIC20 $KERNAL64VIC20OPTS $KERNAL64VIC20OPTSEXITCODE ${@:5} "--limitcycles" "$3" "$4" "1> /dev/null 2> /dev/null"
        $KERNAL64VIC20 $KERNAL64VIC20OPTS $KERNAL64VIC20OPTSEXITCODE ${@:5} "--limitcycles" "$3" "$4" 2> /dev/null | grep "cycles elapsed" | tr '\n' ' '
        exitcode=${PIPESTATUS[0]}
    else
        $KERNAL64VIC20 $KERNAL64VIC20OPTS $KERNAL64VIC20OPTSEXITCODE ${@:5} "--limitcycles" "$3" "$4" 1> /dev/null 2> /dev/null
        exitcode=$?
    fi
#    echo "exited with: " $exitcode
}
