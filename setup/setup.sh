#!/bin/bash
version="v0.0.1"
script_name=`basename "$0"`
# This script reads Bash file with a list of applications 
# and either
# - installs the applications
# - checks that the applications are installed
# - prints the versions of the applications
#
# The Bash config file must contain variable declarations with the setup
# steps to be performed.
#
# The Bash config file is sourced into this script, so it can contain any 
# valid Bash syntax. The only exception is that any variable named "step_<number>_<name>"
# will be rejected if <name> is not one of the variable names described below. This is to 
# detect any mistyped variable names that would otherwise go undetected.
#
# Example of a list of config steps. 
# step_1000_name="protoc"
# step_1000_cmd="./install_protoc.sh"
# step_1000_ver="protoc --version | cut -d ' ' -f2"
# step_1000_bin="protoc"
#
# step_2000_name="gcloud SDK"
# step_2000_cmd="gcloud install"
# etc. 
#
# The parts of the variable declarations:
#
# step_<step number>_<action>=<value>
#
# Setup steps are performed in sequence, first step_1000, then step_2000, etc. 
# 
#
# Actions:
# 
#   name: Required
#         A user-friendly name. Used to print informational messages.
#         
#   cmd:  Required
#         Command to perform the setup. Will be eval'ed by this script. 
#         The command can be any valid Bash command.
#
#         Examples:
#
#           A script:
#           cmd="./install_protoc.sh"   
#
#           A statement:
#           cmd="go install google.golang.org/protobuf/cmd/protoc-gen-go@v1.26"
#
#           A heredoc:
#           cmd=$(cat<<-EOF
#             sudo apt-get install -y zip
#             PB_REL="https://github.com/protocolbuffers/protobuf/releases"
#             sudo unzip -o protoc-\$PROTOC_VERSION-linux-x86_64.zip -d /usr/local
#             rm -f protoc-\$PROTOC_VERSION-linux-x86_64.zip
#             sudo chmod -R 775 /usr/local/bin
#             sudo chmod -R 777 /usr/local/include 
#           EOF
#           )
#
#          Special note about heredoc:
#          The heredoc is eval'ed by the setup script. 
#          Variables must be escaped with \$varname.
#
#
#   bin:  Optional
#         The name of the binary executable
#         If bin is given, we will verify that the binary is present
#         and executable after the installation, or error. 

#   ver:  Optional
#         Command to produce a string with the version number.
#         If not set, no version will be printed for the app. 
#         Command is expected to produce a short string, preferable
#         just the variable number, suitable for printing to the screen. 
#
#   mode: Optional
#         When this is blank, stdout and stderr from cmd will be redirected to a 
#         log file. 
#
#         'interactive'
#         In interactive mode, no output redirection will occur; this is to allow
#         read commands and other commands that require the user to see the output.



# -------------------------------------------------- 
# COMMAND LINE OPTIONS AND USAGE
# --------------------------------------------------

usage() {
    echo "$script_name $version"
    echo ""
    echo "Usage:"
    echo "  $script_name.sh [-c <config_file>] [-ifph] [-o <number>]"
    echo "   -c config_file"
    echo "      Perform steps in config_file. Defaults to 'default.cfg'"
    echo "      See documentation in $script_name for the format of a .cfg file"
    echo "   -n Do NOT Perform installs, just print version information"
    echo "   -f Force reinstalling. By default, binaries that already exist"
    echo "      will be skipped."
    echo "   -i Ignore errors and continue processing"
    echo "      By default processing stops at the first error"
    echo "   -p Print versions of installed apps (default: true)"
    echo "   -v Verbose output (for debugging)"
    echo "   -o number"
    echo "      Only run the install step with number."
    echo "      Combine with -f to force reinstalling one package."
    echo "   -h Print this help message"
}


# -------------------------------------------------- 
# DEFAULTS
# --------------------------------------------------

# Default command line parameters; see usage()
config_file="default.cfg"
verbose="false"
do_install="true"
do_version_print="true"
only_number=""
do_not_ignore_errors="true"
force_reinstalling=""

# The log file where errors and stdout will be redirected
logfile="$HOME/$script_name.log"

# Some installs write config data to the user's profile file
# The install scripts should write to $PROFILE_FILE
export PROFILE_FILE="$HOME/.bashrc"

while getopts "c:hfno:vp" opt; do
    case $opt in
        c ) config_file=$OPTARG;;
        n ) do_install="false";;
	p ) do_version_print="true";;
	v ) verbose="true";;
	f ) force_reinstalling="true";;
	i ) do_not_ignore_errors="";;
	o ) only_number=$OPTARG;;
        h ) usage
            exit 0;;
        *) usage
           exit 1;;
    esac
done


# --------------------------------------------------
# INTERNAL DATA STRUCTURE
# --------------------------------------------------
#
# An array to hold a list of the steps to perform 
# With an input like this...
#    step_001_name="protoc"
#    step_001_cmd="./install_protoc.sh"
#    step_001_ver="protoc --version | cut -d ' ' -f2" 
#    step_202_name="go"
#    step_300_name="tsc"
#
# ... the array will look like this:
#    steps[0] = "001"
#    steps[1] = "202"
#    steps[2] = "300"
declare -a steps


# --------------------------------------------------
# UTILITY FUNCTIONS
# --------------------------------------------------

# Verbose will print the argument if the verbose 
# flag is set to true
verbose() {
    if [[ $verbose == "true" ]]; then 
	echo "$@"
    fi
}


# exec_exists checks if the file passed as $1
# exists and is executable
exec_exists() {

    command -v "$1" > /dev/null 2>&1
    if [[ $? -ne 0 ]]; then 
	return 1 # binary does not exist
    fi
    #return 0 # binary exists
}


# lookup returns the value for a step's action.
# For example, given the variable declaration 
#
#   step_001_ver="protoc --version | cut -d ' ' -f2" 
#
# in the config file, this function call:
#
#   lookup '001' 'ver'
#
# will return "protoc --version | cut -d ' ' -f2"
#
# Use like this:
#  myvar="$(lookup 'cmd' '100')"
# 
lookup(){
    local tmp="step_$2_$1"
    echo "${!tmp}"
}


# --------------------------------------------------
# LOAD CONFIG FILE INTO INTERNAL DATA STRUCTURE
# --------------------------------------------------

verbose "Reading config from $config_file"
verbose "Logging output to $logfile"


source "$config_file"

if [[ $? -ne 0 ]]; then 
    echo "Could not parse default config file $config_file"
    echo "Specify config file with the -c parameter."
    echo "$script_name -h for help"
    exit 1
fi


# Loop through the variables the user declared in the config file
# (or, in reality, any variable in the environment that looks like step_something).
# Collect all the step numbers (see the declaration above), and 
# check that the user did not supply an invalid action name (cmd, ver, mode, etc.)
for var in "${!step_@}"; do

    # The input 
    #
    #   step_001_ver="protoc --version | cut -d ' ' -f2"     
    #
    # Will be parsed as follows:
    #
    #   var     = "step_001_ver"
    #   ${!var} = "protoc --version | cut -d ' ' -f2"
    #   step    = "001"
    #   action  = "ver"
    
    step=$(echo "$var" | cut -d _ -f 2)
    action=$(echo "$var" | cut -d _ -f 3)

    verbose "Read from $config_file, var=$var, step=$step: action $action=${!var}"

    # We're only collecting 1 entry per step, so we're using the
    # name action to do that, since it is required for all steps
    if [[ "$action" == "name" ]]; then 
	steps+=("$step")
    fi 

    # This is just an error checking step to catch 
    # any misnamed variables supplied by the user
    case $action in 
	"cmd")
	;;
	"bin")
	;;
	"ver")
	;;
	"name")
	;;
	"mode")
	;;
	*)
	    echo "Unknown action $key in config file"
	    exit 1
	    ;;
    esac
done



# --------------------------------------------------
# PERFORM INSTALLS
# --------------------------------------------------
if [[ "$do_install" == "true" ]]; then 
    
    # Sort the keys of the array of commands in 
    # numeric order, 1 ... n
    # This is to ensure that the steps are performed in the 
    # order the user specified in the config file. 
    STEPS=$(
	for KEY in ${steps[@]}; do
	    echo "$KEY"
	done | sort -n
	 )


    # Loop through the configuration steps
    for step in $STEPS; #${!cmd[@]};
    do
	# Grab the config values so we have them handy
	CMD="$(lookup 'cmd'  $step)"
	NME="$(lookup 'name' $step)"
	MDE="$(lookup 'mode' $step)"
	BIN="$(lookup 'bin'  $step)"
	
	verbose "$step: name=$NME, mode=$MDE, bin=$BIN, cmd=$CMD"
	
	# If the name is not set, default it 
	if [[ $NME == "" ]]; then
	    NME=$step	    
	    verbose "No name for $step, defaulting to $step"
	fi

	# If the user has specified to only run the step with the a specific number,
	# first check that the number is populated in the variable...
	if [[ $only_number ]]; then
	    # .. then check if the number we want to process is different than the number
	    # we're currently processing. If it's not the one we're looking for, skip.
            if [[ $only_number != $step ]]; then 
                continue 
            fi
	fi
	
	# If a binary was specified, 
	# check if it exits; if so skip re-installing,
	# unless the user passed the -f flag to force reinstalling.
	if [[ "$BIN" != "" && "$force_reinstalling" == "" ]]; then 
	    exec_exists "$BIN"
	    if [[ $? -eq 0 ]]; then 
		continue
            fi
	fi

	source "$PROFILE_FILE"

	echo -n "Configuring $NME..."
	verbose "Command = $CMD"

#	# If there's no install command, error
#	if [[ $CMD == "" ]]; then 
#	    echo "ERROR: no install command for $NME"
#	    if [[ $do_not_ignore_errors ]]; then 
#                exit 1
#	    fi
#	fi
#
	# Perform the installation
	if [[ $MDE == "interactive" ]]; then 
	    eval "$CMD"
	else
            eval "$CMD" > "$logfile" 2>&1
	fi


	if [[ $? -ne 0 ]]; then
	    echo " ERROR"
	    echo "Check $logfile for errors"
	    if [[ $do_not_ignore_errors ]]; then 
		exit 1
	    fi	    
	fi

	source "$PROFILE_FILE" 
	verbose "Before ver check: GOPATH=$GOPATH, PATH=$PATH"

	# If a binary was specified, check that it now exists
	if [[ $BIN != "" ]]; then 
	    exec_exists "$BIN"
	    if [[ $? -ne 0 ]]; then 
		echo " ERROR - executable $BIN does not exist after installation"
		if [[ $do_not_ignore_errors ]]; then 
		    exit 1
		fi	    
	    fi
	fi
	
	echo "OK"
    done
    echo ""
fi


# --------------------------------------------------
# PRINT VERSION NUMBERS
# --------------------------------------------------
if [[ "$do_version_print" == "true" ]]; then 

    # Loop through all of the version check commands we need to 
    # check
    for step in ${steps[@]};
    do

	# Grab the config values so we have them handy
	VER="$(lookup 'ver'  $step)"
	NME="$(lookup 'name' $step)"

	# If the name is not set, default it 
	if [[ $NME == "" ]]; then
	    NME=$step	    
	fi

	# If there's no version command, skip it
	if [[ $VER == "" ]]; then 
	    verbose "No version command for $NME, skipping"
	    continue
	fi

	verbose "Getting version of $NME with command $VER"

	# Do the version check
    installed_version=$(eval $VER 2>&1)
    # We can't use the exist code of eval to check for errors, since it will always be 0
    # Instead we check if the setup.sh string is present, because an error will include the
    # name of the script.
	if [[ "$installed_version" == *"setup.sh"* ]]; then
	    echo "$NME: ERROR getting version number with command $VER"
	fi
	printf "%-20s %s\n" "$NME" "$installed_version"
    done
fi





