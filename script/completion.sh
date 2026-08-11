#!/usr/bin/env bash
#
# Bash completion for the "et" command (SYSTEM ET, native ECAD tool).
#
# Install by sourcing this file, e.g. in ~/.bashrc:
#   source /path/to/ET/script/completion.sh
#
# The switches, their "takes a value" status, and their valid enumerated
# values are taken from src/lib/et_commandline_switches.ads and the
# getopt/elsif chain in src/et/et.adb - keep this in sync if those change.

_et_switches_flag=(
	--version
	--help
	--create-package
	--create-symbol
	--create-device
)

_et_switches_value=(
	--make-conventions
	--log-level
	--import-project
	--import-format
	--create-project
	--open-project
	--save-project-as
	--module
	--sheet
	--package-appearance
	--open-package
	--save-package-as
	--symbol-appearance
	--open-symbol
	--save-symbol-as
	--device-appearance
	--open-device
	--save-device-as
	--create-schematic-frame
	--open-schematic-frame
	--save-schematic-frame-as
	--create-pcb-frame
	--open-pcb-frame
	--save-pcb-frame-as
	--script
	--runmode
)

_et_completion () {
	local cur prev opts
	COMPREPLY=()
	cur="${COMP_WORDS[COMP_CWORD]}"
	prev="${COMP_WORDS[COMP_CWORD-1]}"

	# Completing the value of a switch that takes one:
	case "${prev}" in
		--runmode)
			COMPREPLY=( $(compgen -W "headless symbol package device module rig" -- "${cur}") )
			return 0
			;;

		--import-format)
			COMPREPLY=( $(compgen -W "kicad_v4 kicad_v5" -- "${cur}") )
			return 0
			;;

		--symbol-appearance|--device-appearance)
			COMPREPLY=( $(compgen -W "virtual pcb" -- "${cur}") )
			return 0
			;;

		--package-appearance)
			# et_package_bom_relevance.type_bom_relevant literals:
			COMPREPLY=( $(compgen -W "yes no" -- "${cur}") )
			return 0
			;;

		--import-project|--create-project|--open-project|--save-project-as)
			# these take a project directory:
			COMPREPLY=( $(compgen -d -- "${cur}") )
			return 0
			;;

		--module)
			COMPREPLY=( $(compgen -f -X '!*.mod' -- "${cur}") )
			return 0
			;;

		--open-package|--save-package-as)
			COMPREPLY=( $(compgen -f -X '!*.pac' -- "${cur}") )
			return 0
			;;

		--open-symbol|--save-symbol-as)
			COMPREPLY=( $(compgen -f -X '!*.sym' -- "${cur}") )
			return 0
			;;

		--open-device|--save-device-as)
			COMPREPLY=( $(compgen -f -X '!*.dev' -- "${cur}") )
			return 0
			;;

		--create-schematic-frame|--open-schematic-frame|--save-schematic-frame-as)
			COMPREPLY=( $(compgen -f -X '!*.frs' -- "${cur}") )
			return 0
			;;

		--create-pcb-frame|--open-pcb-frame|--save-pcb-frame-as)
			COMPREPLY=( $(compgen -f -X '!*.frb' -- "${cur}") )
			return 0
			;;

		--script)
			COMPREPLY=( $(compgen -f -X '!*.scr' -- "${cur}") )
			return 0
			;;

		--make-conventions)
			COMPREPLY=( $(compgen -f -- "${cur}") )
			return 0
			;;

		--log-level|--sheet)
			# numeric arguments - nothing sensible to complete:
			return 0
			;;
	esac

	# Otherwise complete a switch name:
	opts="${_et_switches_flag[*]} ${_et_switches_value[*]}"
	COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
	return 0
}

complete -F _et_completion et
