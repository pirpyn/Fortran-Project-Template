#!/bin/bash
# Script bash de gestion simple des dépendances
usage () {
echo "usage: $0 <MOD_DIR>"
echo " Etant donné une repertoire MOD_DIR, ce script renvoie"
echo "l'ordre dans lequel compiler les modules Fortran .f90."
}

getmodname() {
	# renvoie les noms des modules tel que Fortran les connait
	grep -iE '^\ *module\ [^ ]*$' $MOD_DIR/*.f90 | 
	awk -F ':' '{print $2}' | 
	awk '{print $2}'
}

getmodfile() {
	# renvoie les noms des fichiers des modules
	grep -iE '^\ *module\ [^ ]*$' $MOD_DIR/*.f90 | 
	awk -F ':' '{print $1}'
}

get_modules_name_from_file() {
	# fonction qui prend en argument un fichier module
	# et recherche la liste des modules utilisé dedans
	# Dès qu'un module est trouvé, renvoie l'indice correspondant
	# à MOD_NAME
	if [[ $# -ne 1 ]];then
		echo "get_modules_name_from_file: argument <module> manquant"
	 	exit
	fi
	local module=$1
	grep -iE '^\ *use\ *[^ ]*$' $module | 
		awk '{print $2}' | 
		while read used_mod ; do
			i=0
			while [[ ${MOD_NAME[$i]^^} != ${used_mod^^}  ]];do
				i=$(( $i + 1 ))
			done
			echo $i
		done
}

get_modules_recursively() {
	# Fonction récursive qui prend un nombre quelconque d'argument
	# Pour chaque module parent, si n'utilise pas de modules
	# alors elle renvoie le nom du module,
	# sinon la fonction est appelé sur ce module fils
	used_mod=( $( for module in $@; do
		for i in $( get_modules_name_from_file $module ); do
			echo ${MOD_FILE[$i]}
		done 
	done) )
	if [[ ${#used_mod[@]} -gt 0 ]];then
		get_modules_recursively ${used_mod[@]}
	fi
	echo $@
}

delete_multiple_args() {
	# Fonction bash prenant un nombre quelconques d'arguments
	# et supprime ceux les apparations multiples d'un même argument
	local ARGS="$@"
	local cur
	while [[ ${#ARGS} -gt 0 ]]; do
		cur=$(echo $ARGS | awk '{print $1}')
		curn=${cur//\//\\\/}
		ARGS=$(echo $ARGS | sed "s/${curn}//g")
		echo -n "$cur "
	done
}

if [[ $# -ne 1 ]]; then
	usage
	exit 0
else
	MOD_DIR=${1/%\/}
fi

MOD_FILE=( $( getmodfile ) )
MOD_NAME=( $( getmodname ) )

delete_multiple_args $(get_modules_recursively ${MOD_FILE[@]})
echo