#!/bin/bash
#
# SLURM HEADER
#############################################
# JOB INFO
#SBATCH --job-name=grids
#SBATCH --time=00:50:00
#SBATCH -c 20
#SBATCH --mem=10G
#SBATCH --output=/home/csic/bcf/cjt/CODES/tomogrids/test/jobLog_%J.out
#SBATCH --error=/home/csic/bcf/cjt/CODES/tomogrids/test/jobLog_%J.err
##############################################

##################################################
# Dentro de esta seccion, cambiar lo necesario.
# Lo que no se usa simplemente se comenta.
##################################################

#Models
vel="-v vel.in"
#refl="-r refl.in"
#dws="-d dws.in"

#Grids
dx="-dx 0.2"
dz="-dz 0.01"
dt="-dt 0.004"

#Resolucion de cada parametro (num de decimales)
resx="-rx 1 "	#resolution x, por defecto es 5
resz="-rz 2 "	#resolution z, por defecto es 3
#rest="-rt 3 "	#resolution time, por defecto es 3
#resv="-rv 3 "	#resolucion velocity, por defecto es 3
#resd="-rd 2 "	#resolucion dws, por defecto es 2

##################################################
##################################################
##################################################

##################################################
#NO TOCAR a partir de aqui
##################################################

nb_threads=$SLURM_CPUS_PER_TASK #hiper threading

FLAGS_compulsory=" $vel $dx $dz $dt"
FLAGS_resolution=" $resx $resz $rest $resv $resd "
FLAGS_optional=" $refl $dws "

# Con velocidad, dws y reflector
FLAGS="$FLAGS_compulsory $FLAGS_optional $FLAGS_resolution"

echo "FLAGS: $FLAGS"

time srun tomogrids $FLAGS --nb-threads $nb_threads --placement > info.out

##################################################
