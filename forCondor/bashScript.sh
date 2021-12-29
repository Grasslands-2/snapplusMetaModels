#!/bin/bash
source /opt/bifxapps/miniconda3/etc/profile.d/conda.sh
unset $PYTHONPATH
conda activate r_ml2
Rscript /home/glbrc.org/emchasen/cloverBeltSnapPlus/ffcn/ptLowOM/pt_ffcn.R