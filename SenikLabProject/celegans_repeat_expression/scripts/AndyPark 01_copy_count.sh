# ==================================================================================================
# Copy files from Box into project directory
# - gene expression from RPOA-2 degron, rDNA deletion, and WT-N2 strains
# ==================================================================================================
# define paths
path_project_data="/Users/baghuijae/Desktop/Github/celegans_repeat_expression/data/counts"
path_repeat_counts_data="/Users/baghuijae/Library/CloudStorage/Box-Box/Sarinay_Cenik_lab_resources/lab_members/past_members/Trevor Freeman/repeats_expression/data/counts"

# copy files containing "rpoa-2-degron|rDNAdel|wt-N2"
cd "$path_repeat_counts_data"
cp $(ls | grep -E "rpoa-2-degron|rDNAdel|wt-N2") $path_project_data

# remove summarized counts file
cd $path_project_data
rm "20220619_rDNAdel_rpoa2degron_repeats_expression__fsp_ht2-ce10_sbl_sSR_srF-ce10_rmsk.txt"

