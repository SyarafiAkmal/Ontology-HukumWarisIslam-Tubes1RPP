# Ontologi dan Sistem Berbasis Pengetahuan - Hukum Waris dalam Islam
> Tugas Besar 1 IF4070 Representasi Pengetahuan dan Penalaran

## How to Run

- Open SWI Prolog in src directory
- Consult parser.pl by running `[parser].` query
- Parse desired file by running `parse_family_tree('../input/file_name.json').` query
- Save it by running `save_family_tree('prolog_file_name.pl').` query (Recommended: `family-tree.pl`)
- Run `[main].` query to consult all other files
- Make a calculation by running `calculate_inheritance(deceased_name, heir_name, Share).` query. Modify deceased_name and heir_name as you want, but make sure the status of heir_name is true (Alive) and status of deceased_name is false (Dead)
- Voila!