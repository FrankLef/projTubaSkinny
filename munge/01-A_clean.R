
# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()


# select ------------------------------------------------------------------

tmp$clients <- raw_clients |>
  select(-clientgrp1_anon)

tmp$suppliers <- raw_fournisseurs |>
  select(-Langue_id, -NomLangue, - Abreviation, -Region_id, -NomDevise, 
         -SymboleDevise, -No_Compte_Achat, -Poster, -Date_Poster)

tmp$machines <- raw_machines

tmp$operations <- raw_operations

tmp$projects <- raw_projets |>
  filter(between(Soum_id, 20180001, 20199999)) |>
  select(-c("hrs_dist", "hrs_dist_pct", "dist_sgn", 
            "hrs_dist_sgn", "hrs_dist_pct_sgn"))


tmp$projects_ops <- raw_projets_ops |>
  filter(between(Soum_Id, 20180001, 20199999))

tmp$projects_soumis_ops <- raw_projets_soumis_ops |>
  filter(between(Soum_Id, 20180001, 20199999)) |>
  select(-c("DateInitiale_year", "date_creation"))


# rename ------------------------------------------------------------------

# rename specific to clients table
tmp$ren_clients <- c("client_name" = "nom")
tmp$clients <- tmp$clients |>
  eflTools::ren_many(tmp$ren_clients)


# rename specific to suppliers table
tmp$ren_suppliers <- c("supplier_name" = "nom")
tmp$suppliers <- tmp$suppliers |>
  eflTools::ren_many(tmp$ren_suppliers)

# rename specific to projects table
tmp$ren_clients <- c("client_name" = "nom")
tmp$projects <- tmp$projects |>
  eflTools::ren_many(tmp$ren_clients)

# rename specific to projects ops table
tmp$ren_clients <- c("client_name" = "nom")
tmp$projects_ops <- tmp$projects_ops |>
  eflTools::ren_many(tmp$ren_clients)

# rename specific to projects soumis ops table
tmp$ren_clients <- c("client_name" = "nom")
tmp$projects_soumis_ops <- tmp$projects_soumis_ops |>
  eflTools::ren_many(tmp$ren_clients)


# rename for all tables
tmp$ren_all <- c("client_id" = "Client_id",
                 "product_type" = "TypeProd", 
                 "client_activ" = "Client_Actif",
                 "client_grp_id" = "Groupe_Client_id", 
                 "client_grp_desc" = "Groupe_Client_desc",
                 "client_grp_activ" = "Groupe_Client_active",
                 "client_grp1" = "clientgrp1",
                 "client_grp1_id" = "clientgrp1_id", 
                 "client_grp1_desc" = "clientgrp1_desc",
                 "client_grp1_anon" = "clientgrp1_anon",
                 "supplier_no"  = "Fournisseur_NO",
                 "supplier_id"  = "Fournisseur_ID",
                 "supplier_activ"  = "Fournisseur_Actif",
                 "sector" = "Secteur",
                 "is_setup" = "UniteLot_id", 
                 "project_id" = "noProjet", 
                 "action_id" = "ACTION_ID",
                 "job_soum_id" = "DOC_JOB_SOUM_DET_ID",
                 "soum_no" = "Soum_id", 
                 "init_date_min" = "DateInitiale_min", 
                 "init_date_yr" = "DateInitiale_year", 
                 "init_date_mth" = "DateInitiale_mth",
                 "is_illegal" = "IsIllegal"
                     )
tmp$clients <- tmp$clients |>
  eflTools::ren_many(tmp$ren_all)
tmp$suppliers <- tmp$suppliers |>
  eflTools::ren_many(tmp$ren_all)
tmp$machines <- tmp$machines |>
  eflTools::ren_many(tmp$ren_all)
tmp$operations <- tmp$operations |>
  eflTools::ren_many(tmp$ren_all)
tmp$projects <- tmp$projects |>
  eflTools::ren_many(tmp$ren_all)
tmp$projects_ops <- tmp$projects_ops |>
  eflTools::ren_many(tmp$ren_all)
tmp$projects_soumis_ops <- tmp$projects_soumis_ops |>
  eflTools::ren_many(tmp$ren_all)


# datatypes ---------------------------------------------------------------

# convert to date
tmp$projects <- tmp$projects |>
  mutate(across(.cols = where(is.POSIXct), as.Date))
tmp$projects_ops <- tmp$projects_ops |>
  mutate(across(.cols = where(is.POSIXct), as.Date))
tmp$projects_soumis_ops <- tmp$projects_soumis_ops |>
  mutate(across(.cols = where(is.POSIXct), as.Date))


# set to NA ---------------------------------------------------------------

# IMPORTANT: Do not set key column to NA

tmp$clients <- tmp$clients |>
  # exclude the key column from the selection
  mutate(across(.cols = where(is.character) & !matches("client_id"), 
                .fns = function(x) na_if(x, "_NA")))

tmp$machines <- tmp$machines |>
  mutate(across(.cols = where(is.character), 
                .fns = function(x) na_if(x, "_NA")))

# assert ------------------------------------------------------------------

# IMPORTANT: verify that the key column exists

tmp$clients |>
  chain_start() %>%
  verify(nrow(.) >= 415) |>
  verify(has_all_names("client_id", "client_name")) |>
  assert(is_uniq, client_id) |>
  assert(in_set(0, 1), client_activ, client_grp_activ) |>
  assert(predicate = function(x) nzchar(x, keepNA = FALSE),
         client_name, client_grp_desc, client_grp1_id, client_grp1_desc) |>
  chain_end()

tmp$machines |>
  chain_start() %>%
  verify(nrow(.) >= 46) |>
  verify(has_all_names("mach_no", "mach_desc")) |>
  assert(is_uniq, mach_no) |>
  assert(predicate = function(x) nzchar(x, keepNA = FALSE), mach_desc) |>
  chain_end()

tmp$operations |>
  chain_start() %>%
  verify(nrow(.) >= 225) |>
  verify(has_all_names("oper_no", "oper_desc")) |>
  assert(is_uniq, oper_no) |>
  assert(predicate = function(x) nzchar(x, keepNA = FALSE), oper_desc) |>
  chain_end()

tmp$projects |>
  chain_start() %>%
  verify(nrow(.) >= 1947) |>
  verify(has_all_names("project_id")) |>
  assert(is_uniq, project_id) |>
  chain_end()

tmp$projects_ops |>
  chain_start() %>%
  verify(nrow(.) >= 176093) |>
  verify(has_all_names("action_id")) |>
  assert(is_uniq, action_id) |>
  chain_end()

tmp$projects_soumis_ops |>
  chain_start() %>%
  verify(nrow(.) >= 162066) |>
  verify(has_all_names("soum_ops_id", "project_id")) |>
  assert(is_uniq, soum_ops_id) |>
  chain_end()

# teardown ----------------------------------------------------------------

clients <- eflTools::bobj(tmp$clients, formula = client_id ~ client_grp1,
                          id = "client_id")
suppliers <- eflTools::bobj(tmp$suppliers, formula = supplier_no ~ supplier_name,
                          id = "supplier_no")
machines <- eflTools::bobj(tmp$machines, formula = mach_no ~ mach_desc,
                           id = "mach_no")
operations <- eflTools::bobj(tmp$operations, formula = oper_no ~ oper_desc,
                            id = "oper_no")

projects <- eflTools::bobj(tmp$projects, 
                           formula = vente_prix_tot ~ client_grp1_id +
                             vente_qte + vente_achat + vente_fgf + vente_mod + 
                             jobs_work_hrs + init_date_min,
                             id = "project_id")

projects_ops <- eflTools::bobj(tmp$projects_ops, 
                             formula = mach_no + mach_desc ~ jobs_qte + MOD + FGF,
                             id = "action_id")


projects_soumis_ops <- eflTools::bobj(tmp$projects_soumis_ops, 
                               formula = mach_no + mach_desc ~ QTE + MOD + FGF,
                               id = "soum_ops_id")

xprts_obj <- eflTools::xprts()

suppressWarnings(rm(tmp))
