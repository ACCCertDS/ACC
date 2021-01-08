#' Caracteristiques
#'
#' @format Un data.frame avec 295 410 lignes et 16 variables
#'
#' + **Num_Acc** : Identifiant de l’accident
#'
#' + **jour** : Jour de l accident
#'
#' + **mois** : Mois de l accident
#'
#' + **an** : Annee de l accident
#'
#' + **hrmn** : Heure et minutes de l accident
#'
#' + **lum** : Conditions d eclairage
#'   + `Plein jour`
#'   + `Crépuscule ou aube`
#'   + `Nuit sans éclairage`
#'   + `Nuit avec éclairage public`
#'
#' + **dep** : Departement
#'
#' + **com** : Commune
#'
#' + **agg** : Localisation
#'   + `Hors agglomération`
#'   + `En agglomération`
#'
#' + **atm** : Conditions atmospheriques
#'   + `Normale`
#'   + `Pluie légère`
#'   + `Pluie forte`
#'   + `Neige - grêle`
#'   + `Brouillard - fumée`
#'   + `Vent fort - tempête`
#'   + `Temps éblouissant`
#'   + `Temps couvert`
#'   + `Autre`
#'
#' + **col** : Type de collision
#'   + `Non Renseigné`
#'   + `2 véhicules - frontale`
#'   + `2 véhicules - par l'arrière`
#'   + `2 véhicules - par le côté`
#'   + `3 véhicules et plus`
#'   + `Autre collision`
#'   + `Sans collision`
#'
#' + **adr** : Adresse postale (renseignée pour les accidents survenus en agglomeration)
#'
#' + **lat** : Latitude
#'
#' + **long** : Longitude
#'
#' + **gps**
#'
#' + **intersection** : Intersection
#'   + `Hors intersection`
#'   + `Intersection`
#'   + `Giratoire`
#'   + `Place`
#'   + `Passage à niveau`
#'
#'   ...
"caracteristiques"


#' Usagers
#'
#' @format Un data.frame avec avec 1 702 132 lignes et 10 variables
#'
#' + **Num_Acc** : Identifiant de l accident
#'
#' + **id_vehicule** : Identifiant unique du vehicule
#'
#' + **num_veh** : Identifiant du vehicule repris pour chacun des usagers occupant ce vehicule
#'
#' + **catu** : Categorie de l usager
#'   + `Conducteur`
#'   + `Passager`
#'   + `Piéton`
#'
#' + **grav** : Gravite de blessure de l usager
#'   + `Indemne`
#'   + `Tué`
#'   + `Blessé hospitalisé`
#'   + `Blessé léger`
#'
#' + **sexe** : Sexe de l usager
#'   + `Masculin`
#'   + `Féminin`
#'
#' + **an_nais** : Annee de naissance de l usager
#'
#' + **secu1** : Presence et l utilisation de l equipement de securite
#'
#' + **secu2**
#'
#' + **secu3**
#'   ...
"usagers"


#' Lieux
#'
#' @format Un data.frame avec avec xx lignes et xx variables
#'
#' + **gehe**
#'   ...
"lieux"


#' Vehicules
#'
#' Ajouter la description de la table vehicules
#'
#' @format Un data.frame avec avec 504 834 lignes et 8 variables
#'
#' + **Num_Acc** : Identifiant de l’accident
#'
#' + **id_vehicule** : Identifiant unique du vehicule
#'
#' + **num_veh** : Identifiant du vehicule repris pour chacun des usagers occupant ce vehicule
#'
#' + **catv** : Categorie du vehicule
#'   + `Bicyclette`
#'   + `2 roues motorisé`
#'   + `VL seul`
#'   + `VU seul`
#'   + `Poids lourd`
#'   + `Engin agricole`
#'   + `Bus`
#'   + `Train`
#'   + `Tramway`
#'   + `Quad`
#'   + `Autre`
#'
#' + **obs** : Obstacle fixe heurté
#'   + `Non`
#'   + `Oui`
#'   + `Non Renseigné`
#'
#' + **obsm** : Obstacle mobile heurté
#'   + `Aucun`
#'   + `Piéton`
#'   + `Véhicule`
#'   + `Animal`
#'   + `Autre`
#'
#' + **choc** : Point de choc initial
#'   + `Aucun`
#'   + `Avant`
#'   + `Arrière`
#'   + `Côté`
#'   + `Chocs multiples (tonneaux)`
#'   + `Non Renseigné`
#'
#'   ...
"vehicules"
