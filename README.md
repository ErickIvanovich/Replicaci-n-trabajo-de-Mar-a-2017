# Replicación del trabajo para el estimado de defunciones a causa del Huracán María (2017) en Puerto Rico. 
  #### Trabajo de Rafael Irrizary et. al. (2017): https://github.com/c2-d2/pr_mort_official.git
  Replicación creada por Kenny J. Davila Salgado, Stephanie M. Ramos Camacho, Erick Miguel Ivanovich Méndez, Alejandro Soledad Méndez, 
  para el cumplimiento del curso CCOM3031, evaluado por la profesora Patricia Ordoñéz. 
  
  Las intrucciones fueron replicar el trabajo de estos analistas, demógrafos y data scientists. Por tal razón, verán una integridad en la 
  estructura del código en comparación con el original. Simplemente, se hicieron ediciones individuales desde el principio para asegurar
  que es un trabajo replicable. 
  

Este proyecto de Rafael Irrizary trata sobre el efecto de los desastres que dejó el huracán María. En septiembre de 2017, el huracán María causó daños masivos en la infraestructura de Puerto Rico, pero su efecto sobre la mortalidad sigue siendo controvertido. Y de esto es lo que se va aestar viendo en este proyecto.

Cómo correr:

El código de las figuras y tablas debe ser corrido en una plataforma que apoye Jupyter Notebooks (Google Colab, Anaconda, etc.). Adicionalmente, se requiere descargar y subir todos los archivos que se encuentran la carpeta de "Adjusted and Cleaned Data":

-ACS2016.Rdata
-Adjusted Data.R
-adj_rates.RDS
-base.R
-cleaned_age_counts_acs_vs_survey.RDS
-cleaned_hh_size_acs_vs_survey.RDS
-deaths.RDS
-deaths_official.RDS
-hh_main.RDS
-ind_hh.RDS
-individuals.RDS
-left_df.RDS
-official_long.RDS
-pop_est.RDS
-where_df.RDS

Files que se usaron para correr el código de las figuras S4, S5 y S6 y que se tiene que subir al colab antes de correrse:
-base.R
-hh_main.RDS
