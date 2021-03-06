# Replicación del trabajo para el estimado de defunciones a causa del Huracán María (2017) en Puerto Rico. 
  #### Trabajo de Rafael Irrizary et. al. (2017): https://github.com/c2-d2/pr_mort_official.git
  Replicación creada por Kenny J. Davila Salgado, Stephanie M. Ramos Camacho, Erick Miguel Ivanovich Méndez, Alejandro Soledad Méndez, 
  para el cumplimiento del curso CCOM3031, evaluado por la profesora Patricia Ordoñéz. 
  
  Las intrucciones fueron replicar el trabajo de estos analistas, demógrafos y data scientists. Por tal razón, verán una integridad en la 
  estructura del código en comparación con el original. Simplemente, se hicieron ediciones individuales desde el principio para asegurar
  que es un trabajo replicable. 
  

Este proyecto de Rafael Irrizary trata sobre el efecto de los desastres que dejó el huracán María. En septiembre de 2017, el huracán María causó daños masivos en la infraestructura de Puerto Rico, pero su efecto sobre la mortalidad sigue siendo controvertido. Y de esto es lo que se va aestar viendo en este proyecto.

#### Cómo correr:

El código de las figuras y tablas debe ser corrido en una plataforma que apoye Jupyter Notebooks (Google Colab, Anaconda, etc.). Adicionalmente, se requiere descargar y subir todos los archivos que se encuentran en la carpeta de "Adjusted and Cleaned Data":

1. ACS2016.Rdata
2. Adjusted Data.R
3. adj_rates.RDS
4. base.R
5. cleaned_age_counts_acs_vs_survey.RDS
6. cleaned_hh_size_acs_vs_survey.RDS
7. deaths.RDS
8. deaths_official.RDS
9. hh_main.RDS
10. ind_hh.RDS
11. individuals.RDS
12. left_df.RDS
13. official_long.RDS
14. pop_est.RDS
15. where_df.RDS

Asegúrese que los archivos se encuentren en la raíz del "working directory". En el caso de Google Colab, será dentro de la carpeta "content" que viene siendo el directorio default del proyecto. 

Luego de subir los archivos necesarios, podrá correr el código oprimiendo el botón de "run" en cada celda del Google Colab. Para ver el código envuelto, basta con oprimir "Show Code". 

Files que se usaron para correr el código de las figuras S4, S5 y S6 y que se tiene que subir al colab antes de correrse:
-base.R
-hh_main.RDS

#### Links:

1. Google Colab: https://colab.research.google.com/drive/1SJfBa5q8zkAiuIQziDYTarTn9b3-7mko?usp=sharing
2. Google Doc: https://docs.google.com/document/d/15He3s44XqUiY8UoK2XoR5Jrcb0RAGaqK-fUy6pb5a4o/edit?usp=sharing
3. Video: https://drive.google.com/file/d/1ehv2dWsUgsIWD5sevPj7-vNRf1EFcW8t/view?usp=sharing
