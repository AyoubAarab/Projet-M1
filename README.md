# SIMULATION DE COVARIABLES DÉPENDANTES DU TEMPS
## Via une distribution de Weibull

Ce projet consistait à l’analyse de la survie avec des jeux de données simulées comportant des covariables dépendantes du temps. 
À partir du modèle semi-paramétrique de survie de Cox standard, nous avons introduis une approche permettant l’analyse de la survie en s’affranchissant des 
hypothèses classiques du modèle de Cox via l'introduction des effets aléatoires. Nous avons ensuite générer des données longitudinales (covariables dépendantes du temps)
via un modèle linéaire mixte.

Le modèle linéaire à effets mixtes est une extension du modèle linéaire qui prend en compte la variabilité liée aux individus. 
Ce modèle est composé d’une partie fixe et d’une partie aléatoire. La partie fixe est identique pourchaque individu et représente l’effet groupe. 
La partie aléatoire est propre à chacun des individus et traduit la variabilité liée à chaque sujet.

Enfin, nous avons générer des durées de survie suivant une distribution de Weibull avec les covariables dépendantes du temps par la fonction W de Lambert.
