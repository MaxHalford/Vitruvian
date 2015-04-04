library('FactoMineR')

# Read the data
data = read.csv('perfectionSID.csv')

# Calculate ratios
data['hauteur/envergure'] = data['hauteur'] / data['envergure']
data['nombrilPied/nombrilMain'] = data['nombril.pied'] / data['nombril.main']
data['genouPied/epaules'] = data['genou.pied'] / data['epaules']
data['genouPied/hauteur'] = data['genou.pied'] / data['hauteur']
data['epaules/hauteur'] = data['epaules'] / data['hauteur']

# Subset the measures
mesures = subset(data, select = c(3:8))
rownames(mesures) = data[['nom']]

# Subset the ratios
ratios = subset(data, select = c(9:13))
rownames(ratios) = data[['nom']]
# Add a perfect individual
ratios = rbind(ratios, c(1, 1, 1, 0.25, 0.25))
# PCA on the ratios, project the perfect individual without influencing the PCA
acp = PCA(ratios, ind.sup = dim(ratios)[1])

# Classification
hcpc = HCPC(acp)

# Classement des élèves par rapport aux deux premiers axes
centre = acp$ind.sup$coord[c(1, 2)]
eleves = acp$ind$coord
scores = (eleves[,1] - rep(centre[1], 23) ** 2 + (eleves[,2] - rep(centre[2], 23)) ** 2) ** 1/2

# Test 1 : hauteur/envergure == 1
test1 = t.test(data[['hauteur/envergure']], mu = 1)
# Test 2 : nombrilPied/nombrilMain == 1
test2 = t.test(data[['nombrilPied/nombrilMain']], mu = 1)
# Test 3 : genouPied/epaules == 1
test3 = t.test(data[['genouPied/epaules']], mu = 1)
# Test 4 : genouPied/hauteur == 0.25
test4 = t.test(data[['genouPied/hauteur']], mu = 0.25)
# Test 5 : epaules/hauteur == 0.25
test5 = t.test(data[['epaules/hauteur']], mu = 0.25)

# On utilise la méthode FDR pour tester plusieurs hypothèses en même temps
pValues = c(test1$p.value, test2$p.value, test3$p.value, test4$p.value, test5$p.value)
p.adjust(pValues, 'FDR')

