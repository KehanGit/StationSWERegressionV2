dir = '/projects/keya6539/P2_LRM_test/scripts/0functions/'

functionnamelist = list.files(dir,'.R$', full.names = T)

for(i in 1:length(functionnamelist)){
  source(functionnamelist[i])
}
