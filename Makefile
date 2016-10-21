figure-glmnet.png: figure-glmnet.R glmnet.list.RData
	R --no-save < $<
glmnet.list.RData: glmnet.list.R test.error.RData
	R --no-save < $<
figure-test-error.png: figure-test-error.R test.error.RData
	R --no-save < $<
test.error.RData: test.error.R input.features.RData output.diseases.RData models.RData fold.RData
	R --no-save < $<
fold.RData: fold.R hla.RData output.diseases.RData
	R --no-save < $<
models.RData: models.R
	R --no-save < $<
input.features.RData: input.features.R hla.RData output.diseases.RData markers.dosages.RData
	R --no-save < $<
output.diseases.RData: output.diseases.R hla.RData all.autoimmune.RData
	R --no-save < $<
hla.RData: hla.R
	R --no-save < $<
all.autoimmune.RData: all.autoimmune.R
	R --no-save < $<
markers.dosages.RData: markers.dosages.R
	R --no-save < $<
many.folds.RData: many.folds.R
	R --no-save < $<
many.seeds.RData: many.seeds.R
	R --no-save < $<
