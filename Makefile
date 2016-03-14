figure-test-error.png: figure-test-error.R test.error.RData
	R --no-save < $<
test.error.RData: test.error.R trivial.RData glmnet.list.RData 
	R --no-save < $<
trivial.RData: trivial.R fold.RData 
	R --no-save < $<
glmnet.list.RData: glmnet.list.R fold.RData
	R --no-save < $<
fold.RData: fold.R hla.RData
	R --no-save < $<
hla.RData: hla.R
	R --no-save < $<
