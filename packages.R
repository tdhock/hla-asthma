works_with_R("3.2.3",
             "tdhock/ggplot2@a8b06ddb680acdcdbd927773b1011c562134e4d2",
             "tdhock/animint@2a462853491b985ee90e86ef7e017ecd51d2921f",
             "tdhock/directlabels@7b4b08a5dd0ab86e0b90902b3a233903ddd42311",
             data.table="1.9.7",
             "tdhock/namedCapture@05175927a45c301a18e8c6ebae67ea39a842d264",
             glmnet="1.9.5",
             "tdhock/WeightedROC@ef8f35ba7ae85e2995fa66afe13732cebb8b5633",
             doParallel="1.0.6")

registerDoParallel()
library(Matrix)
