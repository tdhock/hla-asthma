load("hla.RData")

output.diseases <- data.frame(
  asthma=hla$clinical$status,
  row.names=hla$clinical$ID)

save(output.diseases, file="output.diseases.RData")
