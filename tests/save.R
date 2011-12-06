if(FALSE) {
a = 1:10
b = letters[1:4]

con = rawConnection(raw(0), "w")
save(a, b, file = con)
data = rawConnectionValue(con)
close(con)

k = rawConnection(data, "r")
load(k)


writeBin(data, "/tmp/g.rda")
}

