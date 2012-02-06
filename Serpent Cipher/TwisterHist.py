import matplotlib.pyplot as plt

OUTPUTFILE = "out"
BINS = 100

List = map(lambda x: int(x.strip()), open(OUTPUTFILE).read().strip(", ").split(","))
plt.hist(List, BINS)
plt.show()

