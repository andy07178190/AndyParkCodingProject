import pyBigWig

bw_path = "/Volumes/HDD/AndyPark/repeats_expression/20220619_rDNAdel_rpoa2degron/align/bw/N2_mean.bw"

bw = pyBigWig.open(bw_path)
print("Chromosome information:")
for chrom, length in bw.chroms().items():
    print(f"{chrom}: {length}")

# For example, get mean signal for chromosome I from 0 to 1,000,000:
mean_signal = bw.stats("chrI", 0, 1000000, type="mean")[0]
print("\nMean signal on chrI (0-1,000,000):", mean_signal)

bw.close()
